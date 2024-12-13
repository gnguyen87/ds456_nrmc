library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(tigris)
library(bslib)

options(rsconnect.max.bundle.size = 10e10)

## Redlining and Racial Covenants
redlining <- st_read('data/redlining_msp_shp') %>% 
  rename(grade = EQINTER20)

racial_cov <- st_read('data/racial_cov_msp_shp') 


## Environmental Data
variables <- c("Tree Canopy (%)", "Annual PM2.5 (tons)", "Mean LST (Fº)")

final_spatial <- st_read('data/final_spatial_shp')  %>% 
  rename(svi_index = svi_ndx,
         `Tree Canopy (%)` = `Tr_Cnpy`,
         `Annual PM2.5 (tons)` = `A_PM2_5`,
         `Mean LST (Fº)`= `men_lst`) 

msp_lake <- st_read('data/msp_lake_shp')

svi <- read.csv("data/svi.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = bs_theme(
    bg = "#06402B", fg = "white", primary = "#FCC780",
    base_font = font_google("Poppins"),
    code_font = font_google("Poppins")
  ),
  # Add custom CSS for navbar height and title alignment
  tags$head(tags$style(HTML("
    .navbar-brand {
      font-size: 20px; /* Adjust title size if needed */
    }
    #svi_plot {
      margin-top: 15px; /* Adjust this value to move the plot down */
       padding-top: 20px;
    }
  "))),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", label = "Choose an environmental variable", choices = variables),
      htmlOutput("text")
    ),
    mainPanel(
      fluidRow(
        column(leafletOutput("cov_redlining_map"), width = 6),
        column(leafletOutput("today_map"), width = 6)
      ),
      fluidRow(
        plotlyOutput("svi_plot"), width = 10
    )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$text <- renderUI({ 
    HTML(paste(
      "Variables: ",
      "  - Racial Covenants: Properties that prohibited people of color from owning/leasing in 1920s",
      "  - HOLC Grade: Census tract's Home Owner Loan Corporation neighborhood grades (1 = “Best”; 2 = “Still Desirable”; 3 = “Declining”; and 4 = “Hazardous” designations, respectively)",
      "  - Annual PM2.5 emissions: Average aggregated emissions from all air permitted facilities located within each census tract (2016-2020)",
      "  - Tree canopy percentage: Percentage of pixels in a city or neighborhood that are categorized as `tree` (2020)",
      "  - Social Vulnerability: Each variable represents the average percentage of the census tract's population in that category (2016-2020)",
      "<i>If there's an error raised when hovering on the maps, try again by hovering on non-water areas.<i>",
      sep = "<br/><br/>"
    ) 
    )
  })
  
  output$cov_redlining_map <- renderLeaflet({
    
    cov_redlining_map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = redlining,
                  fillColor = ~colorFactor(palette = "viridis", redlining$grade)(grade),
                  fillOpacity = 0.7,
                  color = "black", # Polygon border color
                  weight = 1, # Border thickness
                  label = ~paste("HOLC Grade:", redlining$grade),
      ) %>%
      addCircleMarkers(data = racial_cov,
                       radius = .1,
                       color = "red",
                       fillOpacity = 0.01,
                       label = racial_cov$Address
      ) %>% 
      addPolygons(data = msp_lake,
                  color = "lightblue",  # Water outline color
                  weight = 1,      # Outline thickness
                  fillColor = "lightblue",  # Water fill color
                  fillOpacity = 1 
      ) %>%
      addLegend(
        data = redlining,
        position = "bottomright",
        pal = colorFactor(palette = "viridis", domain = redlining$grade),
        values = ~redlining$grade,
        title = "HOLC Grade"
      ) %>% 
      addLegend(
        data = racial_cov,
        position = "bottomright",
        color = "red",
        label = "Racial Covenants"
      )
    
    cov_redlining_map <-  htmlwidgets::onRender(cov_redlining_map, "
  function(el, x) {
    // Function to highlight feature on mouseover
    function highlightFeature(e) {
      var layer = e.target;
      layer.setStyle({
        weight: 5, // Increase border width
        color: '#0000FF' // Highlight border color
      });
    }

    // Function to reset highlight on mouseout
    function resetHighlight(e) {
      var layer = e.target;

      // Handle CircleMarker layers
      if (layer instanceof L.CircleMarker) {
        layer.setStyle({
          color: layer.options.originalColor, // Reset to original stroke color
          weight: 1.5 // Reset border weight
        });
      } else {
        // Reset other layers
        layer.setStyle({
          weight: 1.5, // Reset border weight
          color: 'black' // Reset border color
        });
      }
    }

    // Iterate over all layers and add event listeners
    this.eachLayer(function(layer) {
      // Store original styles for CircleMarker layers
      if (layer instanceof L.CircleMarker) {
        layer.options.originalColor = layer.options.color || 'black'; // Store original color
      }

      // Add hover event listeners
      layer.on({
        mouseover: highlightFeature,
        mouseout: resetHighlight
      });
    });
  }
")
    
    
    
    cov_redlining_map
  })
  
  
  output$today_map <- renderLeaflet({
    
    var <- input$var
#ChatGPT cite: change the logic to if I pick "Tree Canopy (%)", "Annual PM2.5 (tons)", "Mean LST (Fº)" variables, I am able to change the density gradient color.
    palette_function <- if (var == "Tree Canopy (%)") {
      colorNumeric(palette = "Greens", domain = final_spatial[[var]])
    } else if (var == "Mean LST (Fº)") {
      colorNumeric(palette = "YlOrRd", domain = final_spatial[[var]])
    } else if (var == "Annual PM2.5 (tons)") {
      colorNumeric(palette = "plasma", domain = final_spatial[[var]])  
    } else {
      colorNumeric(palette = "viridis", domain = final_spatial[[var]])  
    }
    
    today_map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = final_spatial,
                  fillColor = ~palette_function(final_spatial[[var]]),
                  fillOpacity = 0.7,
                  color = "black", # Polygon border color
                  weight = 1, # Border thickness)
                  layerId = final_spatial$GEOID
      ) %>%
      addPolygons(data = msp_lake,
                  color = "lightblue",  # Water outline color
                  weight = 1,      # Outline thickness
                  fillColor = "lightblue",  # Water fill color
                  fillOpacity = 1 
      ) %>%
      addLegend(
        data = final_spatial,
        position = "bottomright",
        pal = palette_function,
        values = final_spatial[[var]],
        title = var,
      ) 

    
    
    today_map <- htmlwidgets::onRender(today_map, "
  function(el, x) {
    // Function to highlight feature on mouseover
    function highlightFeature(e) {
      var layer = e.target;
      layer.setStyle({
        weight: 5, // Increase border width
        color: '#0000FF' // Highlight border color
      });
    }

    // Function to reset highlight on mouseout
    function resetHighlight(e) {
      var layer = e.target;

      // Handle CircleMarker layers
      if (layer instanceof L.CircleMarker) {
        layer.setStyle({
          color: layer.options.originalColor, // Reset to original stroke color
          weight: 1.5 // Reset border weight
        });
      } else {
        // Reset other layers
        layer.setStyle({
          weight: 1.5, // Reset border weight
          color: 'black' // Reset border color
        });
      }
    }

    // Iterate over all layers and add event listeners
    this.eachLayer(function(layer) {
      // Store original styles for CircleMarker layers
      if (layer instanceof L.CircleMarker) {
        layer.options.originalColor = layer.options.color || 'black'; // Store original color
      }

      // Add hover event listeners
      layer.on({
        mouseover: highlightFeature,
        mouseout: resetHighlight
      });
    });
  }
")
    
    })
  
  
  output_temp <- renderPrint({
    input$today_map_mouseover_shape_mouseover$id
  })
 
  
  output$svi_plot <- renderPlotly({
    
    if (is.null(input$today_map_shape_mouseover)) {
      svi <- svi %>% 
        group_by(var) %>% 
        summarize(val = mean(val)
        )
      current_val <- reactiveVal(1)
    } else {
      svi <- svi %>% 
        filter(FIPS == input$today_map_shape_mouseover$id)
  
    }

    
    plot_ly(
      type = 'scatterpolar',
      r = svi$val,
      theta = svi$var,
      fill = 'toself',
      fillcolor = 'green',
      hoverinfo = 'text', 
      mode = 'none',
      opacity = .7
    ) %>%
      layout(
        title = list(text = "Social Vulnerability Characteristics (%)", font = list(color = "white", face = "bold", size = 17.5)),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100), # Adjust the range dynamically based on your data
            showticklabels = TRUE,      # Ensures tick labels are displayed        # Explicitly place ticks at the variable values
            tickfont = list(color = "black", size = 15), # Styling tick labels
            gridcolor = "green"         # Sets gridline color
          ),
          angularaxis = list(
            gridcolor = "green" # Set the angular gridlines to green
          )
        ),
        showlegend = FALSE
      ) %>%
      layout(plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent") %>% 
    layout(font = list(color = '#FFBF00', size = 13))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
