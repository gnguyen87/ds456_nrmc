library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(tigris)
library(bslib)

## MSP Spatial data

### Census Tracts
msp_census_tracts <- read.csv('data/minneapolis-tree-canopy.csv') %>%
  select(region_name)

msp_census_tracts <- msp_census_tracts %>%
  mutate(region_name = str_replace_all(region_name, '[:alpha:]*', "") %>% str_trim()) %>%
  filter(region_name != "27123043002") %>%
  filter(region_name != "27053980000") %>%
  pull(region_name) %>%
  as.list()


options(tigris_use_cache = TRUE)

mn_tracts <- tracts(state = "MN", cb = FALSE, year = 2022)  

mn_lake <- area_water(state = "MN", count = 'Hennepin')

msp_census_tracts_spatial <- mn_tracts %>%
  filter(GEOID %in%msp_census_tracts)

### Zip Code Shp

minneapolis_zip_codes <- c(
  "55401", "55402", "55403", "55404", "55405", "55406", "55407", 
  "55408", "55409", "55410", "55411", "55412", "55413", "55414", 
  "55415", "55416", "55417", "55418", "55419", "55423", "55430",
  "55454", "55455"
) 

zipcode <- zctas(starts_with = c("55"))

zipcode <- zipcode %>% filter(ZCTA5CE20 %in% minneapolis_zip_codes)

### Zip Code-Census Tract Crosswalk

crosswalk <- st_join(msp_census_tracts_spatial, zipcode, join = st_intersects) %>% 
  select(tract_id = GEOID, zip_code = ZCTA5CE20) %>%
  distinct()

## Social Vulnerability Index

social_vulnerability <- read.csv('data/svi_2020.csv')

social_vulnerability$FIPS <- social_vulnerability$FIPS %>% as.character()

social_vulnerability <- social_vulnerability %>% 
  mutate(RPL_THEMES = ifelse(as.numeric(RPL_THEMES)<0,0,RPL_THEMES )) 

social_vulnerability_spatial <- msp_census_tracts_spatial %>% 
  left_join(social_vulnerability, by = c('GEOID'= 'FIPS')) 


## Air Pollution

air_pollution_data_mn <- st_read('data/air_pollution_data_mn.csv')


air_pollution_data_msp <- air_pollution_data_mn %>%
  filter(ZIP_CODE %in% minneapolis_zip_codes) %>%
  mutate(`EMISSIONS..LB.` = as.numeric(`EMISSIONS..LB.`)) %>%
  mutate(`EMISSIONS..TONS.` = as.numeric(`EMISSIONS..TONS.`))

air_pollution_data_msp_wide <- air_pollution_data_msp %>%
  filter(POLLUTANT == "PM2.5 Primary") %>%
  group_by( ZIP_CODE) %>%
  summarize(
    emissions_tons = sum(`EMISSIONS..TONS.`)
  )

air_pollution_data_msp_wide_with_zip_code_2022 <- zipcode %>% 
  left_join(air_pollution_data_msp_wide, by = c("ZCTA5CE20" = "ZIP_CODE")) %>% 
  st_intersection(crosswalk)

## Tree Canopy

tree_canopy <- read.csv('data/minneapolis-tree-canopy.csv')
tree_canopy$region_name <- tree_canopy$region_name %>% str_replace_all('[:alpha:]*', "") %>% str_trim()

tree_canopy_spatial <- msp_census_tracts_spatial %>% 
  left_join(tree_canopy, by = c('GEOID'= 'region_name')) 

## Join data

tree_canopy_final <- tree_canopy_spatial %>%  st_drop_geometry()
air_pollution_final <- air_pollution_data_msp_wide_with_zip_code_2022 %>%  st_drop_geometry() 
social_vulnerability_spatial_final <- social_vulnerability_spatial %>% st_drop_geometry()


## Redlining and Racial Covenants
redlining <- st_read('data/redlining.json') %>% filter(city == "Minneapolis")
racial_cov <- st_read('data/Hennepin_County_Racial_Covenants_Table.csv') %>% 
  filter(City == "MINNEAPOLIS",
         !is.na(X),
         !is.na(Y)) %>% 
  mutate(X = as.numeric(X),
         Y = as.numeric(Y)) %>% 
  st_as_sf(coords = c("X","Y"), crs = "WGS84")

## Joined today's data (SVI, Tree Canopy, and Air pollution)

tree_canopy_final <- tree_canopy_spatial %>%  st_drop_geometry()
air_pollution_final <- air_pollution_data_msp_wide_with_zip_code_2022 %>%  st_drop_geometry() 
social_vulnerability_spatial_final <- social_vulnerability_spatial %>% st_drop_geometry()


final <- tree_canopy_final %>%
  left_join(air_pollution_final, by = c("GEOID" = "tract_id")) %>% 
  left_join(social_vulnerability_spatial_final,by = c("GEOID" = "GEOID") ) %>% 
  select(GEOID, tree_canopy_area, emissions_tons, RPL_THEMES ) %>% 
  distinct() %>% 
  mutate(emissions_tons = ifelse(is.na(emissions_tons), 0, emissions_tons)) %>% 
  group_by(GEOID) %>% 
  group_by(GEOID) %>% 
  summarize(tree_canopy_area = tree_canopy_area,
            air_pollution = mean(emissions_tons),
            svi_index = RPL_THEMES) %>% 
  ungroup() %>% 
  distinct() %>% 
  rename(
    `Tree Canopy`=tree_canopy_area,
    `Annual PM2.5 (tons)` = air_pollution
  ) %>% 
  filter(GEOID != 27123043002 ) %>% 
  filter(GEOID != 27053980000)


variables <- c("Tree Canopy", "Annual PM2.5 (tons)")

final_spatial <- msp_census_tracts_spatial %>% 
  left_join(final, by = c("GEOID" = "GEOID"))

msp_lake <- mn_lake %>% 
  st_crop(st_bbox(final_spatial)) %>% 
  st_make_valid()

## SVI map data
svi_index_list <- c("EP_POV150", "EP_UNEMP", "EP_UNINSUR", "EP_DISABL", "EP_MINRTY", "EP_NOVEH")

svi <- social_vulnerability %>% 
  pivot_longer(cols = 8:158, names_to = "var", values_to = "val") %>% 
  filter(var %in% svi_index_list) %>% 
  select(!1:5) %>% 
  select(!LOCATION)

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = bs_theme( bootswatch = "simplex"),
  title = "Examining The Roots of Environmental Injustice in Minneapolis",
  tabPanel(
    title = "Then versus Now",
    sidebarLayout(
      sidebarPanel(
        selectInput("var", label = "Choose an environmental variable", choices = variables)
      ),
      mainPanel(
        fluidRow(
          column(leafletOutput("cov_redlining_map"), width = 6),
          column(leafletOutput("today_map"), width = 6)
        ),
        fluidRow(
          plotlyOutput("svi_plot")
        )
      )
    )
  ),
  tabPanel(
    title = "About",
    "About this app"
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$text <- renderUI({ 
    HTML(paste(
      "",
      "From these graphs, I found that the educational attainment is suggested to be much lower in income-deprived towns. Furthermore, this trend performs worse and worse the higher the educational level. More interestingly, UK towns that tend to be of higher deprivation in terms of income tend to be bigger-sized. When examining the correlation between education score, which is based on the key stage 4 (14-16 year-olds) and achieved proportion of town in each key stage, as in those who successfully completed a certain key stage, I found that the higher the achieved town proportion in all key stages, the higher the key stage 4 education score and vice versa. In other words, towns with higher achived proportion in early stages will score higher in the education score in key stage 4, and towns with higher education scores in key stage 4 have higher achieved proportion rate in high-school and university. These findings suggest social and geoeconomic factors such as the relationship between town size and town economic performance as well as early access to and achievement in education are imperative to the town overall educational attainment level.",
      "Source: The dataset this week comes from The UK Office for National Statistics. It was explored in the July 2023 article 'Why do children and young people in smaller towns do better academically than those in larger towns?'",
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

    palette_function <- colorNumeric( palette = "Greens", domain = final_spatial[[var]])
    
    today_map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = final_spatial,
                  fillColor = ~palette_function(final_spatial[[var]]),
                  fillOpacity = 0.7,
                  color = "black", # Polygon border color
                  weight = 1, # Border thickness)
                  label = final_spatial$GEOID,
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
  
#   observe({
#   print(names(input))  # Print available inputs to ensure it's there
# })
#   
#   observe({
#     print(input$today_map_shape_mouseover)  # Inspect what input event provides
#   })

  # current_val <- reactiveVal(1)
  # 
  # observeEvent(input$reset, {
  #   current_val(NULL)
  # })
 
  
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
      
      validate(
        need(nrow(svi) != 0,"Invalid Census Tract. Try hovering on a valid land area." )
      )
  
    }
    
    
    plot_ly(
      type = 'scatterpolar',
      r = svi$val,
      theta = svi$var,
      fill = 'toself',
      fillcolor = 'green',
      mode = 'none',
      opacity = .7
    ) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE)),
        showlegend = FALSE
      ) %>% layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),
        showlegend = F
      ) %>%
      layout(plot_bgcolor  = "transparent",
             paper_bgcolor = "transparent")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
