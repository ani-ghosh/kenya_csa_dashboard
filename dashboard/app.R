# pacman::p_load(geojsonio, leafem, leaflet, lubridate, 
#                maps, magrittr, plotly, raster, RColorBrewer, readxl,
#                reshape2, rvest, sf, shiny, shinyWidgets, shinydashboard, shinythemes, shinyTree, 
#                terra, tidyverse, stars, rgdal, rjson, tmap, mapview)

library(geojsonio)
library(leafem)
library(leaflet)
library(lubridate)
library(maps)
library(magrittr)
library(plotly)
library(raster)
library(RColorBrewer)
library(readxl)
library(reshape2)
library(rvest)
library(sf)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(shinyTree)
library(tidyverse)
library(stars)
library(terra)
library(rgdal)
library(rjson)
library(tmap)
library(mapview)

##############################################################################################################
# data input

basemap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lat = 0, lng = 33, zoom = 5.5) %>%
  # add a minimap
  addMiniMap(tiles = "OpenStreetMap") %>%
  addMouseCoordinates()

# projectPath <- "D:/AICCRA/csa_dashboard/dashboard/"
themes <- read.csv("data/themes.csv")
ke_counties <- readOGR("data/County.shp")

# options
mapoptiontree <- list(Climate = list(`1995-2015` = list(`annual mean temperature`="",
                                                        `annual total rainfall`="",
                                                        P95=""),
                                     `2020-2040` = list(`annual mean temperature`="",
                                                        `annual total rainfall`="",
                                                        P95=""),
                                     `2041-2060` = list(`annual mean temperature`="",
                                                        `annual total rainfall`="",
                                                        P95="")),
                      `Climate risk` = list(drought="", `flood`="", `heat stress`="", multiple=""),
                      Satellite = list(NDVI="", `soil moisture`="", evapotranspiration=""),
                      Biodiversity = list(`protected areas`="", `lakes`=""),
                      Market = list(`travel time`="", `distance to road`=""), 
                      Soils = list(`soil clay particles`="", `CEC`="", `bedrock`="", 
                                   `organic carbon content`="", `organic carbon stock`=""),
                      `Population`="")

###############################################################################################################

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">CSA dashboard</a>'), id="nav",
             windowTitle = "Kenya County Profile",
             
             tabPanel("Maps",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("summary_map", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Make selection for map")), style="color:#045a8d"),
                                        shinyTree("tree", checkbox = TRUE, multiple = FALSE),
                                        h3(textOutput("Showing map of "), align = "right"),
                                        # h3(verbatimTextOutput("selnames"),  align = "right"),
                                        # h3(textOutput("check"), align = "right"),
                                        h3(downloadButton("downloadLayer","Download Layer")),
                                        h3(downloadButton("downloadMap", "Download as Map"))
                                        
                                        
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://ccafs.cgiar.org/', tags$img(src='ccafs logo for web.jpg',height='100',width=325)))
                          
                      )
             ),
             tabPanel("Livelihood",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("County specific summary to go here.")), style="color:#045a8d"),
                          
                          # "Selection choice to update county."
                          selectInput("county", "Select County", choices = ke_counties$COUNTY)
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Plot 1", verbatimTextOutput("plot goes here"))
                          )
                        )
                      )
             ),
             
             ###### Suitability Analysis #########
             
             # tabPanel("Suitability Analysis",
             #          
             #          sidebarLayout(
             #            sidebarPanel(
             #              
             #              span(tags$i(h5("The tools allows you to compute climate risk score based on the key production risks in a farm. You can provide the GPS location of the farm, upload a csv/geojson/kml file with GPS locations or select points on the map (feature coming soon). The diagram on the left shows the basic mechanism of calculating the risk.")), style="color:#045a8d"),
             #              
             #              textInput("crop", "Value chain", "sorghum"),
             #              
             #              numericInput("y-coordinate", "Latitude",  -3.072),
             #              numericInput("x-coordinate", "Longitude",  39.62),
             #              
             #              fileInput("file1", "Choose CSV/GEOJSON/KML File", accept = ".csv|.geojosn|.kml")
             #            ),
             #            
             #            mainPanel(
             #              tabsetPanel(
             #                tabPanel("Background"),
             #                tabPanel("Climate risk report"))
             #            )
             #          ),
             #          
             #          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
             #                        tags$a(href='https://ccafs.cgiar.org/', tags$img(src='ccafs logo for web.jpg',height='100',width=325)))
             #          
             # ),
             
             tabPanel("Value Chain Analysis",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("Value chain analysis to go here.")), style="color:#045a8d"),
                          
                          "Selection choice to update options."
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Analyis 1", verbatimTextOutput("analysis go here"))
                          )
                        )
                      )
             ),
             
             tabPanel("Data",
                      numericInput("maxrows", "Rows to show", 10),
                      tableOutput("table"),
                      # verbatimTextOutput("table"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      "Data published by ", tags$a(href="https://alliancebioversityciat.org/", 
                                                   "Kenya CSA team.")
             ),
             tabPanel("Background",
                      tags$div(
                        tags$h4("Last update"), 
                        "The graphics in the website updated depending on the data availability.", 
                        
                        tags$br(),tags$br(),tags$h4("Background"), 
                        "Kenya CSA description to go here"
                      )
             )
             
  )          
)

server <- function(input, output, session) {
  
############################ MAPS ##############################################  
  
  output$tree <- renderTree({
    mapoptiontree
  })
  
  # selected names from tree
  sel_names <- reactive({
    tree <- input$tree
    req(tree)
    unlist(get_selected(tree))
    
  })
  
  selected <- reactive({
    x <- themes[themes[, "layers"] == sel_names(), ]
    x
    
  })
  
  selected_path <- reactive({
    selected() %>%
      select(file_path)
  })
  
  chosen_color <- reactive({
    selected() %>%
      select(color_theme)
    
  })
  
  # Load basemap and layers
  output$summary_map <- renderLeaflet({
    basemap
    
  })
  
  observeEvent(input$tree, {
    
    tryCatch({
      
      # reading shapefile
      # To do: optimise this section to read all vectors
      if(sel_names() == "protected areas"){

        shp_layer <- readOGR(paste0("data/",selected_path(), '.shp'))

        pal0 <- colorFactor(paste0(chosen_color()), domain = shp_layer$DESIG)
        
        # include pop ups
        labels <- sprintf(
          "<strong>%s</strong><br/>%s",   #<sup></sup>",
          shp_layer$NAME, shp_layer$DESIG
        ) %>% lapply(htmltools::HTML)

        leafletProxy ("summary_map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          addPolygons(data = shp_layer,
                      color = ~pal0(DESIG),
                      fillColor = ~pal0(DESIG),
                      dashArray = "2",
                      highlightOptions = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = labels

          ) 
        
      } else if(sel_names() == "lakes") {

        shp_layer <- readOGR(paste0("data/", selected_path(), '.shp'))

        pal0 <- colorFactor(paste0(chosen_color()), domain = shp_layer$NAME)
        
        leafletProxy ("summary_map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          addPolygons(data = shp_layer,
                      color = ~pal0(NAME),
                      fillColor = ~pal0(NAME))

      } else {
        r <- stars::read_stars(paste0("data/", selected_path(), '.tif'))
        
        y <- values(as(r, "Raster"))
        y <- na.omit(y)
        
        val <- as.numeric(y)
        pal <- colorNumeric(paste0(chosen_color()), domain = val)
        
        leafletProxy ("summary_map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          addStarsImage(r, layerId = "values", project = TRUE, colors = paste0(chosen_color())) 
      }
      
    },
    
    # using id to show a single notification
    id <- "a",
    
    warning = function(warn){
      showNotification("Multiple selections, Please select a single layer", type = 'warning', id = id)
    },
    error = function(err){
      showNotification("Please select a single layer", type = 'err', id = id)}
    
    )
  }
  
  )
  
  # Using a separate observer to add legend  
  
  observe({
    req(input$tree)

    tryCatch({

      if(sel_names() == "protected areas"){

        shp_layer <- readOGR(paste0("data/", selected_path(), '.shp'))

        pal0 <- colorFactor(paste0(chosen_color()), domain = shp_layer$DESIG)

        proxy <- leafletProxy("summary_map")
        proxy%>%
          clearControls() %>%
          addLegend(
            pal = pal0,
            values = shp_layer$DESIG,
            position = "bottomright",
            title = sel_names(),
            opacity = 0.9
          )

        # Lakes doesn't need a legend so skipping to the rasters

      } else{
        r <- stars::read_stars(paste0("data/", selected_path(), '.tif'))
        y <- values(as(r, "Raster"))
        y <- na.omit(y)

        val <- as.numeric(y)
        pal <- colorNumeric(paste0(chosen_color()), domain = val)

        proxy <- leafletProxy("summary_map")
        proxy %>%
          clearControls() %>%
          addImageQuery(r, type = "mousemove", layerId = "values", project = TRUE) %>%
          addLayersControl(overlayGroups = "values") %>%
          addLegend(
            pal = pal,
            values = val,
            position = "bottomright",
            title = sel_names(),
            opacity = 0.9
          )
      }

      # error catching when file is not found
    },
    id = "a",

    warning = function(warn){
      showNotification("Multiple selections, Please select a single layer ", type = 'warning', id=id)
    },

    error = function(err){
      showNotification("Please select a single layer", type = 'err', id=id)}
    )

  })
  
  ############# Download the selected data ######
  
  mapsdata <- reactive({
  
    if(sel_names() == "protected areas" || sel_names() == "lakes"){
      shp_layer <- readOGR(paste0("data/", selected_path(), '.shp'))
      shp_layer
      
    } else {
      
      r <- stars::read_stars(paste0("data/", selected_path(), '.tif'))
      r
    }
    
  })
  
  # Download data
  # Download tiffs
  
    output$downloadLayer <- downloadHandler(
      filename = function() {
        paste0(sel_names(), ".tif")
      },

      content = function(file) {
        stars::write_stars(mapsdata(), file)
      }
    )
    
  # Download static jpeg/png maps
  # To do: improve the maps  
  
    statmaps <- reactive({
      
      if(sel_names() == "protected areas"){
        
        m <- tm_shape(mapsdata()) +
            tm_polygons(col = "DESIG_ENG") +
            tm_layout(main.title = "Kenya Protected Areas",legend.outside = TRUE,
                      legend.title.size = 1)
        m
      
      } else if(sel_names() == "lakes"){
        m <- tm_shape(mapsdata()) +
          tm_polygons(col = "NAME") +
          tm_layout(main.title = "Kenya Lakes",legend.outside = TRUE,
                    legend.title.size = 1)
        m
        
      } else {
        m <- tm_shape(mapsdata()) +
          tm_raster(palette = paste0(chosen_color())) +
          tm_layout(main.title = paste0("Kenya ", sel_names()),
                    main.title.size = 1,
                    main.title.position = "center",
                    legend.outside = TRUE,
                    legend.text.size = 0.5,
                    legend.title.size = 0.5)
        m
        
      }
      
    })
    
  
  output$downloadMap <- downloadHandler(
    
    filename = function() {
      paste0(sel_names(), ".jpeg")
    },

    content = function(file) {
      tmap_save(statmaps(), file)
    }
  )
  
  # # Download shapefile
  # # To do: downloadLayer option to choose whether to download tiff or shapefile(zipped)
  # 
  # output$downloadLayer <- downloadHandler(
  # 
  #   filename = "shapefile.zip",
  #   content = function(file) {
  # 
  #     data = maps_data()
  # 
  #     # create a temp folder for shp files
  #     temp_shp <- tempdir()
  # 
  #     # write shp files
  #     writeOGR(data, temp_shp, "protected_areas", "ESRI Shapefile",
  #              overwrite_layer = TRUE)
  # 
  #     # zip all the shp files
  #     zip_file <- file.path(temp_shp, "protected_areas.zip")
  #     shp_files <- list.files(temp_shp,
  #                             "protected_areas",
  #                             full.names = TRUE)
  # 
  #     zip_command <- paste("zip -j",
  #                          zip_file,
  #                          paste(shp_files, collapse = " "))
  #     system(zip_command)
  # 
  #     # copy the zip file to the file argument
  #     file.copy(zip_file, file)
  # 
  #     # remove all the files created
  #     file.remove(zip_file, shp_files)
  #   }
  # )
  
######################### COUNTY PROFILE #######################
  
  
  
######################### STAKEHOLDER ANALYSIS ###################
  
  
  
######################### DATA ###################################
  
  temp <- list.files(path="data/", pattern="*.json", full.names=TRUE)

  metadata <- purrr::map_df(temp, function(x) {
    purrr::map(jsonlite::fromJSON(x), function(y) ifelse(is.null(y), NA, y))
  })

  data <- reactive({
    metadata <- metadata[,3:6]
    metadata
    
  })
  
  output$table <- renderTable({
    
    shiny::validate(
      need(input$maxrows != "", "Please select the number of rows to show")
    )
    
    head(data(), input$maxrows)
  })
  
  # Download as csv
  
  output$downloadCsv <- downloadHandler(
    
    filename = function() {
      paste0("metadata", ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
################################ BACKGROUND ##############  
  
  
}

shinyApp(ui, server)

# https://stackoverflow.com/questions/53142648/renderleaflet-legend-values-are-not-updated
# https://community.rstudio.com/t/addlegend-based-on-reactive-values-in-r-leaflet/14719
# https://rstudio.github.io/leaflet/shiny.html
# https://stackoverflow.com/questions/41707760/download-a-shape-file-from-shiny
# https://stackoverflow.com/questions/60997933/if-statement-caused-problem-to-the-downloadhandler-in-shiny-app