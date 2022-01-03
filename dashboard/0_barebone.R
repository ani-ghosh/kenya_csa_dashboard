pacman::p_load(geojsonio, leafem, leaflet, lubridate, 
               maps, magrittr, plotly, raster, RColorBrewer, readxl,
               reshape2, rvest, sf, shiny, shinyWidgets, shinydashboard, shinythemes, shinyTree, 
               terra, tidyverse)

##############################################################################################################
# data input

basemap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lat = 0, lng = 33, zoom = 5) %>%
  # add a minimap
  addMiniMap(tiles = "OpenStreetMap")

# options
mapoptiontree <- list(climate = list(`1995-2015` = list(`annual mean temperature`="", 
                                               `annual total rainfall`="", 
                                               P95=""),
                            `2020-2040` = list(`annual mean temperature`="", 
                                               `annual total rainfall`="", 
                                               P95=""),
                            `2041-2060` = list(`annual mean temperature`="", 
                                               `annual total rainfall`="", 
                                               P95="")),
             `climate risk` = list(drought="", `flood`="", `heat stress`="", multiple=""),          
             satellite = list(NDVI="", `soil moisture`="", evapotranspiration=""),
             biodiversity = list(`protected areas`="", `lakes`=""),
             market = list(`travel time`="", `distance to road`=""))

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
                                        
                                        span(tags$i(h6("Make selection for map.")), style="color:#045a8d"),
                                        shinyTree("tree", checkbox = FALSE, multiple = FALSE),
                                        h3(textOutput("Showing map of "), align = "right"),
                                        #h3(verbatimTextOutput("sel_names"),  align = "right"),
                                        
                                    
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://ccafs.cgiar.org/', tags$img(src='ccafs logo for web.jpg',height='100',width=325)))
                          
                      )
             ),
             tabPanel("County profile",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("County specific summary to go here.")), style="color:#045a8d"),
                          
                          "Selection choice to update county."
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Plot 1", verbatimTextOutput("plot go here"))
                          )
                        )
                      )
             ),
             tabPanel("Climate risk analysis",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h5("The tools allows you to compute climate risk score based on the key production risks in a farm. You can provide the GPS location of the farm, upload a csv/geojson/kml file with GPS locations or select points on the map (feature coming soon). The diagram on the left shows the basic mechanism of calculating the risk.")), style="color:#045a8d"),
                          
                          textInput("crop", "Value chain", "sorghum"),
                          
                          numericInput("y-coordinate", "Latitude",  -3.072),
                          numericInput("x-coordinate", "Longitude",  39.62),
                  
                          fileInput("file1", "Choose CSV/GEOJSON/KML File", accept = ".csv|.geojosn|.kml")
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Background"),
                            tabPanel("Climate risk report"))
                        )
                      ),
                      
                      absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                    tags$a(href='https://ccafs.cgiar.org/', tags$img(src='ccafs logo for web.jpg',height='100',width=325)))
                      
             ),
             tabPanel("Stakeholder analysis",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("Stakeholder analysis to go here.")), style="color:#045a8d"),
                          
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
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("table"),
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
  output$tree <- renderTree({
    mapoptiontree
  })
  
  output$sel_names <- renderPrint({
    tree <- input$tree
    req(tree)
    get_selected(tree)[[1]][1]
  })
  
  r <- stars::read_stars("data/KEN_rural_pop_1km.tif")
  
  vals <- values(as(r, "Raster"))
  pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), vals,
                      na.color = "transparent")
  
  output$summary_map <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$tree, {
    leafletProxy("summary_map") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addStarsImage(r, project = TRUE, colors = pal) 
  })
}
shinyApp(ui, server)

# https://stackoverflow.com/questions/53142648/renderleaflet-legend-values-are-not-updated
# https://community.rstudio.com/t/addlegend-based-on-reactive-values-in-r-leaflet/14719
# https://rstudio.github.io/leaflet/shiny.html