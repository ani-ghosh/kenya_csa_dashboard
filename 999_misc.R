rr <- rast("dashboard/data/KEN_rural_pop_1km.tif")
writeRaster(rr, "dashboard/data/test.tif", wopt= list(gdal=c("of=COG")))

r1 <- stars::read_stars("dashboard/data/KEN_rural_pop_1km.tif")
leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addStarsImage(r1, project = TRUE)

r2 <- stars::read_stars("dashboard/data/test.tif")
leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addStarsImage(r2, project = TRUE)

r2 <- raster("dashboard/data/KEN_rural_pop_1km.tif")
leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addRasterImage(r2)

r <- stars::read_stars("data/KEN_rural_pop_1km.tif")
r <- stars::read_stars("dashboard/data/KEN_rural_pop_1km.tif")

vals <- values(as(r, "Raster"))
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), vals,
                    na.color = "transparent")

basemap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lat = 0, lng = 33, zoom = 5) %>%
  addStarsImage(r, project = TRUE, colors = pal) %>%
  # add a legend
  addLegend(
    pal = pal,
    values = vals,
    position = "bottomleft",
    title = "Number of </br> weekly responses",
    opacity = 0.9
  ) %>%
  # add a minimap
  addMiniMap(tiles = "OpenStreetMap")
basemap


# https://cran.r-project.org/web/packages/tiler/vignettes/tiler-intro.html
server <- function(input, output, session){
  addResourcePath("mytiles", "C:/Users/.../mapTiles")
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = "/mytiles/{z}/{x}/{y}.png")
  })
}


  # add a legend
  addLegend(
    pal = pal,
    values = vals,
    position = "bottomleft",
    title = "Variable title",
    opacity = 0.9
  )