MakeLeaflet <-function(name, dataframe, bins = c(0, 10, 20, 25, 100)){
  
  MyData <- AG %>% filter(grepl(name, LAD11NM ,ignore.case = T)) %>%
    rename(LSOA_CODE = lsoa11cd)
  
  MyData <- SpatialPointsDataFrame(coords =MyData[,c(1,2)], data = MyData,
                                   proj4string = CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'))
  MyData <- spTransform(MyData, CRS("+init=epsg:4326"))
  #random thing
  stuff <-data.frame(LSOA_CODE=unique(MyData$LSOA_CODE),row.names=unique(MyData$id))
  
  MyData<- points2polygons(MyData,stuff)
  
  #AG2 <- AG %>% group_by(LSOA_CODE) %>% summarise_all(funs(first))
  
  MyData <- dataframe %>%
    select(LSOA_CODE, LowUsePerc, WD16NM)%>%
    merge(MyData, .)
  
  class(MyData) # data.frame
  
  v<-"pk.eyJ1Ijoiam9ubm9iIiwiYSI6ImNqM2FlNXhpbDAwM3gzMW83ZW1hbHI0bzYifQ.4AfaY0Un0bKU41hZscpy9Q"
  
  m <- leaflet(MyData) %>%
    #addTiles()
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = v))
  
  
  pal <- colorBin("YlOrRd", domain = MyData$LowUsePerc, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>Low-use  %g &#37",
    MyData$WD16NM, MyData$LowUsePerc
  ) %>% lapply(htmltools::HTML)
  
  z <-m %>% addPolygons(
    fillColor = ~pal(LowUsePerc),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
              position = "bottomright")
  
  
  return(z)
}