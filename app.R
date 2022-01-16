#################### Plotting Clustering Results ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##
## Goal of this script:
## Develop an interactive map to plot the clustering results of the landfills in regard to their plastic leakage risk
## Develop an interactive web app for users & stakeholders to get an overview over the risk assessment & the data included

## install required packages (if not installed yet)
require(class)
require(dplyr)
require(leaflet)
require(leaflet.extras)
require(raster)
require(readr)
require(rgdal)
require(rgeos)
require(rsconnect)
require(sf)
require(sp)
require(shiny)
require(tidyverse)



#### 0. Import Data ####

## import landfills clusters spatial data (point data)
landfills_clusters_sf <- st_read('./data/landfill_clusters.gpkg')
landfills_clusters <- readOGR("./data/landfill_clusters.gpkg")

### import landfill polygons
landfills_polygons <- readOGR("./data/landfills/OpenLandfills_Vietnam.shp", use_iconv = T, encoding = "UTF-8")

## import shapefile of vietnam
vietnam <- readOGR("./data/vietnam/vietnam.shp")



#### 1. Interactive Map (Leaflet) ####

## plot map with landfills colored by cluster
# e.g. plot water distance < 500m in red

# create color palette
cof <- colorFactor(c("green","blue","red"), domain = c("1","2","3"))

## create map with leaflet package
map <- leaflet(landfills_clusters_sf) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>% # add basemap
    setView(lng = 105.48, lat = 15.54, zoom = 6) %>% # set map to location of vietnam
    addMiniMap %>% # add mini map which changes according to map frame
    # add outline of vietnam
    addPolygons(data = vietnam, fill = F, weight = 3, color = "#FFFFCC", group = "Outline") %>%
    # add landfill outlines
    addPolygons(data = landfills_polygons, fill = F, weight = 3, color = "#AA40FF") %>%
    # add risk level of landfill as colored markers
    addCircleMarkers(data = landfills_clusters_sf, color = ~cof(km_cluster_unstand), radius = sqrt(landfills_clusters_sf$area_ha)*2, 
                     fillOpacity = 0.5, label = ~name, group = "Risk") %>%
    addLegend("bottomleft", colors = c("red","blue","green"), labels = c("high", "medium", "low"), title = "Leakage Risk")
map



#### Interactive Map ####

## creates the user interface of the app
ui_inter <- fluidPage(
  
  h1("Plastic Leakage Risk Classification of Landfills in Vietnam", id = "nav"), # first level header
                      
  tabPanel("Interactive Map",
    div(class = "outer",
       
       leafletOutput("map", height = "650px"), # sets map output
       
       # set location & size of panel with weather plots
       absolutePanel(id = "controls", class = "panel panel-default",
         draggable = T,
         top = 75, left = "auto", right = 20, bottom = "auto", width = 250, height = "auto",
         
         h3("Weather Data", align = 'center'), # sets title - h3 for 3rd level header
         
         # set outputs for plots
         plotOutput("histRain", height = 200),
         plotOutput("histWind", height = 200),
       ),
    )
  )
)


df <- landfills_clusters_sf[-c(2,9:10,12,16:17)] # select relevant columns

## add long & lat coordinates
df$long <- st_coordinates(landfills_clusters_sf)[,1]
df$lat <- st_coordinates(landfills_clusters_sf)[,2]


## specifies the content of the app (backend)
server_inter <- function(input, output, session) {
    
    ## create interactive map with leaflet
    output$map <- renderLeaflet({
        map %>%
            # add toolbox to draw new polygons
            addDrawToolbar(
                targetGroup = "drawnPoly", 
                rectangleOptions = F, 
                polylineOptions = F, 
                markerOptions = F, 
                editOptions = F,
                circleOptions = F,
                circleMarkerOptions = F,
                polygonOptions = drawPolygonOptions(showArea = T, repeatMode = F, shapeOptions = 
                                                        drawShapeOptions(clickable = T)))
    })
    
    latlongs <- reactiveValues() # temporary to hold coordinates
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0)) # empty dataframe to store coordinates
    
    
    ## create empty reactive spatial df to store drawn polygons
    value <- reactiveValues()
    value$drawnPoly <- SpatialPolygonsDataFrame(SpatialPolygons(list()), data = data.frame(notes=character(0), stringsAsFactors = F))
    
    ## save the current polygon to start drawing another polygon
    observeEvent(input$map_draw_new_feature, {
         
        coor <- unlist(input$map_draw_new_feature$geometry$coordinates) # extract coordinates of new polygon
        
        Longitude <- coor[seq(1,length(coor), 2)] 
        Latitude <- coor[seq(2,length(coor), 2)]
        
        isolate(latlongs$df2 <- rbind(latlongs$df2, cbind(Longitude, Latitude)))
        
        poly <- Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
        polys <- Polygons(list(poly), ID = input$map_draw_new_feature$properties$`_leaflet_id`)
        spPolys <- SpatialPolygons(list(polys))
        #print(spPolys)
        
        value$drawnPoly <- rbind(value$drawnPoly, SpatialPolygonsDataFrame(spPolys, data = data.frame(notes = NA, row.names = row.names(spPolys))))
        
        ## add polygons to landfills polygons df
        test <- SpatialPolygonsDataFrame(spPolys, data = data.frame(name = 1:length(spPolys), row.names = row.names(spPolys)))
        test@data$area <- NA
        test@data$Notes <- NA
        test@data$location <- NA
        test@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        
        # combine current & new polygons into one dataframe by row
        new_polygons <- rbind(landfills_polygons, test)
        
        ## export new & old landfills to shapefile
        shapefile(x = new_polygons, filename = "./data/landfills/OpenLandfills_Vietnam.shp", overwrite = T)
        
        ## export only new landfills to shapefile, to re-run script only with new landfills (faster)
        shapefile(x = test, filename = "./data/landfills/OpenLandfills_Vietnam_new.shp", overwrite = T)
  
        
        
        
        #### Water Areas (JRC Global Surface Water)
        ## downloaded via Google Earth Engine & pre-processed in QGIS (faster & less memory)
        jrc_water <- raster("./data/JRC_GlobalSurfaceWater_Vietnam_30.tif")
        
        
        #### DEM
        slope <- raster("./data/dem/dem_slope.tif")
        
        
        #### Climate Data
        climate_stations_rain <- readOGR("./data/climate_stations_rain.shp")
        climate_stations_wind <- readOGR("./data/climate_stations_wind.shp")
        
        ## import newly created landfill polygons (from webapp)
        #landfills <- readOGR("./data/landfills/OpenLandfills_Vietnam_new.shp", use_iconv = T, encoding = "UTF-8")
        # landfills <- test <- test
        
        ## calculate landfill area in ha (from meters) (polygon area)
        test$area_ha <- area(test)/10000
        
        # first convert data to simple feature object (sf) (for easier operation & later plotting with ggplot)
        # convert CRS to get matching CRS
        landfills_sf <- st_as_sf(test, crs(jrc_water))
        
        # calculate centroids of landfills for calculating distance to nearest points/polygons
        landfills_sf_centroids <- st_centroid(landfills_sf)
        
        
        #### II. Plastic Leakage Factors ####
        
        #### 1. Climatic conditions (Precipitation (daily) & Wind Speed (hourly))
        ## point data --> find nearest station
        
        ## save results in spatialdataframe
        landfills_factors <- landfills_sf_centroids
        
        # initiate columns with dummy variable
        landfills_factors$dist_station <- -1
        landfills_factors$rain <- -1
        landfills_factors$windspeed <- -1
        
        ## function to find nearest climate station & save corresponding data
        nearest_climate_station <- function(landfills_factors, climate, x) {
          
          # convert to simple features (sf) object for easier handling
          climate_stations_sf <- st_as_sf(climate)
          
          ## calculate the distance matrix in meters using Great Circle distance (for lat/long data) from one landfill to all climate stations
          dist_climate <- st_distance(landfills_factors[i,]$geometry, climate_stations_sf$geometry)
          
          ## calculate minimum distance
          landfills_factors[i,]$dist_station <- min(dist_climate)
          
          ## find row associated to min distance
          dist_min_climate <- (which(dist_climate == min(dist_climate), arr.ind=TRUE))[[1,2]]
          
          ## find associated climate station & save climate values to sf object
          if(x == 1) {
            landfills_factors[i,]$rain <- climate_stations_sf[dist_min_climate,][[2]]
          } else {
            landfills_factors[i,]$windspeed <- climate_stations_sf[dist_min_climate,][[2]]
          }
          return(landfills_factors)
        }
        
        
        #### 3. Water Areas (JRC) (& Flooding)
        
        # initiate columns with dummy variable
        landfills_factors$dist_water <- -1
        landfills_factors$dist_permwater <- -1
        landfills_factors$flood_risk <- -1 # % of area flooded
        
        ## function to find nearest water & save corresponding data
        nearest_water <- function(landfills_factors) {
  
          # intersect to get water area in buffer
          water <- intersect(jrc_water, st_buffer(landfills_sf[i,], dist = 100)) #100m buffer
          ## flood risk (% flooded)
          landfills_factors[i,]$flood_risk <- sum(values(water), na.rm=T) / length(values(water)) # na values counted as 0
          
          # get broader buffer for distances to water bodies
          buffer_broad <- st_buffer(landfills_sf_centroids[i,], dist = 10000) # 10km
          water_broad <- intersect(jrc_water, buffer_broad)
          # polygonize to calculate distance
          water_vector <- rasterToPolygons(water_broad, fun = function(x){x>0}, na.rm = T, dissolve = T)
          
          ## calculate minimum distance to closest water
          # account for no water in buffer
          if(is.null(water_vector)) {
            landfills_factors[i,]$dist_water <- NA
            landfills_factors[i,]$dist_permwater <- NA
          } else  {
            
            landfills_factors[i,]$dist_water <- min(st_distance(st_as_sf(water_vector), landfills_sf_centroids[i,]))
            
            # calculate distance to closest permanent water (e.g. river)?? 
            water_perm <- water_vector[water_vector[[1]] >=50,]
            
            # account for no permanent water
            if (length(water_perm) != 0) {
              landfills_factors[i,]$dist_permwater <- min(st_distance(st_as_sf(water_perm), landfills_sf_centroids[i,]))
            } else {
              landfills_factors[i,]$dist_permwater <- NA
            }
          }
          return(landfills_factors)
        }
        
        
        ## Distance to Ocean 
        ocean_vnm <- readOGR("./data/ocean_vnm.shp")
        
        landfills_factors$dist_ocean <- -1
        
        ## function to find distance to ocean
        distance_ocean <- function(landfills_factors) {
          
          ## calculate the distance matrix in meters using Great Circle distance (for lat/long data)
          dist_ocean <- st_distance(landfills_factors[i,]$geometry, st_as_sf(ocean_vnm))
          
          ## calculate minimum distance
          landfills_factors[i,]$dist_ocean <- min(dist_ocean)
          
          return(landfills_factors)
        }
        
        
        #### 5. Topography - DEM (Digital Elevation Model)
        landfills_factors$slope <- -1
        
        ## function to get DEM slope values per landfill
        mean_slope <- function(landfills_factors) {
          
          landfills_factors[i,]$slope <- mean(values(intersect(slope, test[i,])), na.rm =T)
          
          return(landfills_factors)
        }
        
        
        ## loop over landfills - take one landfill at once & calculate variable values
        # index for loop
        i <- 1
        while (i <= length(landfills_factors$geometry)) {
          # function to find nearest station & save climate attributes into sf object
          landfills_factors <- nearest_climate_station(landfills_factors, climate_stations_rain, 1)
          landfills_factors <- nearest_climate_station(landfills_factors, climate_stations_wind, 2)
          
          # function to find nearest water body
          landfills_factors <- nearest_water(landfills_factors)
          
          # function to get mean slope per landfill area
          landfills_factors <- mean_slope(landfills_factors)
          
          # distance to ocean
          landfills_factors <- distance_ocean(landfills_factors)
          
          # increment i
          i <- i+1
        }
        
        
        ## take smallest water distance
        landfills_factors$watermin <- with(landfills_factors, 
                                           pmin(landfills_factors$dist_water, landfills_factors$dist_permwater, landfills_factors$dist_ocean, na.rm=T))
        
        
        ## save dataframe as CSV
        #write.table(landfills_factors, file = "./data/landfill_variables.csv", row.names = F, fileEncoding = "UTF-8", sep = ";")
        
        ## save as geopackage
        st_write(landfills_factors, "./data/landfill_variables_new.gpkg", overwrite=T, append=F)
        
        #### TODO: don't overwrite original landfill_variables ####
        
        
        #### Predict Leakage Risk ####
        
        ## import outcome of data prep script
        variables <- readOGR("./data/landfill_variables_new.gpkg")
        #variables <- landfills_factors
        
        # basic landfills as training data
        train <- st_drop_geometry(landfills_clusters_sf)[,c(7:8,11,13:15)]
        
        ## predict risk class/cluster of new landfill (without re-running clustering algorithm)
        # add cluster as row
        # newly created landfills (from webapp) as testing data
        variables$km_cluster_unstand <- class::knn(train = train[,-6], 
                                                   test = variables@data[,c(7:8,11,13:14)], k = 1, cl = train$km_cluster_unstand)
      
        # drop not needed columns
        landfills_clusters_sf$risk <- NULL
        landfills_clusters_sf$risk_label <- NULL
        
        ## combine all landfills into one spdf
        # save results as shapefile
        st_write(st_as_sf(rbind(landfills_clusters, variables)), "./data/landfill_clusters.gpkg", overwrite = T, append = F)
        
        
        ## update plot upon ending draw
        observeEvent(input$map_draw_stop, {
            
            # replot map - remove DrawToolbar to clear the features & add it back
            leafletProxy('map') %>% 
                removeDrawToolbar(clearFeatures = T) %>% removeShape('temp') %>% clearGroup('drawnPoly') %>% 
                # add new polygons
                #addPolygons(data = value$drawnPoly, group = 'drawnPoly', color = "purple", layerId = row.names(value$drawnPoly)) %>% 
                addDrawToolbar(
                    targetGroup = "drawnPoly", 
                    rectangleOptions = F, 
                    polylineOptions = F, 
                    markerOptions = F, 
                    editOptions = F, 
                    circleOptions = F,
                    polygonOptions = drawPolygonOptions(showArea = T, repeatMode = F, 
                                                        shapeOptions = drawShapeOptions(clickable = T))) %>%
                # plot newly added landfills & risk cluster
                addCircleMarkers(data = variables, color = ~cof(km_cluster_unstand), radius = sqrt(variables$area_ha)*2,
                             fillOpacity = 0.5, label = ~name, group = "Risk")
        })
        
        latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0)) # clear df
    })
    
    # create object for clicked marker (=landfill)
    observeEvent(input$map_marker_click,{
        ## click returns clickid, long & lat
        click <- input$map_marker_click
        leafletProxy("map", session) %>% setView(lng = click$lng, lat = click$lat, zoom = 16)
    })
    
    # reactive expression that returns the set of landfills that are inside map bounds (to plot reactive graphs)
    landfillsInBounds <- reactive({
        if (is.null(input$map_bounds))
            return(landfills_clusters_sf[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(landfills_clusters_sf,
               st_coordinates(landfills_clusters_sf)[,2] >= latRng[1] & st_coordinates(landfills_clusters_sf)[,2] <= latRng[2] &
                   st_coordinates(landfills_clusters_sf)[,1] >= lngRng[1] & st_coordinates(landfills_clusters_sf)[,1] <= lngRng[2])
    })
    
    output$histRain <- renderPlot({
        # If no landfills are in view, don't plot
        if (nrow(landfillsInBounds()) == 0)
            return(NULL)
        hist(landfillsInBounds()$rain,
             main = "",
             xlab = "Yearly Average Precipitation (mm)",
             xlim = range(landfills_clusters_sf$rain),
             col = '#00ffff',
             border = 'white')
    })
    
    output$histWind <- renderPlot({
        # If no landfills are in view, don't plot
        if (nrow(landfillsInBounds()) == 0)
            return(NULL)
        hist(landfillsInBounds()$windspeed,
             main = "",
             xlab = "Yearly Average Wind Speed (km/h)",
             xlim = range(landfills_clusters_sf$windspeed),
             col = '#00DD00',
             border = 'white')
    })
}  

# Run the app
shinyApp(ui_inter, server_inter)

#rsconnect::showLogs(account = "carob", appName = "plasticleakagewebapp")