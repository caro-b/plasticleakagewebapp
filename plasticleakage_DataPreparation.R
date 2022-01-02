#################### Data Preparation ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
## R version: 4.1.1, Operating system: Windows 10
##


#### I. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("cartography","dplyr","gdalUtils","ggplot2","plyr","raster","reproducible","rgeos","rgdal","rnaturalearth","sf","sp","SpaDES","spatialEco","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'C://Users//carob//Documents//plasticleakagewebapp//data'



### II. DATA IMPORT & CLEANING ####

## download country boundary of Vietnam from GADM via inbuilt function
# downloaded from https://data.humdata.org/dataset/viet-nam-administrative-boundaries-polygon-polyline
vietnam <- readOGR("./data/vietnam/vietnam.shp")


#### Plastic Leakage Factors

#### 1. Weather Data (Meteostat) - Precipitation (mm) & Windspeed (km/h)
## downloaded via Python API
## data downloaded for time period: 01.01.2016-31.12.2020
meteostat <- read_csv("./data/Meteostat_Daily.csv")

## aggregate data over station & day
meteostat_daily <- meteostat %>%
  group_by(station, time) %>%
  dplyr::summarise(prcp_mean = mean(prcp, na.rm = T), wspd_mean = mean(wspd, na.rm = T))

meteostat_daily$year <- strftime(meteostat_daily$time, "%Y")
meteostat_daily$month <- strftime(meteostat_daily$time, "%m")
meteostat_daily$day <- strftime(meteostat_daily$time, "%d")
meteostat_daily$monthday <- format(meteostat_daily$time,"%m-%d")

## divide data into years
meteostat_daily_16 <- meteostat_daily[meteostat_daily$year == 2016,]
meteostat_daily_17 <- meteostat_daily[meteostat_daily$year == 2017,]
meteostat_daily_18 <- meteostat_daily[meteostat_daily$year == 2018,]
meteostat_daily_19 <- meteostat_daily[meteostat_daily$year == 2019,]
meteostat_daily_20 <- meteostat_daily[meteostat_daily$year == 2020,]

# check data availability per year
sum(is.na(meteostat_daily_16))
sum(is.na(meteostat_daily_17))
sum(is.na(meteostat_daily_18))
sum(is.na(meteostat_daily_19)) # least NA values - take 2019 as reference year
sum(is.na(meteostat_daily_20))

## plot time series one year to check precipitation distribution
qplot(x = time, y = prcp_mean,
      data = meteostat_daily_19, na.rm = T)

## aggregate over all 5 years to reduce NAs
meteostat_daily_mean <- meteostat_daily %>%
                          group_by(station, monthday) %>%
                          dplyr::summarise(prcp_yearly_mean = mean(prcp_mean, na.rm = T),
                                           wspd_yearly_mean = mean(wspd_mean, na.rm = T))

sum(is.na(meteostat_daily_mean))

qplot(x = monthday, y = prcp_yearly_mean, data = meteostat_daily_mean, na.rm = T)


## aggregate to yearly average
meteostat_yearly_mean <- meteostat_daily_mean %>%
  group_by(station) %>%
  dplyr::summarise(prcp_sum = sum(prcp_yearly_mean, na.rm = T),
                   wspd_mean = mean(wspd_yearly_mean, na.rm = T))


## Heavy Rain days per station (>=100 mm rain per day)
#heavyrain_stations <- meteostat_daily %>% group_by(station) %>% dplyr::summarise(heavyraindays = sum(prcp_mean >= 100, na.rm = T))

## Heavy Wind days per station (>39km/h)
#heavywind_stations <- meteostat_daily %>% group_by(station) %>% dplyr::summarise(heavywindhours = sum(wspd_mean >= 39, na.rm = T))


## add station coordinates to weather data
station_data <- unique(meteostat[,c(1,5:7)])

## join extra station information to daily weather data
climate_data <- left_join(meteostat_yearly_mean, station_data, by ="station")

# convert 0 values to NA (as 0 precipitation very likely wrong data)
climate_data$prcp_sum[climate_data$prcp_sum == 0] <- NA

## split data into rain & wind data frame to account for NA values (for different stations)
climate_data_rain <- climate_data[!is.na(climate_data$prcp_sum),][-3]
climate_data_wind <- climate_data[!is.na(climate_data$wspd_mean),][-2]



#### 2. Water Areas (JRC Global Surface Water)
## downloaded via Google Earth Engine & pre-processed in QGIS (faster & less memory)
jrc_water <- raster("./data/JRC_GlobalSurfaceWater_Vietnam_30.tif")



#### 3. Natural Hazards (Flooding & Storm)

#### a) Flooding
## derived from JRC Global Surface Water Dataset



#### 4. Topography - DEM (Digital Elevation Model)
## downloaded from: https://data.opendevelopmentmekong.net/en/dataset/digital-elevation-model-dem

# import DEM of Vietnam (30m) as RasterLayer
dem <- raster("./data/dem/dem_compress_clipped.tif")

# remove NA values
# dem_subset <- dem
# dem_subset[dem_subset < 0] <- NA
# plot(dem_subset)

# calculate slope in degrees
slope <- terrain(dem, opt = 'slope', unit = 'degrees')

# add slope as band
#dem_stack <- stack(dem_subset, slope)

# rename band
#names(dem_stack)[[1]] <- "elevation"

# plot(slope, main = "Slope (DEM)")
# plot(vietnam, add = T)

# export slope as raster file
writeRaster(slope, "./data/dem/dem_slope.tif", overwrite = T)
slope <- raster("./data/dem/dem_slope.tif")



#### III. create spatial points data from coordinates ####

#### 1. Climate Data

## create spatialpointsdataframe from lat, long coordinates
climate_stations_rain <- SpatialPointsDataFrame(coords = c(climate_data_rain[,c("longitude","latitude")]),
                                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                           data = climate_data_rain)
climate_stations_wind <- SpatialPointsDataFrame(coords = c(climate_data_wind[,c("longitude","latitude")]),
                                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                                data = climate_data_wind)

## export as shapefile
shapefile(x = climate_stations_rain, filename = "./data/climate_stations_rain.shp", overwrite = T)
shapefile(x = climate_stations_wind, filename = "./data/climate_stations_wind.shp", overwrite = T)


#################### Variable Preparation ####################

#### I. Import landfill Locations ####

## import landfill polygons
landfills <- readOGR(paste(dir, "/landfills/OpenLandfills_Vietnam.shp", sep = ""), use_iconv = T, encoding = "UTF-8")

## access & plot first landfill
plot(landfills[1,])

## calculate landfill area in ha (from meters) (polygon area)
landfills$area_ha <- area(landfills)/10000

# first convert data to simple feature object (sf) (for easier operation & later plotting with ggplot)
# convert CRS to get matching CRS
landfills_sf <- st_as_sf(landfills, crs(jrc_water))

# calculate centroids of landfills for calculating distance to nearest points/polygons
landfills_sf_centroids <- st_centroid(landfills_sf)
plot(landfills_sf$geometry)
plot(landfills_sf_centroids$geometry, add = T)



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
## raster with values if there is data (else na) --> nearest & average value?

# initiate columns with dummy variable
landfills_factors$dist_water <- -1
landfills_factors$dist_permwater <- -1
landfills_factors$flood_risk <- -1 # % of area flooded

## function to find nearest water & save corresponding data
nearest_water <- function(landfills_factors) {
  
  # 100m buffer around landfill to get flooding risk
  buffer <- st_buffer(landfills_sf[i,], dist = 100) # 100m
  # intersect to get water area in buffer
  water <- intersect(jrc_water, buffer)
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
  } else  { #if (!is.null(water_vector))
    
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

# download ocean data from naturalearth
ocean <- ne_download(scale = 10, type = 'ocean', category = 'physical')

# intersect ocean data with vietnam borders to get coastline
ocean_vnm <- intersect(ocean, vietnam)

shapefile(x = ocean_vnm, filename = "./data/ocean_vnm.shp", overwrite = T)


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
  
  slope_area <- intersect(slope, landfills[i,])
  landfills_factors[i,]$slope <- mean(values(slope_area), na.rm =T)
  
  return(landfills_factors)
}



## loop over landfills - take one landfill at once & calculate variable values

# index for loop
i <- 1

while (i <= length(landfills_factors$geometry)) {
  #function to find nearest station & save climate attributes into sf object
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
filename <- "./data/landfill_variables.csv"
write.table(landfills_factors, file = filename, row.names = F, fileEncoding = "UTF-8", sep = ";")

## save as geopackage
st_write(landfills_factors, "./data/landfill_variables.gpkg", overwrite=T, append=F)
