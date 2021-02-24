########## Creation of useable climate Data##########
#---
# title: "Prepare climate data"
# Author: Marvin Müsgen
# Description: In this Skript the climate inut data will be preprocessed
#---
getwd()
#Setting Paths
setwd("E:/Marvin/BB_rf") # setting working directory
Input_dir <- "/input/observations/climate_data/" # inpiut directory
funfold= paste0(getwd(),"/functions") # function folder directory
ext <- "/observations/NP_Boundary.shp"
# File in Input_dir: Polygon Shapefile with the extent of the area of Interest
area_path <- paste0(getwd(),"/input/observations/NP_Boundary.shp")
#Path to study area shape
rs_base <- list.files(paste0(getwd(), "/output/Sen2/indices/"),
                      pattern = "*.tif")[1]
winddir <- "data/data_D_MN003.csv" #Winddirection data
windspeed <- "/wind_speed/data/data_F_MN003.csv"
#DWD Climate Data Center (CDC): St?ndliche Stationsmessungen der
#Windrichtung in 10 m H?he in Grad f?r Deutschland,
#Version v19.3, abgerufen am <Datum>.

#set Variables
used_climate <- c("air_temp_mean", "drought", "evapo",
                  "precipitation", "sunshine", "soil_moist")
pckgs <- c("raster","rgdal","gdalUtils", "pbapply", "circular")
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes

#source functions
source(paste0(funfold, "/gdal_resample.R"))
source(paste0(funfold, "/check_library.R"))

#reading librarys
check_library(pckgs)

#read in studyarea
area <- readOGR(area_path)

#read in raster_base; must be a raster the climate date should resample to
rs_base <- raster(paste0(getwd(), "/output/Sen2/indices/", rs_base))

#list climate grid
climate_files <- list.files(paste0(getwd(), Input_dir), pattern = "*.tif")

#stack climate data
climate_ras <- stack(paste0(getwd(),Input_dir,climate_files))

rs_base <- raster(paste0(getwd(), "/output/Sen2/indices/", rs_base))

#crop climate raster to area
area <- sp::spTransform(area, climate_ras[[1]]@crs@projargs)
climate_crop <- crop(climate_ras, area)
climate_crop_list <- unstack(climate_crop)

#preprocess climate_data
climate_preproc<- pblapply(climate_crop_list, function(x){
  print(paste0("preprocess: ",names(x)))
  # preprocessing made according to DWD Climate Data Center (CDC),
  #Raster der Monatsmittel der Lufttemperatur (2m) f?r Deutschland,
  #Version v1.0.
  if(grepl("air_temp_mean", names(x)) == TRUE){
    x <- x / 10
  }

  # preprocessing made according to DWD Climate Data Center (CDC),
  #Raster des monatlichen Trockenheitsindex nach de Martonne f?r Deutschland,
  #Version v1.0.
  else if(grepl("drought", names(x)) ==TRUE){
    values(x)[values(x) == -999] <- NA
    x
  }
  # preprocessing made accourding to DWD Climate Data Center (CDC):
  #Monatliche Raster der realen Evapotranspiration ?ber Gras und sandigem Lehm,
  #Version 0.x, aktuelles Datum.
  else if(grepl("evapo", names(x)) == TRUE){
    x <- x / 10
  }

  # preprocessing made accourding to DWD Climate Data Center (CDC),
  #Raster der Monatssumme der Niederschlagsh?he f?r Deutschland, Version v1.0.
  else if(grepl("precipitation", names(x)) == TRUE){
    values(x)[values(x) == -999] <- NA
    x
  }

  # preprocessing made accourding to DWD Climate Data Center (CDC):
  #Monatliche Raster der mittleren Bodenfeuchte unter Gras und sandigem Lehm,
  #Version 0.x, aktuelles Datum.
  else if(grepl("soil_moist", names(x)) == TRUE){
    x <- x / 100
  }

  #preprocessing made accourding to DWD Climate Data Center (CDC),
  #Raster der Monatssumme der Sonnenscheindauer f?r Deutschland Version v1.0.
  else if(grepl("sunshine", names(x)) == TRUE){
    values(x)[values(x) == -999] <- NA
    x
  }
})

#create stack for resampling
climate_preproc <- stack(climate_preproc)
names(climate_preproc)
substring(names(climate_preproc),15, nchar(names(climate_preproc)))
#initialize progressbar
pb = txtProgressBar(min = 0, max = nlayers(climate_preproc), initial = 0,
                    style = 3)

#resample to 10 m; method biliniar because climate is not stationairy
for (k in 1:nlayers(climate_preproc)){
  print(paste(Sys.time(), "Resampling raster", k, "of",
              nlayers(climate_preproc),"..."))
  if (k == 1){
    l <-list()
    l[[1]] <- gdal_resample(climate_preproc[[k]],rs_base[[1]],
                            method = "bilinear")
    names(l[[1]]) <- names(climate_preproc)[[1]]
  }
  else{
    l[[k]] <- gdal_resample(climate_preproc[[k]],rs_base[[1]],
                            method = "bilinear")
    names(l[[k]]) <- names(climate_preproc)[[k]]
  }

  #setting progress
  setTxtProgressBar(pb,k)
}

#stack climate
climate_resample <-stack(l)
names(climate_resample)
#writing raster out
terra::writeRaster(climate_resample, paste0(getwd(),
                                            "/output/Parameter/climate.tif"))

##########END Creation of useable climate Data##########
