########## Creation of useable climate Data##########
#---
# title: "Prepare climate data"
# Author: Marvin MÃ¼sgen
# Description: In this Skript the climate inut data will be preprocessed
#---

#Setting Paths
dem <- "eu_dem_v11_E40N20.TIF" # name of observation  # DOwnloaded from https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1.1/fetch-land-file?hash=97824c12f357f50638d665b5a58707cd82857d57
setwd("D:/Marvin/BB_rf") # setting working directory
Input_dir <- "/input/observations/dem/" # inpiut directory
funfold= paste0(getwd(),"/functions") # function folder directory
area_path <- paste0(getwd(),"/input/observations/NP_Boundary.shp") #Path to study area shape
rs_base <- "/output/Sen2/Indices/S2A_MSIL2A_20180405T103021_N0207_R108_T32UMU_20180405T141122_indices.tif" #path to extent base
winddir_file <- "/input/observations/climate_data/data/data_DD_MN008.csv" #Winddirection data 
windspeed_file <- "/input/observations/climate_data/wind_speed/data/data_F_MN003.csv"#windspeed dataDWD Climate Data Center (CDC): Stündliche Stationsmessungen der Windrichtung in 10 m Höhe in Grad für Deutschland, Version v19.3, abgerufen am <Datum>.

#set Variables
pckgs <- c("raster","rgdal","gdalUtils", "RSAGA", "terra", "pbapply")

#source functions
source(paste0(funfold, "/gdal_resample.R"))
source(paste0(funfold, "/check_library.R"))

#reading librarys
check_library(pckgs)

#read in studyarea
area <- readOGR(area_path)

#read in raster_base; must be a raster the dem should resample to
rs_base <- raster(paste0(getwd(), rs_base))

#read dem
dem <- raster::raster(paste0(getwd(),Input_dir,dem))

#reproject and resample dem
dem <- projectRaster(dem, rs_base, method = "bilinear")

#reproject sp
area <- sp::spTransform(area, dem@crs@projargs)

#crop
dem_crop <- crop(dem, area)

#write out dem
terra::writeRaster(dem_crop, paste0(getwd(), Input_dir, "/dem_10.tif"))
dem_file <- paste0(getwd(), Input_dir, "dem_10.tif")

#setting RSAGA environment; linking saga_gui.exe to R
env <- rsaga.env()

#have a look on the modules and libraries
rsaga.get.libraries(path = env$modules)
rsaga.get.modules(libs = "ta_morphometry", env = env)
rsaga.get.usage(lib = "ta_morphometry", module = "Slope, Aspect, Curvature", env = env)

#calculate slope
slope_file <- paste0(getwd(), Input_dir, "/slope.sgrd")
rsaga.slope(in.dem = dem_file, out.slope = paste0(getwd(), Input_dir, "/slope.sgrd"), env = env)

#calculate aspect
aspect_file <- paste0(getwd(), Input_dir, "/aspect.sgrd")
rsaga.aspect(in.dem = dem_file, out.aspect = aspect_file, env = env)

#calculating TWI
TWI_file <- paste0(getwd(), Input_dir,"/TWI.sqrd")
catch_area_file <- paste0(getwd(), Input_dir, "/Catchment_area.sgrd")

rsaga.get.modules(libs = "ta_hydrology", env = env)
rsaga.get.usage(lib = "ta_hydrology", env = env, module = "SAGA Wetness Index")
rsaga.geoprocessor(lib = "ta_hydrology", module = "SAGA Wetness Index",
                   param = list(DEM = dem_file, 
                                TWI = TWI_file,
                                AREA = catch_area_file), 
                   env = env)


#calculating wind direction

#reading winddirection data
winddir <- read.csv(paste0(getwd(),winddir_file))
Windspeed <- read.csv(paste0(getwd(), windspeed_file))
#getting month dates from winddata
month <- as.character(substr(winddir$Zeitstempel, 1, 6))
month <- unique(month)

#creating winddirection data frame
weighted_winddirection <- data.frame()

#calulate weighted mean of winddirection by Winddirection and Speed
for(i in 1:length(month)){
  
  if(i == length(month)){
    
    #calculate for month
    winddir_temp <- winddir[which(grepl(month[i], winddir$Zeitstempel) == TRUE),]
    WD <- winddir_temp$Wert
    WS <- Windspeed$Wert[which((Windspeed$Zeitstempel %in% winddir_temp$Zeitstempel) ==TRUE)]
    
    WS_mean <- mean(WS)
    
    V_east <- mean(WS * sin(WD * pi/180))
    V_north <- mean(WS * cos(WD * pi/180))
    
    mean_WD = atan2(V_east, V_north) * 180/pi
    mean_WD = (360 + mean_WD) %% 360
    
    weighted_winddirection[i,1] <- mean_WD 
    weighted_winddirection[i,2] <- month[i]
    weighted_winddirection[i,3] <- WS_mean
    
    #calulate for whole period
    winddir_temp <- winddir
    WD <- winddir_temp$Wert
    WS <- Windspeed$Wert[which((Windspeed$Zeitstempel %in% winddir_temp$Zeitstempel) ==TRUE)]
    
    WS_mean <- mean(WS)
    
    V_east <- mean(WS * sin(WD * pi/180))
    V_north <- mean(WS * cos(WD * pi/180))
    
    mean_WD = atan2(V_east, V_north) * 180/pi
    mean_WD = (360 + mean_WD) %% 360
    
    weighted_winddirection[i+1,1] <- mean_WD 
    weighted_winddirection[i+1,2] <- paste0(month[1],"_to_",month[i])
    weighted_winddirection[i+1,3] <- WS_mean
    colnames(weighted_winddirection) <- c("wmean_windir", "date", "wspeed_mean")
  }
  
  else{
    
   #calculate for month 
    winddir_temp <- winddir[which(grepl(month[i], winddir$Zeitstempel) == TRUE),]
    WD <- winddir_temp$Wert
    WS <- Windspeed$Wert[which((Windspeed$Zeitstempel %in% winddir_temp$Zeitstempel) ==TRUE)]
    
    WS_mean <- mean(WS)

    V_east <- mean(WS * sin(WD * pi/180))
    V_north <- mean(WS * cos(WD * pi/180))

    mean_WD = atan2(V_east, V_north) * 180/pi
    mean_WD = (360 + mean_WD) %% 360

    weighted_winddirection[i,1] <- mean_WD 
    weighted_winddirection[i,2] <- month[i]
    weighted_winddirection[i,3] <- WS_mean
  }
}

#getting winddirection operator
rsaga.get.modules(libs = "ta_morphometry", env = env)
rsaga.get.usage(lib = "ta_morphometry", env = env, module = "Wind Effect (Windward / Leeward Index)")

#initialize progressbar
pb = txtProgressBar(min = 0, max = nrow(weighted_winddirection), initial = 0, style = 3)

#calculation wind effect for every date on every rastercell
for(i in 1:nrow(weighted_winddirection)){
  
  #setprgressbar
  setTxtProgressBar(pb,i)
  
  #creating outputfile
  wind_effect_file <- paste0(getwd(), Input_dir,"Wind_effect_",weighted_winddirection$date[i],".sgrd")
  
  #start processing windeffect
  rsaga.geoprocessor(lib = "ta_morphometry", module = "Wind Effect (Windward / Leeward Index)",
                     param = list(DEM = dem_file, 
                                  DIR_CONST = weighted_winddirection$wmean_windir[i], 
                                  DIR_UNITS = 1, 
                                  ACCEL = weighted_winddirection$wspeed_mean[i],
                                  EFFECT = wind_effect_file),
                     env = env)
  #setprgressbar
  setTxtProgressBar(pb,i)
  print("Saga processing succesfull")
}

#calculating Topographic Position Index

#getting TPI operator
rsaga.get.modules(libs = "ta_morphometry", env = env)
rsaga.get.usage(lib = "ta_morphometry", env = env, module = "Topographic Position Index (TPI)")

#create TPI file
TPI_file <- paste0(getwd(), Input_dir, "TPI.sgrd")

#calculate TPI
rsaga.geoprocessor(lib = "ta_morphometry", module = "Topographic Position Index (TPI)",
                   param = list(DEM = dem_file,
                                TPI = TPI_file),
                   env = env)

#calculating incoming radiation

#getting modules
rsaga.get.libraries(path = env$modules)
rsaga.get.modules(libs = "ta_lighting", env = env)
rsaga.get.usage(lib = "ta_lighting", env = env, module = "Potential Incoming Solar Radiation")
rsaga.get.usage(lib = "ta_lighting", env = env, module = "Sky View Factor")

#create Sky vie factor file
sky_view_file <- paste0(getwd(), Input_dir, "Sky_View_Factor.sgrd")

#caluclate need Sky View Factor
rsaga.geoprocessor(lib = "ta_lighting", module = "Sky View Factor",
                   param = list(DEM = dem_file,
                                SVF = sky_view_file,
                                RADIUS = 1000),
                   env = env)

#creating radiation files
direct_insolation_file <- paste0(getwd(), Input_dir, "direct_insolation.sgrd")
diffuse_insolation_file <- paste0(getwd(), Input_dir, "diffuse_insolation.sgrd")

#calulate Potential incomining radiation
rsaga.geoprocessor(lib = "ta_lighting", module = "Potential Incoming Solar Radiation",
                   param = list(GRD_DEM = dem_file,
                                GRD_SVF = sky_view_file,
                                GRD_DIRECT = direct_insolation_file,
                                GRD_DIFFUS = diffuse_insolation_file),
                   env = env)

#####################

test <- raster("D:/Marvin/BB_rf/input/observations/soil/SMI_L02_Oberboden_monatlich_1951_2018_inv.nc")
plot(test)
values(test)
names(test)
