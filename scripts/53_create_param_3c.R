
########## Creation of and extraction Parametersets##########
#---
# title: "Prepare Parameter Data sets"
# Author: Marvin Müsgen
# Description: In this Skript the Parameters are processed.
# They will be resampled to correct size
# Data will bee extracted
# After Adaption this script works for Scenario 1 and 2
#---


#Settings
library(raster)
library(rgdal)
library(sp)
topo <- list.files("E:/Marvin/BB_rf/input/observations/dem",
                   pattern = ".sdat", full.names = TRUE)
dem <- raster::raster("E:/Marvin/BB_rf/input/observations/dem/dem_10.tif")
climate <- stack("E:/Marvin/BB_rf/output/Parameter/climate.tif")
soil <- list.files("E:/Marvin/BB_rf/input/observations/soil/",
                   pattern=".tif", full.names = TRUE)
xcoordcolnum <- 3 # column number of the x coordinate of your obseravtion date
ycoordcolnum <- 4

#Read files
soil <- stack(soil)
topo_files<- topo[seq(1,length(topo), 2)]
topo <- stack(topo_files)
plot(soil[[2]])
# Read dummy for checking projection
dummy <- readOGR("E:/Marvin/NP_Boundary.shp")
plot(dummy, add=TRUE)

#stack resample, project and stack
dummy <- sp::spTransform(dummy, CRS("+proj=utm +zone=32
                                    +datum=WGS84 +units=m +no_defs"))
dem <- crop(dem, dummy)
climate <- crop(climate, dummy)
soil <- projectRaster(soil, crs = crs(dummy))
soil_10 <- resample(soil, dem, method= "ngb")
soil <- crop(soil_10,dummy)
names(dem)
names(climate) <- names(climate_preproc)
names(soil)
topo <- crop(topo, dummy)
param <- stack(dem, soil)
param <- stack(param, topo)
param <- stack(param, climate)
rasterOptions(maxmemory = 1e+09)

#read in the observations
obs <-read.csv("E:/Marvin/BB_rf/input/observations/
               new/barkbeetle_obs_x_to_2018_3class.csv")
print("observations loaded")
head(obs)
#obs[,5:ncol(obs)] <- NULL
#defining coordinates, Lat = Y Long = X
obs[,xcoordcolnum] <- as.numeric(obs[,xcoordcolnum])
obs[,ycoordcolnum] <- as.numeric(obs[,ycoordcolnum])

xy <- obs[,c(xcoordcolnum,ycoordcolnum)]
xy <-as.list(xy)
#xy <- obs[,c(xcoordcolnum,ycoordcolnum)]
#xy <-as.list(xy)
#obs[,x] <- as.numeric(obs[,x])
#obs[,ycoordcolnum] <- as.numeric(obs[,ycoordcolnum])
#f <-paste0(getwd(),"/output/Sen2/used_indices.txt")

dummy <- readOGR("E:/Marvin/NP_Boundary.shp")
dummy@proj4string@projargs
#if (is.null(projObs) == TRUE){
#  #creating a spatioalpointdataframe
#  spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
#                                 proj4string = CRS(rs_list[[1]]@crs@projargs))
#  print("Spatialdataframe is created")
#  head(spdf)
#  print("POSSIBLE ERROR: Please check the outwritten
#DataFrame, could be with NAs")
#  Print("SOlution: check your coordinates in the observation DF and
#give the correct Projection to the projOBS argument")
#} else{
#creating a spatioalpointdataframe
spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                               proj4string = CRS(dummy@proj4string@projargs))
#  print("Spatialdataframe is created")
#  head(spdf)
#}
#dummy <- readOGR("/Volumes/MarvinLaCie/Marvin/NP_Boundary.shp")
#transforming into correct projection
#r_crs<- rs_list[[1]]@crs
#dummy <- sp::spTransform(dummy, CRS("+proj=utm +zone=32 +datum=WGS84
#+units=m +no_defs"))
spdf <- sp::spTransform(spdf, CRS("+proj=utm +zone=32 +datum=WGS84
                                  +units=m +no_defs"))

#creating correct rasternames
names(param)
#initialize progressbar
pb = txtProgressBar(min = 0, max = nlayers(param), initial = 0, style = 3)

#extracting rasterdata
for (i in 1:nlayers(param)){
  setTxtProgressBar(pb,i)
  #extracting raster values
  print(paste0("Starting extracting raster data for observation points: "
               , Sys.time()))
  vx_rs <- velox::velox(param[[i]])
  df <- as.data.frame(vx_rs$extract_points(spdf))
  colnames(df) <-names(param[[i]])
  print(paste0("Finished extracting raster data for observation points: "
               , Sys.time()))
  obs <- cbind(obs,df)
}

#write data
writeRaster(param, paste0("E:/Marvin/BB_rf/output/new/3mod/topo/",
                          "paramet_data3.tif"))
write.csv2(obs,file = paste0("E:/Marvin/BB_rf/output/new/3mod/topo/",
                             "paramet_data3.csv"))
obs_test<- read.csv2(paste0("E:/Marvin/BB_rf/output/new/topo",
                            "/paramet_data2.csv"))


##########END Creation of and extraction Parametersets#########


########## Create Modelinput Scenario 2##########
#---
# title: "Modelinput Scenario 2"
# Author: Marvin Müsgen
# Description: In this Skript the Parameters are collected for the model of
#scenario 1
# for the execution on the workstation
# After Adaption this works also for scenario 1
#---


paramdata <- read.csv2("E:/Marvin/BB_rf/output/new/3mod/topo/paramet_data3.csv")
sen2data <- list.files("E:/Marvin/BB_rf/output/new/3mod/Sen_2/",
                       pattern = ".csv", full.names = TRUE)
#sen2data<- sen2data[-87]
#sen2data<- sen2data[-1]
#sen2data<- sen2data[-1]
test <- read.csv2("E:/Marvin/BB_rf/output/new/3mod/Sen_1_2/
                  Ascending/_Extracted_data_32018-04-012018-09-30.csv", sep=",")
#test <- read.csv2(sen2data[74], sep =",")
#sen2data_whole_period<- sen2data[1:12]

for(i in 1:length(sen2data)){
  x<- sen2data[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  paramdata <- cbind(paramdata, df)
}
print("loaded param")
for(i in 1:length(sen2data_whole_period)){
  x<- sen2data_whole_period[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  paramdata <- cbind(paramdata, df)
}
print("loaded Sen2")
#sen2data_monthly <- sen2data[13:length(sen2data)]
#sen2data[1,c(2:5)]
sen1ascdata <- list.files("E:/Marvin/BB_rf/output/new/
                          3mod/Sen_1_2/Ascending",
                          pattern = ".csv", full.names = TRUE)
sen1ascdata<- sen1ascdata[-1]
for(i in 1:length(sen1ascdata)){
  x<- sen1ascdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_Asc")
  paramdata <- cbind(paramdata, df)
}

print("loaded ascending")
sen1descdata <- list.files("E:/Marvin/BB_rf/output/
                           new/3mod/Sen_1_2/Descending", pattern = ".csv",
                           full.names = TRUE)
sen1descdata <- sen1descdata[-1]
for(i in 1:length(sen1descdata)){
  x<- sen1descdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_desc")
  paramdata <- cbind(paramdata, df)
}
print("loaded descending")
write.csv(paramdata , "E:/Marvin/BB_rf/output/new/
          3mod/all_inputs_model_3var_sep.csv")
ncol(paramdata)
dem<- raster::raster(paste0(getwd(),"/input/observations/dem/dem_10.tif"))
library(red)
north<- red::raster.north(dem)
spdftrans<- sp::spTransform(spdf,crs(north))
vx_rs <- velox::velox(north)
df <- as.data.frame(vx_rs$extract_points(spdftrans))
km <- kmeans(df, centers = 4) # clustering points into locations for spacevar
#plot(obs$x.coord, obs$y.coord, col = km$cluster, pch = 20) # plot cluster
obs <- cbind(obs,km$cluster) #add spacevar column
colnames(obs)[ncol(obs)] <- "cluster" # rename spacevar column
cluster<- cbind(obs,data) #create traindata
cluster<- obs[,5]
all<- read.csv("E:/Marvin/BB_rf/output/new/3mod/all_inputs_model_3var_sep.csv")
all <- cbind(all, cluster)
write.csv(all , "E:/Marvin/BB_rf/output/new/3mod/all_inputs_model_3var_sep.csv")

##########End  Create Modelinput Scenario 2##########
