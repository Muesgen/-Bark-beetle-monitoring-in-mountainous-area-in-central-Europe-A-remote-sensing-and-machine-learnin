########## Preparing Observation Data ##########
#---
# title: "Preparing Observation Data"
# Author: Marvin Müsgen
# Description: The observation data which is in polygon format is prepared for the analysis process. Points from
#              the polygon format will be created for the time of interest,
#---

#set working directory
setwd("E:/Marvin/BB_rf")
Input_dir <- "/input/observations/Befall_gesamt/"
develop_zone <- "Gesamtbefall_Entwicklungszone.shp"
core_zone <- "/Gesamtbefall_Kernzone.shp"
funfold <- funfold <-  paste0(getwd(),"/functions") #path to function folder
area <- paste0(getwd(), "/input/observations/NP_Boundary.shp")
  
#Set variables 
pckgs <- c("sp", "rgdal", "raster", "rgeos")
year_obs <- "2018"

# loading functions
source(paste0(funfold,"/check_library.R"))

#loading librarys
check_library(pckgs)

#read input data
core <- readOGR(paste0(getwd(), Input_dir, core_zone))
develop <- readOGR(paste0(getwd(), Input_dir, develop_zone))
area <- readOGR(area)

#filter input_data
core <- core[which(grepl("DFE; eingeschlagen", core$BEMERKUNG) == FALSE), ] # only standing trees
core <- core[which(grepl("Baumart?", core$Fraglich) == FALSE), ] # only correct trees
#core_X_2019 <- core[which(core$Jahr_2 >= year_obs),] # only correct time
core_x_2018 <- core

develop <- develop[which(grepl("?", develop$Fraglich) == FALSE), ] # only correct trees
develop <- develop[which(grepl("DFE; eingeschlagen", develop$BEMERKUNG) == FALSE), ] # only standing tress
#develop_X_2019 <- develop[which(develop$Jahr_2 >= year_obs),] # only correct time
develop_X_2018 <- develop 

#remove columns
core_x <- core_x_2019[, 25] # only year
develop_x <- develop_X_2019[,22] # only time

#binding observations together
observations <- rbind(core_x, develop_x, makeUniqueIDs = TRUE)

#transform in correct projection
observations <- spTransform(observations, CRSobj = area@proj4string@projargs)

#creating sampling point data
Sample_old <- spsample(observations[which(observations$Jahr_2 == 2017),], 3333, type = "random")
Sample_2018 <- spsample(observations[which(observations$Jahr_2 == 2018),], 3333, type = "random")

#transform into correct projection
Sample_2018 <- spTransform(Sample_2018, CRSobj = area@proj4string@projargs)
Sample_old <- spTransform(Sample_old, CRSobj = area@proj4string@projargs)

#create healthy sample polygon
not_BB_poly <- gDifference(area, observations) # cutting infestation area from hgealthy area
not_BB_poly <- as(not_BB_poly, "SpatialPolygonsDataFrame") # convert to spdf
writeOGR(not_BB_poly, dsn= paste0(getwd(), "/input/observations"), driver= "ESRI Shapefile", layer = "cuttet_area_x_2018") # writing out

#Sample healthy observation
Sample_x_healthy <- spsample(not_BB_poly, 3333, type = "random")

#tramsform into correct projection
Sample_x_healthy <- spTransform(Sample_x_healthy, CRSobj = area@proj4string@projargs)

# convert to spdf
#Sample_x_healthy <- as(Sample_x_healthy, "SpatialPointsDataFrame")
#Sample_2019 <- as(Sample_2019, "SpatialPointsDataframe")
#Sample_old <- as(Sample_old, "SpatialPointsDataframe")

#create healthy observation dataframe
df <- data.frame(1:3333)
colnames(df) <- "observations"
df$observations <- 0 # no bark beetle attack
df <- cbind(df,Sample_x_healthy@coords)

#create infested observation dataframe older than 1 year
df_not_healthy <- data.frame(1:3333)
colnames(df_not_healthy) <- "observations"
df_not_healthy$observations <- 1 # bark beetle attack recognized a year later or more
df_not_healthy <- cbind(df_not_healthy,Sample_old@coords)

#create observation data frame
obs <- rbind(df, df_not_healthy)

# create infested observation dataframe from actual year
df_new <- data.frame(1:3333)
colnames(df_new) <- "observations"
df_new$observations <- 2 # bark beetle attack recognized 2019
df_new <- cbind(df_new, Sample_2018@coords)

#create observation data frame
obs <- rbind(obs, df_new)

#write out observation
write.csv(obs, file=paste0(getwd(), "/input/observations/Befall_gesamt/barkbeetle_obs_2018_3class.csv"))

