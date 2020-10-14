########## Preparing Data ##########
#---
# title: "Preparing Data"
# Author: Marvin Müsgen
# Description: The observation data which is in polygon format is prepared for the analysis process. Points from
#              the polygon format will be created for the time of interest,
#---

#set working directory
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf")
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
core_X_2018 <- core[which(core$Jahr_2 <= year_obs),] # only correct time

develop <- develop[which(grepl("?", develop$Fraglich) == FALSE), ] # only correct trees
develop <- develop[which(grepl("DFE; eingeschlagen", develop$BEMERKUNG) == FALSE), ] # only standing tress
develop_X_2018 <- develop[which(develop$Jahr_2 <= year_obs),] # only correct time

core_x <- core_X_2018[, 25] # only year
develop_x <- develop_X_2018[,22] # only time

#binding observations together
observations <- rbind(core_x, develop_x, makeUniqueIDs = TRUE)

#transform in correct projection
observations <- spTransform(observations, CRSobj = area@proj4string@projargs)

#creating sampling point
Sample_x_2018 <- spsample(observations, 5000, type = "random")

#transform into correct projection
Sample_x_2018 <- spTransform(Sample_x_2018, CRSobj = area@proj4string@projargs)

#create healthy sample polygon
not_BB_poly <- gDifference(area, observations) # cutting infestation area from hgealthy area
not_BB_poly <- as(not_BB_poly, "SpatialPolygonsDataFrame") # convert to spdf
#writeOGR(not_BB_poly, dsn= paste0(getwd(), "/input/observations"), driver= "ESRI Shapefile", layer = "cuttet_area_x_2018") # writing out

#Sample healthy observation
Sample_x_2018_healthy <- spsample(not_BB_poly, 5000, type = "random")
Sample_x_2018_healthy <- spTransform(Sample_x_2018_healthy, CRSobj = area@proj4string@projargs)

# convert to spdf
Sample_x_2018_healthy <- as(Sample_x_2018_healthy, "SpatialPointsDataFrame")
Sample_x_2018 <- as(Sample_x_2018, "SpatialPointsDataframe")

#create healthy observation dataframe
df <- data.frame(1:5000)
colnames(df) <- "observations"
df$observations <- 0 # no bark beetle attack
df <- cbind(df,Sample_x_2018_healthy@coords)

#create infested observation dataframe
df_not_healthy <- data.frame(1:5000)
colnames(df_not_healthy) <- "observations"
df_not_healthy$observations <- 1 # bark beetle attack
df_not_healthy <- cbind(df_not_healthy, Sample_x_2018@coords)

#create observation data frame
obs <- rbind(df, df_not_healthy)

#write out observation
write.csv(obs, file=paste0(getwd(), "/input/observations/Befall_gesamt/barkbeetle_obs_x_to_2018_2class"))
