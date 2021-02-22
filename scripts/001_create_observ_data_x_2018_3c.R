########## Preparing Data ##########
#---
# title: "Preparing Data"
# Author: Marvin Müsgen
# Description: The observation data which is in polygon format is prepared for the analysis process. Points from
#              the polygon format will be created for the time of interest,
#---
getwd()
#set working directory
setwd("E:/Marvin/BB_rf")
Input_dir <- "/input/observations/Befall_gesamt/"
develop_zone <- "Gesamtbefall_Entwicklungszone.shp"
core_zone <- "/Gesamtbefall_Kernzone.shp"
funfold <- funfold <-  paste0(getwd(),"/functions") #path to function folder
area <- paste0(getwd(), "/input/observations/NP_Boundary.shp")

#Set variables 
pckgs <- c("sp", "rgdal", "raster", "rgeos")
year_obs <- 2018

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
core_X_2018 <- core[which(as.numeric(as.character(core$Jahr_2)) <= year_obs),] # only correct time

develop <- develop[which(grepl("?", develop$Fraglich) == FALSE), ] # only correct trees
develop <- develop[which(grepl("DFE; eingeschlagen", develop$BEMERKUNG) == FALSE), ] # only standing tress
develop_X_2018 <- develop[which(as.numeric(as.character(develop$Jahr_2)) <= year_obs),] # only correct time

core_x <- core_X_2018[, 25] # only year
develop_x <- develop_X_2018[,22] # only time

#binding observations together
observations <- rbind(core_x, develop_x, makeUniqueIDs = TRUE)

#transform in correct projection
observations <- spTransform(observations, CRSobj = area@proj4string@projargs)

#creating sampling point
Sample_older_2018 <- spsample(observations[which(as.numeric(as.character(observations$Jahr_2)) < 2018),], 3333, type = "random")
Sample_new_2018 <- spsample(observations[which(as.numeric(as.character(observations$Jahr_2)) == 2018 ),], 3333, type = "random")
#transform into correct projection
Sample_older_2018 <- spTransform(Sample_older_2018, CRSobj = area@proj4string@projargs)
sample_new_2018 <- spTransform(Sample_new_2018, CRSobj = area@proj4string@projargs)
#create healthy sample polygon
not_BB_poly <- gDifference(area, observations) # cutting infestation area from hgealthy area
not_BB_poly <- as(not_BB_poly, "SpatialPolygonsDataFrame") # convert to spdf
#writeOGR(not_BB_poly, dsn= paste0(getwd(), "/input/observations"), driver= "ESRI Shapefile", layer = "cuttet_area_x_2018") # writing out

#Sample healthy observation
Sample_healthy_2018 <- spsample(not_BB_poly, 3333, type = "random")
Sample_healthy_2018 <- spTransform(Sample_healthy_2018, CRSobj = area@proj4string@projargs)

# convert to spdf
Sample_healthy_2018 <- as(Sample_healthy_2018, "SpatialPointsDataFrame")
Sample_older_2018 <- as(Sample_older_2018, "SpatialPointsDataframe")
sample_new_2018 <- as(Sample_new_2018, "SpatialPointsDataframe")
#create healthy observation dataframe
df <- data.frame(1:3333)
colnames(df) <- "observations"
df$observations <- 0 # no bark beetle attack
df <- cbind(df,Sample_healthy_2018@coords)

#create infested observation dataframe before 2018
df_old_2018 <- data.frame(1:3333)
colnames(df_old_2018) <- "observations"
df_old_2018$observations <- 1 # bark beetle attack
df_old_2018 <- cbind(df_old_2018, Sample_older_2018@coords)

#create infested observation dataframe new 2018
df_new_2018 <- data.frame(1:3333)
colnames(df_new_2018) <- "observations"
df_new_2018$observations <- 2 # bark beetle attack
df_new_2018 <- cbind(df_new_2018, Sample_new_2018@coords)

#create observation data frame
obs <- rbind(df, df_old_2018)
obs <- rbind(obs, df_new_2018)
#write out observation
write.csv(obs, file=paste0(getwd(), "/input/observations/new/barkbeetle_obs_x_to_2018_3class.csv"))
