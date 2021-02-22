######### creating randomForest Model#####
# This script creates a random forest model with a forward feature selection and a leave location out sample strategie

###FFS###
# forward feature selection preserve the effect of outliners and define the perfect parameter set
# Parameter which does not increase the model explaination are not picked for the model
# A 5 fold crossvalidation is used for the model validation

###Leave location out ####
#The leave location out sample strategie preserve the effect of spatial autocorrelation

###randForest###
# In the current research for spatio and spatio temporal ecological modelling the rF machine learning alghorithm 
# is considered the most performing. You find some interesting stuff here:

#Meyer, H., Reudenbach, C., Hengl, T., Katurji, M., Nauss, T. (2018): Improving performance of spatio-temporal machine learning models using forward feature selection and target-oriented validation. Environmental Modelling & Software, 101, 1-9.
#Meyer, H., Reudenbach, C., Wöllauer, S., Nauss, T. (2019): Importance of spatial predictor variable selection in machine learning applications - Moving from data reproduction to spatial prediction. Ecological Modelling. 411.
#The talk from the OpenGeoHub summer school 2019 on spatial validation and variable selection: https://www.youtube.com/watch?v=mkHlmYEzsVQ.
#Meyer, H., Pebesma, E. (2020): Predicting into unknown space? Estimating the area of applicability of spatial prediction models. http://arxiv.org/abs/2005.07939
#Tutorial (https://youtu.be/EyP04zLe9qo) and Lecture(https://youtu.be/OoNH6Nl-X2s) recording from OpenGeoHub summer school 2020 on the area of applicability.

#Set Paths
setwd("E:/Marvin/BB_rf") # working directory
input_dir <- "/output/" # input data dir
funfold= paste0(getwd(),"/functions") # function folder directory
used_indices <- "used_indices.txt" # used Sen2 vegetation Indices
used_pol <- c("VH","VV") # used Sentinel 1 polarizations
used_orbit <- c("Ascending", "Descending")

#Set Variables
startdate <- "2018-04-01" # start date of interes
enddate <- "2018-09-30" # end date of interest
observations <- "o_total.csv" # observation file
observation <- "observatio" #observation column
pckgs <- c("stringr","CAST", "caret", "doParallel","randomForest","sp","rgeos") # needed Packages
#loading libararys and functions
source(paste0(funfold,"/autostopcluster.R"))
source(paste0(funfold,"/check_library.R"))

#loading librarys
check_library(pckgs)

###loading vegetation indices for checking
singleString <- paste(readLines(paste0(getwd(),input_dir,"Sen2/",used_indices)), collapse=" ")
veg_names <- unlist(strsplit(singleString, " "))

#listing Sentinel 2 Data
files_2<- list.files(paste0(getwd(),"/modelinput2/Sen2/"), pattern = "observation")

### reading Sentinel 2 Data ###
#this elaborate process prevents errors in the training data, if the data is stored in two or more csv
if(length(files_2) == 1){
  data2 <- read.csv(paste0(getwd(),"/modelinput2/Sen2/",files_2))
}else{
  for (i in 1:length(files_2)){
    if (i == 1){
      df <- read.csv(paste0(getwd(),"/modelinput2/Sen2/",files_2[i]))
      df <- df[,c(2,3,4,5)]
    }
    else if(i == length(files_2)){
      temp <- read.csv(paste0(getwd(),"/modelinput2/Sen2/",files_2[i]))
      temp <- temp[,c(2,3,4,5)]
      df <- cbind(df,temp)
      for (j in 1:length(veg_names)){
        if(j == 1){
          data2 <- df[,grepl(veg_names[j], colnames(df))]
        }
        else{
          veg_temp <- df[,grepl(veg_names[j], colnames(df))]
          data2 <- cbind(data2, veg_temp)
        }
      }
    }
    else{
      temp <- read.csv(paste0(getwd(),"/modelinput2/Sen2/",files_2[i]))
      temp <- temp[,c(2,3,4,5)]
      df <- cbind(df,temp)
    }
  }
}
#listing Sentinel 1 files

files_1a <- list.files(paste0(getwd(),input_dir,"Sen1_3/Ascending/"), pattern = "observation_stats")
files_1d <- list.files(paste0(getwd(),input_dir,"Sen1_3/Descending/"), pattern = "observation_stats")

### reading Sentinel 1 Data ###
#this elaborate process prevents errors in the training data
if(length(files_1a) == 1 & length(files_1d) ==1){
  dfA <- read.csv(paste0(getwd(),"/modelinput2/Ascending/",files_1a))
  dfD <- read.csv(paste0(getwd(),"/modelinput2/Descending/",files_1d))
  colnames(dfA) <- paste0(colnames(dfA),"_ASCEND")
  colnames(dfD) <- paste0(colnames(dfD),"_DESCEND")
  data1 <- cbind(dfA, dfD)
}else{
  for (x in 1:length(files_1a)){
    if (x == 1){
      dfA <- read.csv(paste0(getwd(),"/modelinput2/Ascending/",files_1a[x]))
      dfD <- read.csv(paste0(getwd(),"/modelinput2/Descending/",files_1d[x]))
      colnames(dfA) <- paste0(colnames(dfA),"_ASCEND")
      colnames(dfD) <- paste0(colnames(dfD),"_DESCEND")
      df1 <- cbind(dfA, dfD)
    }
    else if(x == length(files_1a)){
      dfA <- read.csv(paste0(getwd(),"/modelinput2/Ascending/",files_1a[x]))
      dfD <- read.csv(paste0(getwd(),"/modelinput2/Descending/",files_1d[x]))
      colnames(dfA) <- paste0(colnames(dfA),"_ASCEND")
      colnames(dfD) <- paste0(colnames(dfD),"_DESCEND")
      temp <- cbind(dfA, dfD)
      df1 <- cbind(df1, temp)
      for (y in 1:length(used_pol)){
        if(y == 1){
          data1 <- df1[,grepl(used_pol[y], colnames(df1))]
        }
        else{
          pol_temp <- df1[,grepl(used_pol[y], colnames(df1))]
          data1 <- cbind(data1, pol_temp)
        }
      }
    }
    else{
      dfA <- read.csv(paste0(getwd(),"/modelinput2/Ascending/",files_1a[x]))
      dfD <- read.csv(paste0(getwd(),"/modelinput2/Descending/",files_1d[x]))
      colnames(dfA) <- paste0(colnames(dfA),"_ASCEND")
      colnames(dfD) <- paste0(colnames(dfD),"_DESCEND")
      temp <- cbind(dfA, dfD)
      df1 <- cbind(df1, temp)
    }
  }
}

#creating training data
start <- gsub("-",".",startdate) # create Filter 
data2 <- data2[,which(grepl("2018", colnames(data2)))] # Filtering data for time period of interest
data1 <- data1[,which(grepl("2018", colnames(data1)))] # Filtering data for time period of interest
substring(names(climate_preproc),15, nchar(names(climate_preproc)))
data3 <- read.csv(paste0(getwd(),"/modelinput2/paramet_data3.csv"), sep = ";")
colnames(data3)[(ncol(data3)-35):ncol(data3)] <- substring(names(climate_preproc),15, nchar(names(climate_preproc)))
data <-  cbind(data1,data2)
data <- cbind(data,data3)
obs <- read.csv("E:/Marvin/BB_rf/input/observations/new/barkbeetle_obs_x_to_2018_3class.csv")
#defining coordinates, Lat = Y Long = X
obs[,3] <- as.numeric(as.character(obs[,3]))
obs[,4] <- as.numeric(as.character(obs[,4]))
xy <- obs[,c(3,4)]
xy <-as.list(xy)
#f <-paste0(getwd(),"/output/Sen2/used_indices.txt")
dummy <- readOGR("E:/Marvin/NP_Boundary.shp")
dummy@proj4string@projargs
if (is.null(projObs) == TRUE){
  #creating a spatioalpointdataframe
  spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                                 proj4string = CRS(rs_list[[1]]@crs@projargs))
  print("Spatialdataframe is created")
  head(spdf)
  print("POSSIBLE ERROR: Please check the outwritten DataFrame, could be with NAs")
  Print("SOlution: check your coordinates in the observation DF and give the correct Projection to the projOBS argument")
} else{
  #creating a spatioalpointdataframe
  spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                                 proj4string = CRS(dummy@proj4string@projargs))
  head(spdf)
}
spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                               proj4string = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))

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
#Split data 70/30; save 30% for prediction and validation
print(paste0("Start model: ", Sys.time()))
set.seed(1993)
traindat$observations <- as.factor(traindat$observations)
traindat$observations <- make.names(traindat$observations)
index <- caret::createDataPartition(y = traindat$observations, p = .70, list = FALSE)
pred <- traindat[-index, ]
resp <- traindat[-index, which(colnames(traindat) == observations)]
ind <- CAST::CreateSpacetimeFolds(pred, spacevar = "cluster", k = 4)
trainctl <- caret::trainControl(method = "cv", number = 4, classProbs = TRUE, 
                                index = ind$index, indexOut = ind$indexOut, savePredictions = TRUE, returnResamp = "all")
l <- ncol(obs)
pred <- pred[, -(1:l)]
#attention na.roughfix, what na include strategie to choose?
pred <- na.roughfix(pred)
cl <- autoStopCluster(parallel::makeCluster(parallel::detectCores()-1))
doParallel::registerDoParallel(cl)
mod1 <- CAST::ffs(predictors = pred, response = resp, method = "rf", importance = TRUE, trControl = trainctl, 
                  metric = "Accuracy")
#mod2 <- CAST::ffs(predictors = pred, response = resp, method = "rf", importance = TRUE, trControl = trainctl, 
#                  metric = "Kappa")
#mod3 <- CAST::ffs(predictors = pred, response = resp, method = "rf", importance = TRUE, trControl = trainctl, 
#                  metric = "RMSE")
#mod4 <- CAST::ffs(predictors = pred, response = resp, method = "rf", importance = TRUE, trControl = trainctl, 
#                  metric = "Accuracy", na.action = na.roughfix)
##mod1 = caret::train(pred, resp, method = "rf", trControl = trainctl)
saveRDS(mod1, paste0(getwd(),"/results/ALL_mod1_3variables_2018.rds"))
print(paste0("Finished model: ", Sys.time()))
mod1 <- readRDS(paste0(getwd(),"/output/ALL_mod1_3variables_2018.rds"))
mod2 <- readRDS(paste0(getwd(),"/output/models/global_pca_mod2.rds"))

#plot varimportance
plot(caret::varImp(mod2))

#predict model
predictmod2 <- predict(mod2, traindat[index,6:ncol(traindat)])

#list predictor raster
Sen2files <- list.files(paste0(getwd(),"/output/Sen2/pca/"), pattern = "*.tif", full.names = TRUE)	
Sen1Afiles <- list.files(paste0(getwd(),"/output/Sen1/Ascending/pca/"), pattern = "*.tif", full.names = TRUE)	
Sen1Dfiles <- list.files(paste0(getwd(),"/output/Sen1/Descending/pca/"), pattern = "*.tif", full.names = TRUE)
Sen1files <- append(Sen1Dfiles, Sen1Afiles)

#stack prediction stack
predstack1A<- stack(Sen1Afiles)
predstack1B<- stack(Sen1Dfiles)
predstack2 <- stack(Sen2files)

#reproject
predstack2 <- projectRaster(predstack2, predstack1B)
predstack1A <- projectRaster(predstack1A, predstack1B)

#stack into on predictionstack
predstack <- stack(predstack2, predstack1B)
predstack <- stack(predstack, predstack1A)

#test and replace if variable names do not fit to model
names(predstack)[str_detect(names(predstack), pattern = "_asc") == TRUE] <- paste0(names(predstack)[str_detect(names(predstack), pattern = "_asc") == TRUE], "_asc")
names(predstack)[str_detect(names(predstack), pattern = "_dsc") == TRUE] <- paste0(names(predstack)[str_detect(names(predstack), pattern = "_dsc") == TRUE], "_dsc")

#predict
areapred4 <- raster::predict(predstack, mod4) # predict on stack
area<- readOGR("/Volumes/MarvinLaCie/Marvin/BB_rf/input/observations/NP_Boundary.shp")
plot(areapred2)
plot(area, add = TRUE)
points <- readOGR("/Volumes/MarvinLaCie/Marvin/observation.shp")
raster::writeRaster(areapred2[[1]], paste0(getwd(), "/results/areapred2.tif"))

#model statistics
mod4
#mod2$pred
mod4$finalModel
mod4$selectedvars
mod4$times

####Test for whole predictor stack

#list predictor raster
Sen2files <- list.files(paste0(getwd(),"/output/Sen2/pca/"), pattern = "*.tif", full.names = TRUE)	
Sen1Afiles <- list.files(paste0(getwd(),"/output/Sen1/Ascending/pca/"), pattern = "*2018-09-30.tif", full.names = TRUE)	
Sen1Dfiles <- list.files(paste0(getwd(),"/output/Sen1/Descending/pca/"), pattern = "*2018-09-30.tif", full.names = TRUE)
Sen1files <- append(Sen1Dfiles, Sen1Afiles)
predrasterlist <- append(predrasterlist, Sen1Dfiles)

#stack prediction stack
predstack1A<- stack(Sen1Afiles)
predstack1B<- stack(Sen1Dfiles)
predstack2 <- stack(Sen2files)

#reproject
predstack2 <- projectRaster(predstack2, predstack1B)
predstack1A <- projectRaster(predstack1A, predstack1B)

#stack into on predictionstack
predstack <- stack(predstack2, predstack1B)
predstack <- stack(predstack, predstack1A)

#test and replace if variable names do not fit to model
names(predstack)[str_detect(names(predstack), pattern = "_asc") == TRUE] <- paste0(names(predstack)[str_detect(names(predstack), pattern = "_asc") == TRUE], "_asc")
names(predstack)[str_detect(names(predstack), pattern = "_dsc") == TRUE] <- paste0(names(predstack)[str_detect(names(predstack), pattern = "_dsc") == TRUE], "_dsc")

#predict
areapred4 <- raster::predict(predstack, mod4) # predict on stack
area<- readOGR("/Volumes/MarvinLaCie/Marvin/BB_rf/input/observations/NP_Boundary.shp")
plot(areapred2)
plot(area, add = TRUE)
points <- readOGR("/Volumes/MarvinLaCie/Marvin/observation.shp")
raster::writeRaster(areapred2[[1]], paste0(getwd(), "/results/areapred2.tif"))


######################################################
data(iris)
iris.na <- iris
set.seed(111)
## artificially drop some data values.
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
set.seed(222)
iris.imputed <- rfImpute(Species ~ ., iris.na)
set.seed(333)
iris.rf <- randomForest(Species ~ ., iris.imputed)
print(iris.rf)

data(iris)
iris.na <- iris
set.seed(111)
## artificially drop some data values.
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
iris.roughfix <- na.roughfix(iris.na)
iris.narf <- randomForest(Species ~ ., iris.na, na.action=na.roughfix)
print(iris.narf)
pred.roughfix <- na.roughfix(pred)
