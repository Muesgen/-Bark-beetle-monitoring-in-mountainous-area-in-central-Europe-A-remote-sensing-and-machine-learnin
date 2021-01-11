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
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf") # working directory
input_dir <- "/output/" # input data dir
funfold= paste0(getwd(),"/functions") # function folder directory
used_indices <- "used_indices.txt" # used Sen2 vegetation Indices
used_pol <- c("VH","VV","ratio") # used Sentinel 1 polarizations
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
files_2<- list.files(paste0(getwd(),input_dir,"Sen2/pca/"), pattern = "PCA_Extracted")

### reading Sentinel 2 Data ###
#this elaborate process prevents errors in the training data, if the data is stored in two or more csv
if(length(files_2) == 1){
  df <- read.csv(paste0(getwd(),input_dir,"Sen2/pca/",files_2))
}else{
  for (i in 1:length(files_2)){
    if (i == 1){
      df <- read.csv(paste0(getwd(),input_dir,"Sen2/pca/",files_2[i]))
  }
  else if(i == length(files_2)){
      temp <- read.csv(paste0(getwd(),input_dir,"Sen2/pca/",files_2[i]))
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
      temp <- read.csv(paste0(getwd(),input_dir,"Sen2/",files_2[i]))
      df <- cbind(df,temp)
    }
  }
}
#listing Sentinel 1 files

files_1a <- list.files(paste0(getwd(),input_dir,"Sen1/Ascending/"), pattern = paste0("observation_stats",startdate,enddate))
files_1d <- list.files(paste0(getwd(),input_dir,"Sen1/Descending/"), pattern = paste0("observation_stats",startdate,enddate))

### reading Sentinel 1 Data ###
#this elaborate process prevents errors in the training data
for (x in 1:length(files_1a)){
  if (x == 1){
    dfA <- read.csv(paste0(getwd(),input_dir,"Sen1/Ascending/",files_1a[x]))
    dfD <- read.csv(paste0(getwd(),input_dir,"Sen1/Descending/",files_1d[x]))
    colnames(dfA) <- paste0(colnames(dfA),"_ASCEND")
    colnames(dfD) <- paste0(colnames(dfD),"_DESCEND")
    df1 <- cbind(dfA, dfD)
  }
  else if(x == length(files_1a)){
    dfA <- read.csv(paste0(getwd(),input_dir,"Sen1/Ascending/",files_1a[x]))
    dfD <- read.csv(paste0(getwd(),input_dir,"Sen1/Descending/",files_1d[x]))
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
    dfA <- read.csv(paste0(getwd(),input_dir,"Sen1/Ascending/",files_1a[x]))
    dfD <- read.csv(paste0(getwd(),input_dir,"Sen1/Descending/",files_1d[x]))
    colnames(dfA) <- paste0(colnames(dfA),"_ASCEND")
    colnames(dfD) <- paste0(colnames(dfD),"_DESCEND")
    temp <- cbind(dfA, dfD)
    df1 <- cbind(df1, temp)
  }
}

#creating training data
data <-  cbind(data1,data2)
obs <- read.csv(paste0(getwd(),"/input/observations/", observations))
#obs[,5:ncol(obs)] <- NULL
km <- kmeans(cbind(obs$x.coord, obs$y.coord), centers = 3)
plot(obs$x.coord, obs$y.coord, col = km$cluster, pch = 20)
obs <- cbind(obs,km$cluster)
colnames(obs)[ncol(obs)] <- "cluster"
traindat <- cbind(obs,data)

#Split data 70/30; save 30% for prediction and validation
print(paste0("Start model: ", Sys.time()))
set.seed(1993)
traindat$observatio <- as.factor(traindat$observatio)
traindat$observatio <- make.names(traindat$observatio)
index = caret::createDataPartition(y = traindat$observatio, p = .70, list = FALSE)
pred = traindat[-index, ]
resp = traindat[-index, which(colnames(traindat) == observation)]
ind = CAST::CreateSpacetimeFolds(pred, spacevar = "cluster", k = 3)
trainctl = caret::trainControl(method = "cv", number = 3, classProbs = TRUE, 
                               index = ind$index, indexOut = ind$indexOut, savePredictions = TRUE, returnResamp = "all")
l = ncol(obs)
pred <- pred[, -(1:l)]
cl = autoStopCluster(parallel::makeCluster(parallel::detectCores()-1))
doParallel::registerDoParallel(cl)
mod1 = CAST::ffs(predictors = pred, response = resp, method = "rf", importance = TRUE, trControl = trainctl, 
                 metric = "Accuracy")
#mod1 = caret::train(pred, resp, method = "rf", trControl = trainctl)
saveRDS(mod1, paste0(getwd(),"/output/models/global.rds"))
print(paste0("Finished model: ", Sys.time()))
