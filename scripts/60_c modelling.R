# This script creates a random forest model with a forward feature selection
#and a leave location out sample strategie

###FFS###
# forward feature selection preserve the effect of outliners and define
#the perfect parameter set
# Parameter which does not increase the model explaination are not picked
#for the model
# A 5 fold crossvalidation is used for the model validation

###Leave location out ####
#The leave location out sample strategie preserve the effect of spatial
#autocorrelation

###randForest###
# In the current research for spatio and spatio temporal ecological modelling
#the rF machine learning alghorithm
# is considered the most performing. You find some interesting stuff here:

#Meyer, H., Reudenbach, C., Hengl, T., Katurji, M., Nauss, T. (2018):
#Improving performance of spatio-temporal machine learning models using
#forward feature selection and target-oriented validation. Environmental
#Modelling & Software, 101, 1-9.
#Meyer, H., Reudenbach, C., Wöllauer, S., Nauss, T. (2019):
#Importance of spatial predictor variable selection in machine learning
#applications - Moving from data reproduction to spatial prediction.
#Ecological Modelling. 411.
#The talk from the OpenGeoHub summer school 2019 on spatial validation
#and variable selection: https://www.youtube.com/watch?v=mkHlmYEzsVQ.
#Meyer, H., Pebesma, E. (2020): Predicting into unknown space? Estimating
#the area of applicability of spatial prediction models.
#http://arxiv.org/abs/2005.07939
#Tutorial (https://youtu.be/EyP04zLe9qo) and Lecture
#(https://youtu.be/OoNH6Nl-X2s) recording from OpenGeoHub
#summer school 2020 on the area of applicability.


########## Model Scenario 2 ###################

#---
# title: "Model Scenario 2"
# Author: Marvin Müsgen
# Description: In this Skript the Model for the second 2 is calculated.
# used model is random forest. Forward feature selection and spatial
#validation are used
#---


###create Model
#loading library
check_library <- function(pck){
  for (i in 1:length(pck)){
    if(pck[i] %in% rownames(installed.packages()) == FALSE) {
      install.packages(pck[i])}
  }
  lapply(pck, require, character.only = TRUE)
}
pckgs <- c("stringr","CAST", "caret", "doParallel","randomForest","sp",
           "rgeos") # needed Packages
check_library(pckgs)

# Function to select on how many cores next function should be executed
autoStopCluster <- function(cl) {
  stopifnot(inherits(cl, "cluster"))
  env <- new.env()
  env$cluster <- cl
  attr(cl, "gcMe") <- env
  reg.finalizer(env, function(e) {
    message("Finalizing cluster ...")
    message(capture.output(print(e$cluster)))
    try(parallel::stopCluster(e$cluster), silent = FALSE)
    message("Finalizing cluster ... done")
  })
  cl
}
# reading data
traindat <- read.csv("E:/Marvin/BB_rf/output/new/3mod/
                     all_inputs_model_3var_sep.csv")

# prepare model input data
print(paste0("Start model: ", Sys.time()))
set.seed(1993)
traindat$observations <- as.factor(traindat$observations)
traindat$observations <- make.names(traindat$observations)
index <- caret::createDataPartition(y = traindat$observations,
                                    p = .70, list = FALSE)
pred <- traindat[-index, ]
#spatial strategy
resp <- traindat[-index, which(colnames(traindat) == "observations")]
ind <- CAST::CreateSpacetimeFolds(pred, spacevar = "cluster", k = 4)
trainctl <- caret::trainControl(method = "cv", number = 4,
                                classProbs = TRUE,
                                index = ind$index,
                                indexOut = ind$indexOut,
                                savePredictions = TRUE, returnResamp = "all")
l <- 5
pred <- pred[, -(1:7)]
pred <- pred[,-ncol(pred)]
#attention na.roughfix, what na include strategie to choose?
pred <- na.roughfix(pred)
cl <- autoStopCluster(parallel::makeCluster(parallel::detectCores()-1))

#create model
doParallel::registerDoParallel(cl)
mod1 <- CAST::ffs(predictors = pred, response = resp,
                  method = "rf", importance = TRUE, trControl = trainctl,
                  metric = "Accuracy")

saveRDS(mod1, paste0("E:/Marvin/BB_rf/results/
                     ALL_mod3_new_3variables_2018_2901.rds"))

##########End  Create Model Scenario 2##########


########## Model Scenario 1 ###################

#---
# title: "Model Scenario 1"
# Author: Marvin Müsgen
# Description: In this Skript the Model for the first scenario is calculated.
# used model is random forest. F
#orward feature selection and spatial validation are used
#---


###create Model

###loading library
check_library <- function(pck){
  for (i in 1:length(pck)){
    if(pck[i] %in% rownames(installed.packages()) == FALSE) {
      #install.packages(pck[i])}
  }
  lapply(pck, require, character.only = TRUE)
}
pckgs <- c("stringr","CAST", "caret", "doParallel","randomForest","sp","rgeos"
           ) # needed Packages
check_library(pckgs)

# Function to select on how many cores next function should be executed
autoStopCluster <- function(cl) {
  stopifnot(inherits(cl, "cluster"))
  env <- new.env()
  env$cluster <- cl
  attr(cl, "gcMe") <- env
  reg.finalizer(env, function(e) {
    message("Finalizing cluster ...")
    message(capture.output(print(e$cluster)))
    try(parallel::stopCluster(e$cluster), silent = FALSE)
    message("Finalizing cluster ... done")
  })
  cl
}
#reading data
traindat <- read.csv2("E:/Marvin/BB_rf/modeldata_2_variables.csv")
#prepare data
print(paste0("Start model: ", Sys.time()))
set.seed(1993)
traindat$observations <- as.factor(traindat$observations)
traindat$observations <- make.names(traindat$observations)
index <- caret::createDataPartition(y = traindat$observations,
                                    p = .70, list = FALSE)
pred <- traindat[-index, ]
resp <- traindat[-index, which(colnames(traindat) == "observations")]
#spatial validation
ind <- CAST::CreateSpacetimeFolds(pred, spacevar = "cluster", k = 4)
trainctl <- caret::trainControl(method = "cv", number = 4, classProbs = TRUE,
                                index = ind$index,
                                indexOut = ind$indexOut,
                                savePredictions = TRUE, returnResamp = "all")
l <- 5
pred <- pred[, -(1:6)]
#attention na.roughfix, what na include strategie to choose?
pred <- na.roughfix(pred)
cl <- autoStopCluster(parallel::makeCluster(parallel::detectCores()-1))

#create model
doParallel::registerDoParallel(cl)
mod1 <- CAST::ffs(predictors = pred, response = resp, method = "rf", importance = TRUE, trControl = trainctl,
                  metric = "Accuracy")
saveRDS(mod1, paste0("E:/Marvin/BB_rf/results/ALL_mod1_3variables_2018.rds"))

##########End Model Scenario 1 ###################

