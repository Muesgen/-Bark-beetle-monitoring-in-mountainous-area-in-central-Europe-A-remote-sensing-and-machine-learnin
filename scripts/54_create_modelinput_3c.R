# model all 2 classes #2

paramdata <- read.csv2("E:/Marvin/BB_rf/output/new/3mod/topo/paramet_data3.csv")
sen2data <- list.files("E:/Marvin/BB_rf/output/new/3mod/Sen_2/", pattern = ".csv", full.names = TRUE)
#sen2data<- sen2data[-87]
#sen2data<- sen2data[-1]
#sen2data<- sen2data[-1]
test <- read.csv2("E:/Marvin/BB_rf/output/new/3mod/Sen_1_2/Ascending/_Extracted_data_32018-04-012018-09-30.csv", sep=",")
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
sen1ascdata <- list.files("E:/Marvin/BB_rf/output/new/3mod/Sen_1_2/Ascending", pattern = ".csv", full.names = TRUE)
sen1ascdata<- sen1ascdata[-1]
for(i in 1:length(sen1ascdata)){
  x<- sen1ascdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_Asc")
  paramdata <- cbind(paramdata, df)
}

print("loaded ascending")
sen1descdata <- list.files("E:/Marvin/BB_rf/output/new/3mod/Sen_1_2/Descending", pattern = ".csv", full.names = TRUE)
sen1descdata <- sen1descdata[-1]
for(i in 1:length(sen1descdata)){
  x<- sen1descdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_desc")
  paramdata <- cbind(paramdata, df)
}
print("loaded descending")
write.csv(paramdata , "E:/Marvin/BB_rf/output/new/3mod/all_inputs_model_3var_sep.csv")
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
