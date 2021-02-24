########## Plots and Model performance ###################

#---
# title: "Plots"
# Author: Marvin Müsgen
# Description: Plots and Evaluation
#---


library(gt)
library(sf)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
#Studyarea+
check_library <- function(pck){
  for (i in 1:length(pck)){
    if(pck[i] %in% rownames(installed.packages()) == FALSE) {install.packages(pck[i])}
  }
  lapply(pck, require, character.only = TRUE)
}
pckgs <- c("stringr","CAST", "caret", "doParallel","randomForest","sp","rgeos") # needed Packages
check_library(pckgs)
dem_plot <- raster("E:/Marvin/BB_rf/input/observations/dem/dem_10.tif")
np <- rgdal::readOGR("C:/Users/mmues/Desktop/Masterarbeit/data/NP_Boundary.shp")
new_proj <- np@proj4string@projargs
dem_plot <- projectRaster(predstack, crs=new_proj)
np.sf <- sf::st_as_sf(np)
np.sf <- st_make_valid(np.sf)
map <- get_map(c(8.25, 48.6), zoom = 11)
nationalpark <- ggmap(map)+ggsn::scalebar(x.min = 8.183144, x.max = 8.21817,
                                          y.min = 48.707075, y.max = 48.47491, st.size = 4,
                                          dist = 5, dd2km = TRUE,dist_unit = "km", model = 'WGS84', transform = T) +
  geom_polygon(data = np, aes(x = long, y = lat, group = group), alpha = 0.3, fill="red")



world <- ne_countries(scale='medium',returnclass = 'sf')
germany <- subset(world, admin == "Germany")
gplot(test) +
  geom_tile(aes(fill=factor(value),alpha=0.8)) +
  geom_polygon(data=OR, aes(x=long, y=lat, group=group),
               fill=NA,color="grey50", size=1)+
  coord_equal()
germany.plot <- ggplot(data = germany) +
  geom_sf(fill = "cornsilk") + geom_rect(xmin = 8.183144, xmax = 8.371817, ymin = 48.49075, ymax = 48.69491,
                                         fill = NA, colour = "red", size = 0.5)
studyarea<- ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
  draw_plot(germany.plot, x = 19, y = 7, width = 9.5, height =14) +
  draw_plot(nationalpark, x = 0, y = 0, width = 19, height = 20)


ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/studyarea.png', studyarea)

library(ggplot2)
library(ggmap)
#plot prediciton
var2 <- readOGR("C:/Users/mmues/Desktop/Masterarbeit/data/Var2modelpredictfix.shp")
var2.2 <- readOGR("C:/Users/mmues/Desktop/Masterarbeit/data/Var2modelpredict.shp")
var2 <- sp::spTransform(var2, np@proj4string)
var2.2 <- sp::spTransform(var2.2, np@proj4string)


nationalpark <- ggmap(map)+ggsn::scalebar(x.min = 8.183144, x.max = 8.21817,
                                          y.min = 48.707075, y.max = 48.47491, st.size = 4,
                                          dist = 5, dd2km = TRUE,dist_unit = "km", model = 'WGS84', transform = T) +
  geom_polygon(data = var2, aes(x = long, y = lat, group = group), alpha = 1, fill="darkgreen") +
  geom_polygon(data = var2.2, aes(x = long, y = lat, group = group), alpha = 1, fill="red")

ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/predictstack2.png', nationalpark)
var2_sqm <- raster::area(var2) / 10000
var2.2_sqm <- raster::area(var2.2) / 10000


var3.1 <- readOGR("C:/Users/mmues/Desktop/Masterarbeit/data/Var3modelpredict_1.shp")
var3.2 <- readOGR("C:/Users/mmues/Desktop/Masterarbeit/data/Var3modelpredict_2.shp")
var3.3 <- readOGR("C:/Users/mmues/Desktop/Masterarbeit/data/Var3modelpredict_3.shp")
var3.1 <- sp::spTransform(var3.1, np@proj4string)
var3.2 <- sp::spTransform(var3.2, np@proj4string)
var3.3 <- sp::spTransform(var3.3, np@proj4string)

nationalpark <- ggmap(map)+ggsn::scalebar(x.min = 8.183144, x.max = 8.21817,
                                          y.min = 48.707075, y.max = 48.47491, st.size = 4,
                                          dist = 5, dd2km = TRUE,dist_unit = "km", model = 'WGS84', transform = T) +
  geom_polygon(data = var3.1, aes(x = long, y = lat, group = group), alpha = 1, fill="darkgreen") +
  geom_polygon(data = var3.2, aes(x = long, y = lat, group = group), alpha = 1, fill="red") +
  geom_polygon(data = var3.3, aes(x = long, y = lat, group = group), alpha = 1, fill="yellow")
ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/predictstack3.png', nationalpark)
var3.1_sqm <- raster::area(var3.1)
var3.1_sqm<- sum(var3.1_sqm) / 10000
var3.2_sqm <- raster::area(var3.2)
var3.2_sqm<- sum(var3.2_sqm) / 10000
var3.3_sqm <- raster::area(var3.3)
var3.3_sqm<- sum(var3.3_sqm) / 10000

#used variables
vars <- read.csv2("C:/Users/mmues/Desktop/Masterarbeit/data/Variables.csv")
colnames(vars)[1] <- "Variable"

Attach1<- vars %>%
  gt(groupname_col = "Type")

gtsave(Attach1, "Attachement1.png", "C:/Users/mmues/Desktop/Masterarbeit/Graphics/")


#confusion matrix
library(tidyr)
library(dplyr)
library(reshape2)
library(htmlTable)
library(gt)

#model accuracy
var2model <- readRDS("E:/Marvin/BB_rf/results/ALL_mod2_2variables_2018_20_01_2020.rds")
var3model <- readRDS("E:/Marvin/BB_rf/results/ALL_mod3_new_3variables_2018_2901.rds")
traindat <- read.csv("E:/Marvin/BB_rf/output/new/3mod/all_inputs_model_3var_sep.csv")
traindat <- read.csv("E:/Marvin/BB_rf/output/new/3mod/all_inputs_model_3var_sep.csv")
var2model$trainingData
set.seed(1993)
traindat$observations <- as.factor(traindat$observations)
traindat$observations <- make.names(traindat$observations)
index <- caret::createDataPartition(y = traindat$observations, p = .70, list = FALSE)
obs <- traindat[index,]
traindat <- traindat[,-ncol(traindat)]
traindat <- traindat[, -c(1:7)]
traindat<- na.roughfix(traindat)
test3 <- predict(var3model, traindat[index,])
obs <- traindat[index,]
conf3 <- caret::confusionMatrix(test3, as.factor(obs$observations))

traindat2 <- read.csv2("E:/Marvin/BB_rf/Modelinput_2_variables_20_01_21/all_inputs_model_2var_20_01-2021.csv", sep=",")
#traindat2 <-read.csv("E:/Marvin/BB_rf/Modelinput_2_variables_20_01_21/all_inputs_model_2var.csv", sep = ";")
traindat2$observations <- as.factor(traindat2$observations)
traindat2$observations <- make.names(traindat2$observations)
set.seed(1993)
index <- caret::createDataPartition(y = traindat2$observations, p = .70, list = FALSE)
obs <- traindat2[index,]
#traindat2 <- traindat2[,-ncol(traindat2)]
traindat2 <- traindat2[, -c(1:6)]
traindat2<- na.roughfix(traindat2)
r <- colnames(traindat2)
for(i in 391:446){
  r[i] <- paste(substring(colnames(traindat2[i]), 1, nchar(colnames(traindat2[i]))-5), "_Asc")
}
for(j in 447:ncol(traindat2)){
  r[j] <- paste(substring(colnames(traindat2[j]), 1, nchar(colnames(traindat2[j]))-6), "_desc")
}
colnames(traindat2) <- r
identical(colnames(var2model$trainingData[9]), colnames(traindat2[427]))
for (v in 1:ncol(traindat2)){
  traindat2[,v] <- as.numeric(as.character(traindat2[,v]))
}
test2 <- predict(var2model, traindat2[index,])
obs <- traindat2[index,]
conf2 <- caret::confusionMatrix(test2, as.factor(obs$observations))
conf2$overall

### prediction Stack


var2model <- readRDS("E:/Marvin/BB_rf/results/ALL_mod2_2variables_2018_20_01_2020.rds")
var3model <- readRDS("E:/Marvin/BB_rf/results/ALL_mod3_new_3variables_2018_2901.rds")
Sen2 <- list.files("E:/Marvin/BB_rf/output/Sen2/rasterstats/withCloud/", pattern = ".tif", full.names = T)
Sen2 <- stack(Sen2)
Sen1<- raster("E:/Marvin/BB_rf/output/Sen1_2/Ascending/VV_mean_from 2018-052.tif", check.names = FALSE)
traindat2 <- read.csv2("E:/Marvin/BB_rf/Modelinput_2_variables_20_01_21/all_inputs_model_2var_20_01-2021.csv", sep=",")
colnames(traindat2)
topo_names <- colnames(traindat2)[c(7:60)]
tif_topo <- stack("E:/Marvin/BB_rf/output/new/topo/topoparamet_data2.tif")
names(tif_topo) <- topo_names
var2model$selectedvars
tif_pred <- tif_topo[[c(49,21,7,12)]]
Sen2_pred <- Sen2[[c(6,4,8,10,14, 13)]]
names(Sen2_pred) <- c("MSR670_max", "GLI201809_means", "MSR670201809_sd","MVI201805_max", "NDRE201804_max", "MVI201805_sd")
predstack <- stack(Sen2_pred, tif_pred)
names(Sen1) <- paste("VV2018.05_means","_Asc")
Sen1_pj <- projectRaster(Sen1, predstack)
Sen1_pj@data@names <- as.character(paste0("VV2018.05_means _Asc"))
predstack <- stack(predstack, Sen1_pj)
predstack[[11]]@data@names <- "VV2018.05_means _Asc"
predextract <- mask(predstack, np.t)
np.t <- sp::spTransform(np, crs(predstack[[1]]))
rs_list <- predstack
namen <- names(rs_list)
varnamen <- var2model$selectedvars

#initialize progressbar
pb = txtProgressBar(min = 0, max = length(rs_list), initial = 0, style = 3)

#extracting rasterdata
for (i in 1:length(rs_list)){
  setTxtProgressBar(pb,i)
  #extracting raster values
  print(paste0("Starting extracting raster data for observation points: ", Sys.time()))
  df <- as.data.frame(rs_list[[i]])
  colnames(df) <-  namen[[i]]
  if(i ==1){
    obs <- df
  }
  else{
    print(paste0("Finished extracting raster data for observation points: ", Sys.time()))
    obs <- cbind(obs,df)
  }
}
obs<- na.roughfix(obs)
colnames(obs)[[11]] <- varnamen[9]
pred_stack_df <- predict(var2model, obs)
pred_stack_df <- as.character(pred_stack_df)
pred_stack_df <- substr(pred_stack_df,2, 2)
pred_stack_df <- as.numeric(pred_stack_df)
predraster <- predstack[[1]]
values(predraster) <- pred_stack_df
plot(predraster)
predraster <- mask(predraster, np.t)
plot(predraster)
writeRaster(predraster, "E:/Marvin/BB_rf/results/predicitonvar2model.tif")


var3model$selectedvars
Sen2 <- list.files("E:/Marvin/BB_rf/output/Sen2/rasterstats/withCloud/", pattern = ".tif", full.names = T)
Sen2 <- stack(Sen2)
predstack3 <- Sen2[[c(9,18,11,3,5,16,17,15,7,12,2,1)]]
names(predstack3) <- c("MSR670_sd","NGRDI201809_means","MVI201804_min","ExGR201807_min","GLI_means","NDRE201809_sd",
                       "NGRDI201804_max","NDRE201807_means","MSR670201809_min","MVI201805_min","DSWI4201806_sd","CIrededge_sd")
Prediction3modelstack <- raster::predict(predstack3, var3model)
predraster3 <- mask(Prediction3modelstack, np.t)
writeRaster(predraster3, "E:/Marvin/BB_rf/results/predicitonvar3model.tif")
plot(predraster3)

predpoly2<- rasterToPolygons(predraster, dissolve = T)

###FFS variable importance

x<- caret::varImp(var2model)
for (i in seq(nrow(x$importance))){
  x$importance$mean[i] <- rowMeans(x$importance[i,1:2])
}

y<- caret::varImp(var3model)
for (i in seq(nrow(y$importance))){
  if(i == 1){
    for(j in seq(nrow(y$importance))){
      y$importance$maxi[j] <- y$importance[j,1] + y$importance[j,2] + y$importance[j,3]
    }
    maxi<- max(y$importance$maxi)
    y$importance$maxi<- y$importance$maxi / maxi * 100
  }
  y$importance$mean[i] <- rowMeans(y$importance[i,1:2])
}

write.table(x = x$importance, file = paste0("C:/Users/mmues/Desktop/Masterarbeit/data/" , "varimp2.csv"), sep = ",", dec = ".")
varimp2tbl<- read.csv("C:/Users/mmues/Desktop/Masterarbeit/data/varimp2.csv", sep = ",")
varimpgg2<-fortify(varimp2tbl)
varimpgg2$variable <- rownames(varimpgg2)
varimpgg2$variable[3] <- "sunshine_duration201804"
varimpgg2$variable[4] <- "air_temperature201806"
varimpgg2$variable[9] <- "VV_asc_201805_means"

write.table(x = y$importance, file = paste0("C:/Users/mmues/Desktop/Masterarbeit/data/" , "varimp3.csv"), sep = ",", dec = ".")
varimp3tbl<- read.csv("C:/Users/mmues/Desktop/Masterarbeit/data/varimp3.csv", sep = ",")
varimpgg3<-fortify(varimp3tbl)
varimpgg3$variable <- rownames((varimpgg3))

library(caret)
varimpffs2<- ggplot2::ggplot(varimpgg2, aes(x=reorder(variable,+mean,sum), y=mean)) + geom_point(color = "dodgerblue1", size = 2)+
  geom_linerange(aes(ymin = 0, ymax = mean), color = "dodgerblue1", linetype = 2)+
  coord_flip()+
  xlab("Predictor variable")+
  ylab("Relative importance")+
  theme_minimal()+
  theme(text = element_text(size=12))

varimpffs3<- ggplot2::ggplot(varimpgg3, aes(x=reorder(variable,+maxi,sum), y=maxi)) + geom_point(color = "dodgerblue1", size = 2)+
  geom_linerange(aes(ymin = 0, ymax = maxi), color = "dodgerblue1", linetype = 2)+
  coord_flip()+
  xlab("Predictor variable")+
  ylab("Relative importance")+
  theme_minimal()+
  theme(text = element_text(size=12))

ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/varimpmod1ffs.png', varimpffs)

library(gridExtra)
ffs <- grid.arrange(varimpffs2, varimpffs3, ncol = 2)
ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/ffs.png', ffs)


## terrain ascending descending

traindat <- read.csv("E:/Marvin/BB_rf/output/new/3mod/all_inputs_model_3var_sep.csv")
colnames(traindat)
traindat <- traindat[,-c(1:397)]
traindat <- traindat[,-c(9:56)]
traindat <- traindat[, -c(17:64)]
#traindat <-traindat[,-c(20:67)]
colnames(traindat) <- c("means_VH_Asc", "max_VH_Asc", "min_VH_Asc", "sd_VH_Asc", "means_VV_Asc", "max_VV_Asc",
                        "min_VV_Asc", "sd_VV_Asc", "means_VH_Dsc", "max_VH_Dsc", "min_VH_Dsc", "sd_VH_Dsc",
                        "means_VV_Dsc", "max_VV_Dsc","min_VV_Dsc", "sd_VV_Dsc", "cluster")
traindat_cl <- traindat[,-ncol(traindat)]
for(i in 1:ncol(traindat_cl)){
  if(i == 1){
    boxplot_df <- as.data.frame(traindat_cl[,i])
    colnames(boxplot_df) <- "values"
    rowcols <- nrow(traindat_cl)
    rowcols[c(1:nrow(traindat_cl))] <- colnames(traindat_cl[i])
    boxplot_df$Band <- rowcols
  }else{
    temp_df <- as.data.frame(traindat_cl[,i])
    colnames(temp_df) <- "values"
    rowcols <- nrow(traindat_cl)
    rowcols[c(1:nrow(traindat_cl))] <- colnames(traindat_cl[i])
    temp_df$Band <- rowcols
    boxplot_df<- rbind(boxplot_df, temp_df)
  }
}
library(stringr)
boxplot_vh <- boxplot_df[which(str_detect(boxplot_df$Band, "VH")== TRUE),]
boxplot_vv <- boxplot_df[which(str_detect(boxplot_df$Band, "VV")== TRUE),]
p_vh <- ggplot(boxplot_vh, aes(Band, values)) + geom_boxplot() + theme_grey(base_size = 10) +
  ylab("dB Backscatter") + coord_flip() + scale_y_continuous(breaks=seq(-30, 15,5)) +
  xlab(NULL) + stat_boxplot(geom ="errorbar", width=0.6, colour = "#666666", alpha= 0.7) + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(text = element_text(size=10))
p_vv <- ggplot(boxplot_vv, aes(Band, values)) + geom_boxplot() + theme_grey(base_size = 10) +
  ylab("dB Backscatter") + coord_flip() + scale_y_continuous(breaks=seq(-30, 15,5)) +
  xlab(NULL) + stat_boxplot(geom ="errorbar", width=0.6, colour = "#666666", alpha= 0.7) + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(text = element_text(size=10))
terrai_effect_all <- grid.arrange(p_vv, p_vh, ncol = 2)
ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/terrai_effect_all.png', terrai_effect_all)

p <- ggplot(boxplot_df, aes(Band, values)) + geom_boxplot() + coord_flip()
traindat_1 <- dplyr::filter(traindat, cluster %in% "1")
traindat_cl_1 <- traindat_1[,-ncol(traindat_1)]
for(i in 1:ncol(traindat_cl_1)){
  if(i == 1){
    boxplot_df_1 <- as.data.frame(traindat_cl_1[,i])
    colnames(boxplot_df_1) <- "values"
    rowcols <- nrow(traindat_cl_1)
    rowcols[c(1:nrow(traindat_cl_1))] <- colnames(traindat_cl_1[i])
    boxplot_df_1$Band <- rowcols
  }else{
    temp_df <- as.data.frame(traindat_cl_1[,i])
    colnames(temp_df) <- "values"
    rowcols <- nrow(traindat_cl_1)
    rowcols[c(1:nrow(traindat_cl_1))] <- colnames(traindat_cl_1[i])
    temp_df$Band <- rowcols
    boxplot_df_1<- rbind(boxplot_df_1, temp_df)
  }
}
boxplot_df_1vv <- boxplot_df_1[which(str_detect(boxplot_df_1$Band, "VV")== TRUE),]
boxplot_df_1vv$Band <- gsub("_VV_", "_", boxplot_df_1vv$Band)
p_df_1vv <- ggplot(boxplot_df_1vv, aes(Band, values)) + geom_boxplot() +  theme_grey(base_size = 10) +
  ylab("dB Backscatter") + coord_flip()  + ylim(-25, 10)+
  xlab(NULL) + stat_boxplot(geom ="errorbar", width=0.6, colour = "#666666", alpha= 0.7) + ggtitle("North") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(text = element_text(size=10))


traindat_2 <- dplyr::filter(traindat, cluster %in% "2")
traindat_cl_2 <- traindat_2[,-ncol(traindat_2)]
for(i in 1:ncol(traindat_cl_2)){
  if(i == 1){
    boxplot_df_2 <- as.data.frame(traindat_cl_2[,i])
    colnames(boxplot_df_2) <- "values"
    rowcols <- nrow(traindat_cl_2)
    rowcols[c(1:nrow(traindat_cl_2))] <- colnames(traindat_cl_2[i])
    boxplot_df_2$Band <- rowcols
  }else{
    temp_df <- as.data.frame(traindat_cl_2[,i])
    colnames(temp_df) <- "values"
    rowcols <- nrow(traindat_cl_2)
    rowcols[c(1:nrow(traindat_cl_2))] <- colnames(traindat_cl_2[i])
    temp_df$Band <- rowcols
    boxplot_df_2<- rbind(boxplot_df_2, temp_df)
  }
}
p_2 <- ggplot(boxplot_df_2, aes(Band, values)) + geom_boxplot()
boxplot_df_2vv <- boxplot_df_2[which(str_detect(boxplot_df_2$Band, "VV")== TRUE),]
boxplot_df_2vv$Band <- gsub("_VV_", "_", boxplot_df_2vv$Band)
p_df_2vv <- ggplot(boxplot_df_2vv, aes(Band, values)) + geom_boxplot() + theme_grey(base_size = 10) +
  ylab("dB Backscatter") + coord_flip() + ylim(-25, 10) +
  xlab(NULL) + stat_boxplot(geom ="errorbar", width=0.6, colour = "#666666", alpha= 0.7) + ggtitle("East") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(text = element_text(size=10))
#p_3 <- ggplot(boxplot_df_3, aes(Band, values)) + geom_boxplot()
traindat_3 <- dplyr::filter(traindat, cluster %in% "3")
traindat_cl_3 <- traindat_3[,-ncol(traindat_3)]
for(i in 1:ncol(traindat_cl_3)){
  if(i == 1){
    boxplot_df_3 <- as.data.frame(traindat_cl_3[,i])
    colnames(boxplot_df_3) <- "values"
    rowcols <- nrow(traindat_cl_3)
    rowcols[c(1:nrow(traindat_cl_3))] <- colnames(traindat_cl_3[i])
    boxplot_df_3$Band <- rowcols
  }else{
    temp_df <- as.data.frame(traindat_cl_3[,i])
    colnames(temp_df) <- "values"
    rowcols <- nrow(traindat_cl_3)
    rowcols[c(1:nrow(traindat_cl_3))] <- colnames(traindat_cl_3[i])
    temp_df$Band <- rowcols
    boxplot_df_3<- rbind(boxplot_df_3, temp_df)
  }
}
boxplot_df_3vv <- boxplot_df_3[which(str_detect(boxplot_df_3$Band, "VV")== TRUE),]
boxplot_df_3vv$Band <- gsub("_VV_", "_", boxplot_df_3vv$Band)
p_df_3vv <- ggplot(boxplot_df_3vv, aes(Band, values)) + geom_boxplot() + theme_grey(base_size = 10) +
  ylab("dB Backscatter") + coord_flip() + ylim(-25, 10) +
  xlab(NULL) + stat_boxplot(geom ="errorbar", width=0.6, colour = "#666666", alpha= 0.7) + ggtitle("South") + theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(text = element_text(size=10))

p_3 <- ggplot(boxplot_df_3, aes(Band, values)) + geom_boxplot()

traindat_4 <- dplyr::filter(traindat, cluster %in% "4")
traindat_cl_4 <- traindat_4[,-ncol(traindat_4)]
for(i in 1:ncol(traindat_cl_4)){
  if(i == 1){
    boxplot_df_4 <- as.data.frame(traindat_cl_4[,i])
    colnames(boxplot_df_4) <- "values"
    rowcols <- nrow(traindat_cl_4)
    rowcols[c(1:nrow(traindat_cl_4))] <- colnames(traindat_cl_4[i])
    boxplot_df_4$Band <- rowcols
  }else{
    temp_df <- as.data.frame(traindat_cl_4[,i])
    colnames(temp_df) <- "values"
    rowcols <- nrow(traindat_cl_4)
    rowcols[c(1:nrow(traindat_cl_4))] <- colnames(traindat_cl_4[i])
    temp_df$Band <- rowcols
    boxplot_df_4<- rbind(boxplot_df_4, temp_df)
  }
}
boxplot_df_4vv <- boxplot_df_4[which(str_detect(boxplot_df_4$Band, "VV")== TRUE),]
boxplot_df_4vv$Band <- gsub("_VV_", "_", boxplot_df_4vv$Band)
p_df_4vv <- ggplot(boxplot_df_4vv, aes(Band, values)) + geom_boxplot() + theme_grey(base_size = 10) +
  ylab("dB Backscatter") + coord_flip() + ylim(-25, 10) +
  xlab(NULL) + stat_boxplot(geom ="errorbar", width=0.6, colour = "#666666", alpha= 0.7) + ggtitle("West") + theme_bw() + theme(plot.title = element_text(hjust=0.5))+
  theme(text = element_text(size=10))

Asc_desc_orientation <- grid.arrange(p_df_1vv, p_df_2vv, p_df_3vv, p_df_4vv, ncol = 2)
ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/Asc_desc_direction.png', Asc_desc_orientation)

## stack prediction

var2model <- readRDS("E:/Marvin/BB_rf/results/ALL_mod2_2variables_2018_20_01_2020.rds")
var3model <- readRDS("E:/Marvin/BB_rf/results/ALL_mod3_new_3variables_2018_2901.rds")
tif_asc<- stack("E:/Marvin/BB_rf/output/new/Sen_1_2/Ascending/_Sen1_Ascending_Stack2.tif")
tif_dsc <- stack("E:/Marvin/BB_rf/output/new/Sen_1_2/Descending/_Sen1_Descending_Stack2.tif")
tif_topo <- stack("E:/Marvin/BB_rf/output/new/topo/topoparamet_data2.tif")

## topographic influence

topos <- data.frame()
topos$MSR670 <- traindat$MSR670_means
topos$Wind_effect <- traindat$Wind_effect_201804_to_201809
topos$VV_Asc <- traindat$VV_means._Asc
topos$Air_temperature <- traindat$grids_germany_monthly_air_temp_mean_201807
topos$cluster <- traindat$cluster

p_df_4vv <- ggplot(boxplot_df_4vv, aes(cluster)) + geom_boxplot() + theme_grey(base_size = 10) +
  ylab("dB Backscatter") + coord_flip() + ylim(-25, 10) +
  xlab(NULL) + stat_boxplot(geom ="errorbar", width=0.6, colour = "#666666", alpha= 0.7) + ggtitle("West") + theme_bw() + theme(plot.title = element_text(hjust=0.5))+
  theme(text = element_text(size=10))

#dates

dates <- read.csv2("C:/Users/mmues/Desktop/Masterarbeit/data/dates.csv")
dates
plot(dates)
library(ggplot2)
dateplot<- ggplot2::ggplot(dates, aes(fill=type, y=Frequency, x=Date)) +
  geom_bar(position="dodge", stat="identity")
ggsave('C:/Users/mmues/Desktop/Masterarbeit/Graphics/dates.png', dateplot)


##### ENd Plots
