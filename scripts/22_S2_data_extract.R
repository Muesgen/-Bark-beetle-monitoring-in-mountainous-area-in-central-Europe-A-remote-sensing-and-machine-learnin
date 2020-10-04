########## Remove cloud pixel from Sentinel data, calucate raster statistics and do a principal component analysis ##########
#---
# title: "Sentinel 2  raster_calc"
# Author: Marvin MÃüsgen
# Description: This Script remove cloud values fro
#---
 
######Setting parameters ###############################
observations <- "o_total.csv" # name of observation data
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf") # setting working directory
Input_dir <- "/output/Sen2/Indices" # inpiut directory
# nedded packages:
pckgs <- c("velox","raster","rgdal","pbapply","doParallel", "gdalUtils","rgeos","sp", "matrixStats","dplyr","terra","sf", "stringr")
funfold= paste0(getwd(),"/functions") # function folder directory
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes
ext <- "/observations/NP_Boundary.shp" # File in Input_dir: Polygon Shapefile with the extent of the area of Interest
xcoordcolnum <- 3 # column number of the x coordinate of your obseravtion date
ycoordcolnum <- 4 # column number of the y coordinate of your observation data
projObs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"  # Projection of the x and y column of observation file
area_path <- paste0(getwd(),"/input/observations/NP_Boundary.shp")


#######Loading librarys and functions ##################
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/statistic_function.R"))
source(paste0(funfold,"/gdal_resample.R"))

check_library(pckgs)
######################################################

###Listing Dates #####
#files_dates<- as.character(as.Date(substr(list.files(paste0(getwd(), Input_dir,"/"),full.names = FALSE, pattern = ".tif$"), 12, 19), format="%Y%m%d"))
files_dates<- as.character(substr(list.files(paste0(getwd(), Input_dir,"/"),full.names = FALSE, pattern = ".tif$"), 12, 19))
month <- as.character(substr(list.files(paste0(getwd(), Input_dir,"/"),full.names = FALSE, pattern = ".tif$"), 12, 17))
month <- unique(month)
month %>% group_by_all() %>% filter(n() == 1)
print("File dates are listed")
print(files_dates)

###Listing the month for filtering later the files dates
#forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
#forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))
#seq_dates <- seq.Date(forced_start, forced_end, by = "month")
#month<- strftime(seq_dates,"%Y-%m")
#print("Month are listed")
#print(month)

#speeding up the raster package
rasterOptions(maxmemory = 1e+09)

#reading the rasters
temp <- list.files(paste0(getwd(), Input_dir,"/"), pattern = "*.tif", full.names = TRUE)
rs_list<- lapply(temp, stack)
#rs <- rs_list[[1]]

#read in the observations
obs <-read.csv(paste0(getwd(),"/input/observations/", observations))
print("observations loaded")
head(obs)
#obs[,5:ncol(obs)] <- NULL
#defining coordinates, Lat = Y Long = X
xy <- obs[,c(xcoordcolnum,ycoordcolnum)]
xy <-as.list(xy)
f <-paste0(getwd(),"/output/Sen2/used_indices.txt")
if (is.null(projObs) == TRUE){
  #creating a spatioalpointdataframe
  spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                                 proj4string = CRS(rs[[1]]@crs@projargs))
  print("Spatialdataframe is created")
  head(spdf)
  print("POSSIBLE ERROR: Please check the outwritten DataFrame, could be with NAs")
  Print("SOlution: check your coordinates in the observation DF and give the correct Projection to the projOBS argument")
} else{
  #creating a spatioalpointdataframe
  spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                                 proj4string = CRS(projObs))
  print("Spatialdataframe is created")
  head(spdf)
}

#transforming into correct projection
r_crs<- rs_list[[1]]@crs
spdf <- sp::spTransform(spdf, CRS(proj4string(r_crs)))

#creating correct rasternames
singleString <- paste(readLines(paste0(getwd(),"/output/Sen2/used_indices.txt")), collapse=" ")
veg_names <- unlist(strsplit(singleString, " "))

#initialize progressbar
pb = txtProgressBar(min = 0, max = length(rs_list), initial = 0, style = 3)

#extracting rasterdata
for (i in 1:length(rs_list)){
  setTxtProgressBar(pb,i)
  names(rs_list[[i]]) <- paste0(files_dates[[i]],"_",veg_names)
  #extracting raster values
  print(paste0("Starting extracting raster data for observation points: ", Sys.time()))
  vx_rs <- velox::velox(rs_list[[i]])
  df <- as.data.frame(vx_rs$extract_points(spdf))
  colnames(df) <-  paste0(files_dates[[i]],"_",veg_names)
  print(paste0("Finished extracting raster data for observation points: ", Sys.time()))
  obs <- cbind(obs,df)
}

write.csv(obs,file = paste0(getwd(),"/output/Sen2/_Extracted_data",start,end,".csv"))

####### Extracting clouds ######

# list sentinel dirs
cloud_dirs <- list.files(paste0(getwd(), "/input/Sentinel_2_l2a/"), pattern = "*.SAFE", full.names = TRUE)

#defining cloud mask
searchPattern <- "SCL_20m.jp2$"

#reading cloud raster
cloud_path_list <- list()
# listing cloud files
for (i in 1:length(cloud_dirs)){
  cloud_path_list[i] <- list.files(file.path(cloud_dirs[i], "GRANULE"), pattern = searchPattern, full.names = TRUE, recursive = TRUE)
}
# creating tifs, you also can skip this step. But not every rgdal and gdal version can read jp2 format. so this creates tif formats 
pblapply(cloud_path_list, function(x){
  gdal_translate(x, paste0(str_sub(x,end = -4),"tif"))
})

#new cloud mask format
searchPattern2 <- "SCL_20m.tif$"

#listing cloud tif files
cloud_path_list_tif <- list()
for (i in 1:length(cloud_dirs)){
  cloud_path_list_tif[i] <- list.files(file.path(cloud_dirs[i], "GRANULE"), pattern = searchPattern2, full.names = TRUE, recursive = TRUE)
}

# stacking clouds
rs_clouds <- stack(cloud_path_list_tif)

#clear obs df
obs[,5:ncol(obs)] <- NULL

#list cloud_dates
cloud_dates <- as.character(substr(list.files(paste0(getwd(),"/input/Sentinel_2_l2a/")), 12, 19))


#set progressbar
pb = txtProgressBar(min = 0, max = nlayers(rs_clouds), initial = 0, style = 3)
# extracting raster
for (i in 1:nlayers(rs_clouds)){
  setTxtProgressBar(pb,i)
  #names(rs_clouds[[i]]) <- paste0(files_dates[[i]],"_",veg_names)
  #extracting raster values
  print(paste0("Starting extracting raster data for observation points: ", Sys.time()))
  vx_rs <- velox::velox(rs_clouds[[i]])
  df <- as.data.frame(vx_rs$extract_points(spdf))
  colnames(df) <-  paste0(cloud_dates[[i]],"_scl")
  print(paste0("Finished extracting raster data for observation points: ", Sys.time()))
  obs <- cbind(obs,df)
}

write.csv(obs, paste0(getwd(),"/output/Sen2/_Extracted_SCL",start,end,".csv"))

####### Remove Clouds and cloud shadows from extracted data #######

scl <- read.csv(paste0(getwd(),"/output/Sen2/_Extracted_SCL2018-04-012018-09-30.csv"))
data <- read.csv(paste0(getwd(),"/output/Sen2/_Extracted_data2018-04-012018-09-30.csv")) 

for (i in 1:length(cloud_dates)){
  print(paste0("Succesfully removed cloud pixels ",cloud_dates[i]))
  #selecting the correct cloud coverage
  tmp <- scl[str_detect(colnames(scl), cloud_dates[i]) == TRUE]
  #selecting cloud cover 3= cloud shadows, 8=cloud_Medium_probability, 9=Cloud_high_Probability, 10=Thin_Cirrus
  rows <- which(tmp == 3 | tmp == 8 | tmp == 9 | tmp == 10)
  #remove cloud values
  data[rows,str_detect(colnames(data), cloud_dates[i]) == TRUE] <- NA
}

write.csv(data, paste0(getwd(),"/output/Sen2/_Extracted_data_no_clouds",start,end,".csv"))

####### Statistics #########
#calculating statistics from the observation points and write it to data frame
obs <- data
#splitting data frame into indices and calculating the statistics
pblapply(veg_names,function(x){
  obs_stats <- as.data.frame(matrix(ncol=7,nrow= NROW(obs)))
  headers <-c(paste0(x,"_means"), paste0(x,"_max"), paste0(x,"_min"), paste0(x,"_25q"), 
             paste0(x,"_75q"), paste0(x,"_sd"), paste0(x,"_median"))
  colnames(obs_stats) <- headers
  obs_I <- obs[,grepl(x, colnames(obs))]
  obs_stats[,1] <- rowMeans(obs_I[,1:ncol(obs_I)], 1, na.rm = TRUE) #mean
  obs_stats[,2] <- apply(obs_I[,1:ncol(obs_I)],1, max, na.rm = TRUE) #maximum
  obs_stats[,3] <- apply(obs_I[,1:ncol(obs_I)],1, min, na.rm = TRUE) # min
  obs_quants <- cbind(obs_I, t(apply(obs_I, 1, quantile, c(0.25, .5, .75), na.rm = TRUE))) #calc the quantiles
  obs_stats[,4] <- obs_quants$`25%`  #25 quantile
  obs_stats[,5] <- obs_quants$`75%` # 25 quantile
  obs_stats[,6] <- apply(obs_I[,1:ncol(obs_I)],1, sd, na.rm=TRUE) # standard deviaton
  obs_stats[,7] <- obs_quants$`50%` #median
  index1 <- which(obs_stats == Inf | obs_stats == -Inf | obs_stats == NA) # list index of wrong data
  obs_stats[index1,] <- NA
  is.na(obs_stats) <- do.call(cbind,lapply(obs_stats, is.infinite))
  write.csv(obs_stats, file = paste0(getwd(),"/output/Sen2/observation_stats_without_cloudvalues",start,end,x,".csv"))
  #calculating statistics for every vegetation indix and month
  lapply(month, function(y){
    month_stats <- as.data.frame(matrix(ncol=7,nrow= NROW(obs)))
    headers <-c(paste0(x,y,"_means"), paste0(x,y,"_max"), paste0(x,y,"_min"), paste0(x,y,"_25q"), 
                paste0(x,y,"_75q"), paste0(x,y,"_sd"), paste0(x,y,"_median"))
    colnames(month_stats) <- headers
    obs_IM <- obs_I[,grepl(y, colnames(obs_I))]
    month_stats[,1] <- rowMeans(obs_IM[,1:ncol(obs_IM)], 1, na.rm = TRUE) #mean
    month_stats[,2] <- apply(obs_IM[,1:ncol(obs_IM)],1, max, na.rm = TRUE) #maximum
    month_stats[,3] <- apply(obs_IM[,1:ncol(obs_IM)],1, min, na.rm = TRUE) # min
    obs_quants_M <- cbind(obs_IM, t(apply(obs_IM, 1, quantile, c(0.25, .5, .75), na.rm = TRUE))) #calc the quantiles
    month_stats[,4] <- obs_quants_M$`25%`  #25 quantile
    month_stats[,5] <- obs_quants_M$`75%` # 25 quantile
    month_stats[,6] <- apply(obs_IM[,1:ncol(obs_IM)],1, sd, na.rm=TRUE) # standard deviaton
    month_stats[,7] <- obs_quants_M$`50%` #median
    index2 <- which(obs_stats == Inf | obs_stats == -Inf | obs_stats == NA) #list index if wrong data
    month_stats[index2,] <- NA
    is.na(month_stats) <- do.call(cbind,lapply(month_stats, is.infinite))
    write.csv(month_stats, file = paste0(getwd(),"/output/Sen2/observation_stats_without_cloudvalues",x,y,".csv"))
    })
  })

####### raster statistics #######

###change the stack format to a processable format###
rs_unstacked <- lapply(rs_list, function(x){
  r_temp <- raster::unstack(x)
  names(r_temp) <- paste0(names(x),"_", veg_names)
  return(r_temp)
})
rs_list<- unlist(rs_unstacked)
rs <- stack(rs_list)

name <- names(rs)

###creating cloud masks###
#resampling clouds
for (j in 1:nlayers(rs_clouds)){
  print(paste(Sys.time(), "Resampling raster", j, "of", nlayers(rs_clouds),"..."))
  if (j == 1){
    l <-list()
    l[[1]] <- gdal_resample(rs_clouds[[j]],rs_list[[1]], method = "near")
    names(l[[1]]) <- names(rs_clouds)[[1]]
  }
  else{
    l[[j]] <- gdal_resample(rs_clouds[[j]],rs_list[[1]], method = "near")
    names(l[[j]]) <- names(rs_clouds)[[j]]
  }
}
rs_clouds <- stack(l)
#croping clouds to studyarea
studyarea <- readOGR(area_path)
studyarea <-  spTransform(studyarea, projection(rs_clouds))
rs_clouds <- crop(rs_clouds, studyarea)
#set progressbar
pb = txtProgressBar(min = 0, max = nlayers(rs_clouds), initial = 0, style = 3)
# cloud masking
for (i in 1:nlayers(rs_clouds)){
  setTxtProgressBar(pb,i)
  t <- rs_clouds[[i]]
  t[t == 3 | t == 8 | t == 9 | t == 10] <- NA
  rs_clouds[[i]] <- t
  #masking clouds
}

#settin progressbar
pb = txtProgressBar(min = 0, max = nlayers(rs_clouds), initial = 0, style = 3)

#remove values which are cloud pixels from Rasterstack before calculate statistics
for (i in 1:length(cloud_dates)){
  setTxtProgressBar(pb,i)
  print(Sys.time())
  #selecting the correct cloud coverage
  tmp_rs <- rs[[which(str_detect(names(rs), cloud_dates[i]) == TRUE)]]
  if(nlayers(tmp_rs) == 0){
    print(paste0("no layer for date:", cloud_dates[i],"; skipping to next date"))
    next
  }
  #selecting cloud cover 3= cloud shadows, 8=cloud_Medium_probability, 9=Cloud_high_Probability, 10=Thin_Cirrus
  tmp_clouds <- rs_clouds[[which(str_detect(names(rs_clouds), cloud_dates[i]) == TRUE)]]
  #remove cloud values
  print(paste0("starting masking for date: ", cloud_dates[i]))
  if(exists("ras_masked") == FALSE){
    ras_masked <- mask(tmp_rs, tmp_clouds)
  }
  temp_ras <- mask(tmp_rs, tmp_clouds)
  ras_masked <- raster::stack(ras_masked,temp_ras)
  print(paste0("finished ", cloud_dates[i]))
}
terra::writeRaster(ras_masked, paste0(getwd(),"/output/Sen2/rasterVEGIndices_clouds_masked_out.tif"), overwrite=TRUE)


### calculating raster statistics
pblapply(veg_names, function(x){
  print(paste0("start calulating raster statistics for whole time period of polarization: ",x," ",Sys.time()))
  rs_stat <- ras_masked[[which(grepl(x,name) == TRUE)]]
  print("mean...")
  beginCluster()
  rs_mean <- na.exclude(stackApply(rs_stat, indices= rep(1,nlayers(rs_stat)), fun = mean))
  print("min...")
  rs_min <- stackApply(rs_stat, indices= rep(1,nlayers(rs_stat)), fun = min, na.rm = TRUE)
  print("max...")
  rs_max <- stackApply(rs_stat, indices= rep(1,nlayers(rs_stat)), fun = max, na.rm = TRUE)
  print("75th quantile..")
  rs_75q <- clusterR(rs_stat,fq75)
  print("25th quantile..")
  rs_25q <- clusterR(rs_stat,fq25)
  print("standard deviation...")
  rs_sd <- clusterR(rs_stat,fqsd)
  print("median...")
  rs_median <- clusterR(rs_stat,fq50)
  endCluster()
  print(paste0(" raster statistics are calculated succesfully for whole time period of polarization: ",x," ",Sys.time()))
  print(paste0("start writing raster statistics for whole time period: ",x," ",Sys.time()))
  terra::writeRaster(rs_mean, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_mean_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_max, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_max_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_min, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_min_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_75q, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_75q_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_25q, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_25q_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_sd, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_sd_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_median, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_median_from",start,"_to_",end,".tif"), overwrite=TRUE)
  rm(rs_mean,rs_max,rs_min,rs_75q,rs_25q,rs_sd, rs_median)
  print(paste0("Finish writing raster statistics for whole time period: ",x," ",Sys.time()))
  pblapply(month, function(y){
    m_dates<- as.character(substr(names(rs_stat), 12, 19))
    index <- which(m_dates %in% m_dates[grepl(y, m_dates)])# getting the Index of selected rasters in time period
    rs_month <- rs_stat[[index]]
    print(paste0("start calulating raster statistics for month ",y," of vegetation Index: ",x," ",Sys.time()))
    #rs_stat <- rs_month[[which(grepl(x,names(rs_month)) == TRUE)]]
    beginCluster()
    print("mean...")
    rsm_mean <- na.exclude(stackApply(rs_month, indices= rep(1,nlayers(rs_month)), fun = mean))
    print("min...")
    rsm_min <- stackApply(rs_month, indices= rep(1,nlayers(rs_month)), fun = min, na.rm = TRUE)
    print("max...")
    rsm_max <- stackApply(rs_month, indices= rep(1,nlayers(rs_month)), fun = max, na.rm = TRUE)
    print("75th quantile...")
    rsm_75q <- clusterR(rs_month,fq75)
    print("25th quantile...")
    rsm_25q <- clusterR(rs_month,fq25)
    print("standard deviaton...")
    rsm_sd <- clusterR(rs_month,fqsd)
    print("median...")
    rsm_median <- clusterR(rs_month,fq50)
    endCluster()
    print(paste0("Finished calculating raster statistics for month ",y," of vegetation index: ",x," ",Sys.time()))
    print(paste0("start writing raster statistic for month: ", x, " ",Sys.time()))
    terra::writeRaster(rsm_mean, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_mean_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_max, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_max_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_min, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_min_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_75q, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_75q_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_25q, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_25q_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_sd, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_sd_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_median, paste0(getwd(),"/output/Sen2/rasterstats/",x,"_median_from ",y,".tif"), overwrite=TRUE)
    rm(rs_month,rsm_mean,rsm_max,rsm_min,rsm_75q,rsm_25q,rsm_sd, rsm_median)
    print(paste0(" raster statistics for month ", y,"of Vegetation Index ",x," are written succesfully"))
  })
  rm(rs_stat)
})
"start calulating raster statistics for whole time period of polarization: MSI 2020-09-29 14:18:41"
