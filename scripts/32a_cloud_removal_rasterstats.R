######### Sentinel 2 cloud removal, raster statistics calculation##########
#---
# title: "Sentinel 2  cloud removaL and calc rasterstats"
# Author: Marvin Müsgen
# Description: In a first step the script remove all values which were detected
# as clouds and cloud shadows from the ESA SCL classification
# In the second script raster statistics will be calculate over
#the indices for the whole period of interest and every month
#---

# Set Paths
observations <- "o_total.csv" # name of observation data
setwd("D:/Marvin/BB_rf") # setting working directory
Input_dir <- "/output/Sen2/Indices" # inpiut directory
funfold= paste0(getwd(),"/functions") # function folder directory
area_path <- paste0(getwd(),"/input/observations/NP_Boundary.shp")
ext <- "/observations/NP_Boundary.shp" # File in Input_dir: Polygon
#Shapefile with the extent of the area of Interest

# Set variables:
pckgs <- c("velox","raster","rgdal","pbapply","doParallel", "gdalUtils",
           "rgeos","sp", "matrixStats","dplyr","terra","sf", "stringr")
# needed packages
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes
xcoordcolnum <- 3 # column number of the x coordinate of your obseravtion date
ycoordcolnum <- 4 # column number of the y coordinate of your observation data
projObs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
# Projection of the x and y column of observation file

# source functions
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/statistic_function.R"))
source(paste0(funfold,"/gdal_resample.R"))

# reading librarys
check_library(pckgs)

# listing dates of interest
files_dates<- as.character(substr(list.files(paste0(getwd(),
              Input_dir,"/"),full.names = FALSE, pattern = ".tif$"), 12, 19))
# listing dates of images
month <- as.character(substr(list.files(paste0(getwd(), Input_dir,"/"),
              full.names = FALSE, pattern = ".tif$"), 12, 17)) # listing month
month <- unique(month) # filter month to unique values
print("File dates are listed")
print(files_dates)

# speeding up the raster package
rasterOptions(maxmemory = 1e+09)

# reading the vegetation indices rasters
temp <- list.files(paste0(getwd(), Input_dir,"/"), pattern = "*.tif",
                   full.names = TRUE)
rs_list<- lapply(temp, stack)

###Preprocessing for cloud removal

#list cloud_dates
cloud_dates <- as.character(substr(list.files(paste0(getwd(),
                            "/input/Sentinel_2_l2a/")), 12, 19))

# list sentinel dirs
cloud_dirs <- list.files(paste0(getwd(), "/input/Sentinel_2_l2a/"),
                         pattern = "*.SAFE", full.names = TRUE)

#defining cloud mask
searchPattern <- "SCL_20m.jp2$"

#reading cloud raster
cloud_path_list <- list()

# listing cloud files
for (i in 1:length(cloud_dirs)){
  cloud_path_list[i] <- list.files(file.path(cloud_dirs[i], "GRANULE"),
          pattern = searchPattern, full.names = TRUE, recursive = TRUE)
}

# creating tifs, you also can skip this step. But not every
#rgdal and gdal version can read jp2 format. so this creates tif formats
pblapply(cloud_path_list, function(x){
  gdal_translate(x, paste0(str_sub(x,end = -4),"tif"))
})

#new cloud mask format
searchPattern2 <- "SCL_20m.tif$"

#listing cloud tif files
cloud_path_list_tif <- list()
for (i in 1:length(cloud_dirs)){
  cloud_path_list_tif[i] <- list.files(file.path(cloud_dirs[i], "GRANULE"),
              pattern = searchPattern2, full.names = TRUE, recursive = TRUE)
}

# stacking clouds
rs_clouds <- stack(cloud_path_list_tif)

#creating correct rasternames for raster indices
singleString <- paste(readLines(paste0(getwd(),
                "/output/Sen2/used_indices.txt")), collapse=" ")
# file to used indices
veg_names <- unlist(strsplit(singleString, " ")) # creating used indices list

#change the stack format to a processable format; nested within itself to stack
rs_unstacked <- lapply(rs_list, function(x){
  r_temp <- raster::unstack(x)
  names(r_temp) <- paste0(names(x),"_", veg_names)
  return(r_temp)
})
rs_list<- unlist(rs_unstacked)
rs <- stack(rs_list)

#save raster names
name <- names(rs)

#creating cloud masks
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

#stack clouds
rs_clouds <- stack(l)

#croping clouds to studyarea
studyarea <- readOGR(area_path)
studyarea <-  spTransform(studyarea, projection(rs_clouds))
rs_clouds <- crop(rs_clouds, studyarea)

#set progressbar
pb = txtProgressBar(min = 0, max = nlayers(rs_clouds), initial = 0, style = 3)

# cloud masking
for (i in 1:nlayers(rs_clouds)){

  #set progressbar
  setTxtProgressBar(pb,i)

  #masking clouds
  t <- rs_clouds[[i]]
  t[t == 3 | t == 8 | t == 9 | t == 10] <- NA # define all cloud related pixel values
  rs_clouds[[i]] <- t # set all cloud related pixel values to NA
}

#setting progressbar
pb = txtProgressBar(min = 0, max = nlayers(rs_clouds), initial = 0, style = 3)

#remove values which are cloud pixels from Rasterstack before calculate statistics
for (i in 1:length(cloud_dates)){

  #set progressbar
  setTxtProgressBar(pb,i)
  print(Sys.time())

  #selecting the correct cloud coverage
  tmp_rs <- rs[[which(str_detect(names(rs), cloud_dates[i]) == TRUE)]]
  if(nlayers(tmp_rs) == 0){
    print(paste0("no layer for date:", cloud_dates[i],"; skipping to next date"))
    next # if the clouds do not overlap with vegetation indices skip
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

#write out the cloud correct vegetation indices raster
terra::writeRaster(ras_masked, paste0(getwd(),"/output/Sen2/rasterVEGIndices_clouds_masked_out.tif"), overwrite=TRUE)

### calculating raster statistics
pblapply(veg_names, function(x){
  print(paste0("start calulating raster statistics for whole time period of Index: ",x," ",Sys.time()))
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

#########END Sentinel 2 cloud removal, raster statistics calculation##########

