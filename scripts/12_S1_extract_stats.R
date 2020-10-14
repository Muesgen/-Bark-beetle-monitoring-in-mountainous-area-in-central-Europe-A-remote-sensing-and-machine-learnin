############# SKript for extracting Rasters #############

######Setting parameters#########
observations <- "o_total.csv" # name of observation data
setwd("D:/Marvin/BB_rf") # setting working directory
Input_dir <- "/input" # inpiut directory
Sat_dir <- "/sentinel_1_preproc" # satellite direcotry
sub_dir <- "/Ascending" # sub directory
Output_dir <- "/output/Sen1" # output directory
# nedded packages:
pckgs <- c("velox","raster","rgdal","pbapply","doParallel", "gdalUtils","rgeos","sp", "matrixStats","dplyr","terra")
funfold= paste0(getwd(),"/functions") # function folder directory
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes
ext <- "/observations/NP_Boundary.shp" # File in Input_dir: Polygon Shapefile with the extent of the area of Interest
xcoordcolnum <- 3 # column number of the x coordinate of your obseravtion date
ycoordcolnum <- 4 # column number of the y coordinate of your observation data
projObs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"  # Projection of the x and y column of observation file
pols <- c("VH","VV","ratio") # SAR Polarizations
###################################

########sourcing functions#########
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/statistic_function.R"))
###################################
#checking Library and install necassery packages
check_library(pckgs)

#Listing the Dates of Images
files_dates<- as.character(as.Date(substr(list.files(paste0(getwd(), Input_dir, Sat_dir, sub_dir,"/"),full.names = FALSE, pattern = ".tif$"), 18, 26), format="%Y%m%d"))
print("File dates are listed")
print(files_dates)
#Listing the month for filtering later the files dates
forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))
seq_dates <- seq.Date(forced_start, forced_end, by = "month")
month<- strftime(seq_dates,"%Y-%m")
print("Month are listed")
print(month)
#rsstack <- raster::stack(paste0(getwd(),Input_dir,Sat_dir, sub_dir,"/", list.files(paste0(getwd(), Input_dir, Sat_dir, sub_dir,"/"))))

#speeding up the raster package
rasterOptions(maxmemory = 1e+09)

#reading the rasters
temp <- list.files(paste0(getwd(), Input_dir, Sat_dir, sub_dir,"/"), pattern = "*.tif", full.names = TRUE)
ras_list <- lapply(temp, raster)

# Define clipping extent and res
e <- bbox(ras_list[[1]]@extent) # extent
tr <- c(10.0,10.0) # pixel size

extRas <- ras_list[[1]]
#stacking rasters
if (lapply(ras_list, function(X){ extRas@extent == X@extent }) == TRUE){
  print("Extents of Rasters are not equal, extent of rasters will be alligned")
  #Define clipping extent and res
  e <- bbox(ras_list[[1]]@extent) # extent
  tr <- c(10.0,10.0) # pixel size
  #alignig extent
  r_list<- pbsapply(temp, function(x){
    gdalUtils::align_rasters(x,temp[[1]],dstfile = sub('\\.tif', '_ext.tif', x),nThreads = 7)
  })
  #stackig rasters
  temp2 <- list.files(paste0(getwd(), Input_dir, Sat_dir, sub_dir,"/"), pattern = "*ext.tif", full.names = TRUE)
  rs <- stack(temp2)
}else{
  print("Extents are equal")
  rs <- stack(ras_list)
}

###reading the obeservation shape and setting the projection

#read in the observations
obs <-read.csv(paste0(getwd(),Input_dir,"/observations/", observations))
print("observations loaded")
head(obs)
obs[,5:ncol(obs)] <- NULL
#defining coordinates, Lat = Y Long = X
xy <- obs[,c(xcoordcolnum,ycoordcolnum)]
xy <-as.list(xy)

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
r_crs<- rs[[1]]@crs
spdf <- sp::spTransform(spdf, CRS(proj4string(r_crs)))

#extracting raster values
print(paste0("Starting extracting raster data for observation points: ", Sys.time()))
vx_rs <- velox::velox(rs)
df <- as.data.frame(vx_rs$extract_points(spdf))
print(paste0("Finished extracting raster data for observation points: ", Sys.time()))

#Renaming the colnames to the files dates
if(length(rs@layers) == length(files_dates)){
  colnames(df) <- files_dates
  print("Rasterstack with single Layer was detected. Rename colnames in files_dates")
} else {
  #files_d<- lapply(files_dates, function(x){
  #  replicate(length(rs@layers)/length(files_dates), x) # replicate names for renaming colnames
  #  })
  #print(paste0("Rasterstack with ", length(rs@layers)/length(files_dates)," layer per raster was detected. Renaming colnames  for multibandraster was succesful"))
  #files_d<- unlist(files_d, recursive = FALSE) # unlisting nested lists
  n <- length(rs@layers) / length(files_dates)
  for (i in 1: length(files_dates)){#renaming colnames with dates and polarization
    colnames(df)[n*i] <- paste0(files_dates[i],"ratio")
    colnames(df)[n*i-1] <- paste0(files_dates[i],"VV")
    colnames(df)[n*i-2] <- paste0(files_dates[i],"VH")
  }
}

obs <- cbind(obs,df)

write.csv(obs,file = paste0(getwd(),Output_dir,sub_dir, "/_Extracted_data",start,end,".csv"))
terra::writeRaster(rs, paste0(getwd(),Output_dir,sub_dir, "/_Sen1_Descending_Stack.tif"), overwrite=TRUE)

####### Statistics #########

#splitting data frame into polarization
#calculating the statistics
pblapply(pols,function(x){
  obs_stats <- as.data.frame(matrix(ncol=7,nrow= NROW(obs)))
  headers <-c(paste0(x,"_means"), paste0(x,"_max"), paste0(x,"_min"), paste0(x,"_25q"), 
             paste0(x,"_75q"), paste0(x,"_sd"), paste0(x,"_median"))
  colnames(obs_stats) <- headers
  obs_I <- obs[,grepl(x, colnames(obs))]
  obs_stats[,1] <- rowMeans(obs_I[,1:ncol(obs_I)], 1, na.rm = TRUE) #mean
  obs_stats[,2] <- apply(obs_I[,1:ncol(obs_I)],1, max, na.rm = TRUE) #maximum
  obs_stats[,3] <- apply(obs_I[,1:ncol(obs_I)],1, min, na.rm = TRUE) # min
  obs_quants <- cbind(obs_I, t(apply(obs_I, 1, quantile, c(0.25, .5, .75)))) #calc the quantiles
  obs_stats[,4] <- obs_quants$`25%`  #25 quantile
  obs_stats[,5] <- obs_quants$`75%` # 25 quantile
  obs_stats[,6] <- apply(obs_I[,1:ncol(obs_I)],1, sd, na.rm=TRUE) # standard deviaton
  obs_stats[,7] <- obs_quants$`50%` #median
  write.csv(obs_stats, file = paste0(getwd(),Output_dir,sub_dir, "/observation_stats",start,end,x,".csv"))
  lapply(month, function(y){
    month_stats <- as.data.frame(matrix(ncol=7,nrow= NROW(obs)))
    headers <-c(paste0(x,y,"_means"), paste0(x,y,"_max"), paste0(x,y,"_min"), paste0(x,y,"_25q"), 
                paste0(x,y,"_75q"), paste0(x,y,"_sd"), paste0(x,y,"_median"))
    colnames(month_stats) <- headers
    obs_IM <- obs_I[,grepl(y, colnames(obs_I))]
    month_stats[,1] <- rowMeans(obs_IM[,1:ncol(obs_IM)], 1, na.rm = TRUE) #mean
    month_stats[,2] <- apply(obs_IM[,1:ncol(obs_IM)],1, max, na.rm = TRUE) #maximum
    month_stats[,3] <- apply(obs_IM[,1:ncol(obs_IM)],1, min, na.rm = TRUE) # min
    obs_quants_M <- cbind(obs_IM, t(apply(obs_IM, 1, quantile, c(0.25, .5, .75)))) #calc the quantiles
    month_stats[,4] <- obs_quants_M$`25%`  #25 quantile
    month_stats[,5] <- obs_quants_M$`75%` # 25 quantile
    month_stats[,6] <- apply(obs_IM[,1:ncol(obs_IM)],1, sd, na.rm=TRUE) # standard deviaton
    month_stats[,7] <- obs_quants_M$`50%` #median
    write.csv(month_stats, file = paste0(getwd(),Output_dir,sub_dir, "/observation_stats",x,y,".csv"))
    })
  })

####### raster statistics #######
#n <- match(x, pols)
###renaming the rasterlayer by their polarization
n <- length(names(rs)) / length(files_dates)
name <- names(rs)
for (i in 1: length(files_dates)){#renaming colnames with dates and polarization
  #names(rs)[n*i] <- paste0(names(rs)[n*i],"ratio")
  #names(rs)[n*i-1] <- paste0(names(rs)[n*i-1],"VV")
  #names(rs)[n*i-2] <- paste0(names(rs)[n*i-2],"VH")
  name[n*i] <- paste0(name[n*i],"ratio")
  name[n*i-1] <- paste0(name[n*i-1],"VV")
  name[n*i-2] <- paste0(name[n*i-2],"VH")
}

#s <- lapply(pols, function (x){
#  n <- match(x, pols)
#  names(rs)[which(names(rs) == names(rs)[seq(n, length(names(rs)), length(pols))])] <- paste0(names(rs)[seq(n, length(names(rs)), length(pols))],x)
#})

r_dates<- as.character(as.Date(substr(name, 18, 26), format="%Y%m%d"))

### calculating raster statistics
pblapply(pols, function(x){
  print(paste0("start calulating raster statistics for whole time period of polarization: ",x," ",Sys.time()))
  rs_stat <- rs[[which(grepl(x,name) == TRUE)]]
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
  terra::writeRaster(rs_mean, paste0(getwd(),Output_dir,sub_dir,"/",x,"_mean_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_max, paste0(getwd(),Output_dir,sub_dir,"/",x,"_max_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_min, paste0(getwd(),Output_dir,sub_dir,"/",x,"_min_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_75q, paste0(getwd(),Output_dir,sub_dir,"/",x,"_75q_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_25q, paste0(getwd(),Output_dir,sub_dir,"/",x,"_25q_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_sd, paste0(getwd(),Output_dir,sub_dir,"/",x,"_sd_from",start,"_to_",end,".tif"), overwrite=TRUE)
  terra::writeRaster(rs_median, paste0(getwd(),Output_dir,sub_dir,"/",x,"_median_from",start,"_to_",end,".tif"), overwrite=TRUE)
  rm(rs_mean,rs_max,rs_min,rs_75q,rs_25q,rs_sd, rs_median)
  print(paste0("Finish writing raster statistics for whole time period: ",x," ",Sys.time()))
  pblapply(month, function(y){
    m_dates<- as.character(as.Date(substr(names(rs_stat), 18, 26), format="%Y%m%d"))
    index <- which(m_dates %in% m_dates[grepl(y, m_dates)])# getting the Index of selected rasters in time period
    rs_month <- rs_stat[[index]]
    print(paste0("start calulating raster statistics for month ",y," of polarization: ",x," ",Sys.time()))
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
    print(paste0("Finished calculating raster statistics for month ",y," of polarization: ",x," ",Sys.time()))
    print(paste0("start writing raster statistic for whole time period: ", x, " ",Sys.time()))
    terra::writeRaster(rsm_mean, paste0(getwd(),Output_dir,sub_dir,"/",x,"_mean_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_max, paste0(getwd(),Output_dir,sub_dir,"/",x,"_max_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_min, paste0(getwd(),Output_dir,sub_dir,"/",x,"_min_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_75q, paste0(getwd(),Output_dir,sub_dir,"/",x,"_75q_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_25q, paste0(getwd(),Output_dir,sub_dir,"/",x,"_25q_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_sd, paste0(getwd(),Output_dir,sub_dir,"/",x,"_sd_from ",y,".tif"), overwrite=TRUE)
    terra::writeRaster(rsm_median, paste0(getwd(),Output_dir,sub_dir,"/",x,"_median_from ",y,".tif"), overwrite=TRUE)
    rm(rs_month,rsm_mean,rsm_max,rsm_min,rsm_75q,rsm_25q,rsm_sd, rsm_median)
    print(paste0(" raster statistics for month ", y,"of polarization ",x," are written succesfully"))
  })
  rm(rs_stat)
})

###################possible solutions##########################
#}
#gdalwarp(f, sub('\\.tif', '_clipped.tif', f), tr=tr, 
#         r='bilinear', te=c(e), multi=TRUE)
#system.time(sapply(ras_list, function(f){ 
#  gdalwarp(f, sub('\\.tif', '_cliped.tif', f), tr=tr, r='bilinear', 
#           te=c(e), multi=TRUE)
#))
#pbsapply(temp, function(x){
#  gdalUtils::align_rasters(x,temp[[1]],dstfile = sub('\\.tif', '_ext.tif', x),nThreads = 7)
#})
#extRas@extent
#else{
#  print("Extents are equal")
#}

#Checking if Extents are the same
#extRas <- ras_list[[1]]
#if (lapply(ras_list, function(X){ extRas@extent == X@extent }) == TRUE){
#  print("Extents of Rasters are not equal, extent of rasters will be alligned in 2 steps")
#  shp <- readOGR(dsn=paste0(getwd(),Input_dir,ext)) #reading shapefile with area of iterest
#  shp <- sp::spTransform(shp, CRS(proj4string(extRas))) #setting the crs argument
#  print("Cropping the rasters")
#  ras_list <- pbapply::pblapply(ras_list, function(X){
#    raster::crop(X,shp) #cropping the rasters
#    writeRaster(X, paste0(getwd(), Input_dir, Sat_dir, sub_dir,"/test",X,".tif"))
#  })
#  print("Resampling the rasters")
#  UseCores <- detectCores() -1
#  cl <- makeCluster(UseCores)
#  registerDoParallel(cl) 
#  ras_list  <- pbapply::pblapply(ras_list, function(X){
#      raster::resample(extRas,X, method="bilinear")
#  })
#  print("Extent of the Rasters are equal")
#}


#lapply(ras_list, function(X){
#raster::resample(extent,X, method="bilinear")
#})
#e= c(8.183057,8.371883,48.49067,48.69494)
#y <- gdalwarp(X, ras_list[[2]], r='bilinear', 
#              te=c(e), multi=TRUE)##

#lapply(ras_list,function(X){
#  ras_list[[2]]@extent <- alignExtent(extent@extent, ras_list[[2]], snap = "near")
#  return(r_align)})
#X = ras_list[[2]]
#rs<- stack(ras_list[[1]],ras_list[[2]])
#rs[[2]]


#obs_VV <- obs[,grepl("VV", colnames(obs))]
#obs_VH <- obs[,grepl("VH", colnames(obs))]