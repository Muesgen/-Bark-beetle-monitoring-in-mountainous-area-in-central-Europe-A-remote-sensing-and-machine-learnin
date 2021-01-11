############# SKript for extracting Rasters #############

######Setting parameters#########
observations <- "barkbeetle_obs_x_to_2018_2class.csv" # name of observation data
#/Volumes/MarvinLaCie/Marvin/BB_rf/input/observations/Befall_gesamt/barkbeetle_obs_x_to_2018_2class.csv
setwd("D:/Marvin/BB_rf") # setting working directory
Input_dir <- "/input" # inpiut directory
Sat_dir <- "/sentinel_1_preproc2" # satellite direcotry
sub_dir <- "/Descending" # sub directory
Output_dir <- "/output/Sen1_2" # output directory
# nedded packages:
pckgs <- c("velox","raster","rgdal","pbapply","doParallel", "gdalUtils","rgeos","sp", "matrixStats","dplyr","terra")
funfold= paste0(getwd(),"/functions") # function folder directory
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes
ext <- "/observations/NP_Boundary.shp" # File in Input_dir: Polygon Shapefile with the extent of the area of Interest
xcoordcolnum <- 3 # column number of the x coordinate of your obseravtion date
ycoordcolnum <- 4 # column number of the y coordinate of your observation data
projObs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"  # Projection of the x and y column of observation file
pols <- c("VH","VV") # SAR Polarizations
###################################

########sourcing functions#########
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/statistic_function.R"))
###################################
#checking Library and install necassery packages
check_library(pckgs)

#Listing the Dates of Images
files_dates<- as.character(as.Date(substr(list.files(paste0(getwd(), Input_dir, Sat_dir, sub_dir,"/"),full.names = FALSE, pattern = "ext.tif$"), 18, 26), format="%Y%m%d"))
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
nlayers(rs)

###reading the obeservation shape and setting the projection

#read in the observations
#obs <-read.csv(paste0(getwd(),Input_dir,"/observations/", observations))
obs <- read.csv("E:/Marvin/BB_rf/input/observations/Befall_gesamt/barkbeetle_obs_2018_3class.csv")
print("observations loaded")
head(obs)
obs[,5:ncol(obs)] <- NULL
#defining coordinates, Lat = Y Long = X
obs[,xcoordcolnum] <- as.numeric(obs[,xcoordcolnum])
obs[,ycoordcolnum] <- as.numeric(obs[,ycoordcolnum])

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
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  print("Spatialdataframe is created")
  head(spdf)
}
#transforming into correct projection
dummy <- readOGR("E:/Marvin/NP_Boundary.shp")
dummy@proj4string@projargs

temp2 <- list.files(paste0(getwd(), Input_dir, Sat_dir, sub_dir,"/"), pattern = "*ext.tif", full.names = TRUE)
temp
rs <- stack(temp2)
#r_crs<- rs[[1]]@crs
#spdf <- sp::spTransform(spdf, CRS(proj4string(r_crs)))

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
    #colnames(df)[n*i] <- paste0(files_dates[i],"ratio")
    colnames(df)[n*i] <- paste0(files_dates[i],"VV")
    colnames(df)[n*i-1] <- paste0(files_dates[i],"VH")
  }
}
length(colnames(df))
names(rs)
obs <- cbind(obs,df)


#write.csv(obs,file = paste0(getwd(),Output_dir,sub_dir, "/_Extracted_data_2",start,end,".csv"))
write.csv(obs,file = paste0("E:/Marvin/BB_rf/output/Sen1_3/_Extracted_data_3",start,end,".csv"))
"E:\Marvin\BB_rf\output\Sen1_3"

terra::writeRaster(rs, paste0(getwd(),Output_dir,sub_dir, "/_Sen1_Ascending_Stack2.tif"), overwrite=TRUE)

####### Statistics #########

#splitting data frame into polarization
#calculating the statistics
pblapply(pols,function(x){
  obs_stats <- as.data.frame(matrix(ncol=4,nrow= NROW(obs)))
  headers <-c(paste0(x,"_means"), paste0(x,"_max"), paste0(x,"_min"), paste0(x,"_sd"))
  colnames(obs_stats) <- headers
  obs_I <- obs[,grepl(x, colnames(obs))]
  obs_stats[,1] <- rowMeans(obs_I[,1:ncol(obs_I)], 1, na.rm = TRUE) #mean
  obs_stats[,2] <- apply(obs_I[,1:ncol(obs_I)],1, max, na.rm = TRUE) #maximum
  obs_stats[,3] <- apply(obs_I[,1:ncol(obs_I)],1, min, na.rm = TRUE) # min
  obs_stats[,4] <- apply(obs_I[,1:ncol(obs_I)],1, sd, na.rm=TRUE) # standard deviaton
  #write.csv(obs_stats, file = paste0(getwd(),Output_dir,sub_dir, "/observation_stats",start,end,x,"2.csv"))
  write.csv(obs_stats, file = paste0("E:/Marvin/BB_rf/output/Sen1_3/observation_stats",start,end,x,"3.csv"))
  lapply(month, function(y){
    month_stats <- as.data.frame(matrix(ncol=4,nrow= NROW(obs)))
    headers <-c(paste0(x,y,"_means"), paste0(x,y,"_max"), paste0(x,y,"_min"), paste0(x,y,"_sd"))
    colnames(month_stats) <- headers
    obs_IM <- obs_I[,grepl(y, colnames(obs_I))]
    month_stats[,1] <- rowMeans(obs_IM[,1:ncol(obs_IM)], 1, na.rm = TRUE) #mean
    month_stats[,2] <- apply(obs_IM[,1:ncol(obs_IM)],1, max, na.rm = TRUE) #maximum
    month_stats[,3] <- apply(obs_IM[,1:ncol(obs_IM)],1, min, na.rm = TRUE) # min
    month_stats[,4] <- apply(obs_IM[,1:ncol(obs_IM)],1, sd, na.rm=TRUE) # standard deviaton
    #write.csv(month_stats, file = paste0(getwd(),Output_dir,sub_dir, "/observation_stats",x,y,"2.csv"))
    write.csv(month_stats, file = paste0("E:/Marvin/BB_rf/output/Sen1_3/observation_stats",x,y,"3.csv"))
    
    })
  })

####### raster statistics #######
###renaming the rasterlayer by their polarization
n <- length(names(rs)) / length(files_dates)
name <- names(rs)
for (i in 1: length(files_dates)){#renaming colnames with dates and polarization
  #names(rs)[n*i] <- paste0(names(rs)[n*i],"ratio")
  #names(rs)[n*i-1] <- paste0(names(rs)[n*i-1],"VV")
  #names(rs)[n*i-2] <- paste0(names(rs)[n*i-2],"VH")
  #name[n*i] <- paste0(name[n*i],"ratio")
  name[n*i] <- paste0(name[n*i],"VV")
  name[n*i-1] <- paste0(name[n*i-1],"VH")
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
  rs_sd <- clusterR(rs_stat,fqsd)

  endCluster()
  print(paste0(" raster statistics are calculated succesfully for whole time period of polarization: ",x," ",Sys.time()))
  print(paste0("start writing raster statistics for whole time period: ",x," ",Sys.time()))
  terra::writeRaster(rs_mean, paste0(getwd(),Output_dir,sub_dir,"/",x,"_mean_from",start,"_to_",end,"2.tif"), overwrite=TRUE)
  terra::writeRaster(rs_max, paste0(getwd(),Output_dir,sub_dir,"/",x,"_max_from",start,"_to_",end,"2.tif"), overwrite=TRUE)
  terra::writeRaster(rs_min, paste0(getwd(),Output_dir,sub_dir,"/",x,"_min_from",start,"_to_",end,"2.tif"), overwrite=TRUE)
  terra::writeRaster(rs_sd, paste0(getwd(),Output_dir,sub_dir,"/",x,"_sd_from",start,"_to_",end,"2.tif"), overwrite=TRUE)
  rm(rs_mean,rs_max,rs_min,rs_sd)
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
    rsm_sd <- clusterR(rs_month,fqsd)
    print("median...")
    endCluster()
    print(paste0("Finished calculating raster statistics for month ",y," of polarization: ",x," ",Sys.time()))
    print(paste0("start writing raster statistic for whole time period: ", x, " ",Sys.time()))
    terra::writeRaster(rsm_mean, paste0(getwd(),Output_dir,sub_dir,"/",x,"_mean_from ",y,"2.tif"), overwrite=TRUE)
    terra::writeRaster(rsm_max, paste0(getwd(),Output_dir,sub_dir,"/",x,"_max_from ",y,"2.tif"), overwrite=TRUE)
    terra::writeRaster(rsm_min, paste0(getwd(),Output_dir,sub_dir,"/",x,"_min_from ",y,"2.tif"), overwrite=TRUE)
    terra::writeRaster(rsm_sd, paste0(getwd(),Output_dir,sub_dir,"/",x,"_sd_from ",y,"2.tif"), overwrite=TRUE)
    rm(rs_month,rsm_mean,rsm_max,rsm_min,rsm_sd)
    print(paste0(" raster statistics for month ", y,"of polarization ",x," are written succesfully"))
  })
  rm(rs_stat)
})

