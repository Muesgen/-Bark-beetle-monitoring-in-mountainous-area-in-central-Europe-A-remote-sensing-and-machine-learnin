########## Sentinel 1 pca data extraction from observation file##########
#---
# title: "Sentinel 1  extract data"
# Author: Marvin Müsgen
# Description: In this Skripts the data from the Sentinel 1 pca rasters were extracted from point coordinates of observations
#---

#Setting Paths
observations <- "o_total.csv" # name of observation data
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf") # setting working directory
Input_dir <- "/output/Sen1/pca" # inpiut directory
funfold= paste0(getwd(),"/functions") # function folder directory
ext <- "/observations/NP_Boundary.shp" # File in Input_dir: Polygon Shapefile with the extent of the area of Interest
area_path <- paste0(getwd(),"/input/observations/NP_Boundary.shp") #Path to study area shape

#Setting Parameters
pckgs <- c("velox","raster","rgdal", "rgeos","sp","terra","sf", "stringr") # needed packages
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes
xcoordcolnum <- 3 # column number of the x coordinate of your obseravtion date
ycoordcolnum <- 4 # column number of the y coordinate of your observation data
projObs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"  # Projection of the x and y column of observation file
used_pols <- c("VH", "VV", "ratio")
used_orbit <- "asc"

#Loading functions 
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/statistic_function.R"))

#loading Librarys
check_library(pckgs)

#Listing the Dates of Images
files_dates<- as.character(as.Date(substr(list.files(paste0(getwd(),"/input/sentinel_1_preproc/Ascending/"),full.names = FALSE, pattern = ".tif$"), 18, 26), format="%Y%m%d"))
print("File dates are listed")
print(files_dates)

#Listing the month for filtering later the files dates
forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01"))
seq_dates <- seq.Date(forced_start, forced_end, by = "month")
month<- strftime(seq_dates,"%Y-%m")
print("Month are listed")
print(month)

#speeding up the raster package
rasterOptions(maxmemory = 1e+09)

#reading the rasters
temp <- list.files(paste0(getwd(),"/output/Sen1/Ascending/pca/"), pattern = "*.tif", full.names = TRUE)
#rs_list<- lapply(temp, stack)
rs_list<- stack(temp)

#read in the observations
obs <-read.csv(paste0(getwd(),"/input/observations/", observations))
print("observations loaded")
head(obs)
#obs[,5:ncol(obs)] <- NULL 

#defining coordinates, Lat = Y Long = X
xy <- obs[,c(xcoordcolnum,ycoordcolnum)]
xy <-as.list(xy)

#creating a SPatialpointdataframe while checking if projection fits
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

#initialize progressbar
pb = txtProgressBar(min = 0, max = length(used_pols), initial = 0, style = 3)

#extracting rasterdata
for (i in 1:length(used_pols)){
  
  #set Progress
  setTxtProgressBar(pb,i)
  print(Sys.time())
  
  #reading raster Files 
  ras_files <- list.files(paste0(getwd(),"/output/Sen1/Ascending/pca/"), pattern = "*.tif", full.names = TRUE) # list raster
  rs_temp <- ras_files[which(grepl(used_pols[i], ras_files) == TRUE)] #filter Index
  
  # checking if Index include more than 1 Index, teh correct Index is filtered by this if command
  if(length(which(grepl(used_pols[i], used_pols))) > 1){
    var <- used_pols[which(used_pols[i] == used_pols)] # getting the correct Index
    test <- used_pols[which(grepl(used_pols[i],used_pols))] # getting all seleced Index
    fake <- test[which(test != var)] #get wrong Index
    rs_temp <- rs_temp[which(grepl(fake, rs_temp) == FALSE)] # remove wrong index from rasterlist
  }
  
  # Stack raster into correct extraction format
  rs <- stack(rs_temp) # stack
  
  #save colnames for extracted data
  names <- names(rs)
  
  #extracting raster values
  print(paste0("Starting extracting raster data for observation points: ", Sys.time()))
  vx_rs <- velox::velox(rs) # create velox object
  df <- as.data.frame(vx_rs$extract_points(spdf))# extract data at observation points
  colnames(df) <-  names # rename colnames of extracted data
  print(paste0("Finished extracting raster data for observation points: ", Sys.time()))
  obs <- cbind(obs,df)
}

#write out extracted data
write.csv(obs,file = paste0(getwd(),"/output/Sen1/Ascending/pca/PCA_Extracted_data_",used_orbit,"_",start,end,".csv"))
