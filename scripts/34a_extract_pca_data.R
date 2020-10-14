########## Sentinel 2 data extraction from observation file##########
#---
# title: "Sentinel 2  extract data"
# Author: Marvin Müsgen
# Description: In this Skripts the data from the pca rasters were extracted from point coordinates of observations
#---

#Setting Paths
observations <- "o_total.csv" # name of observation data
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf") # setting working directory
Input_dir <- "/output/Sen2/pca" # inpiut directory
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

#Loading functions 
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/statistic_function.R"))

#loading Librarys
check_library(pckgs)

#Listing Dates
files_dates<- as.character(substr(list.files(paste0(getwd(),"/output/Sen2/Indices/"),full.names = FALSE, pattern = ".tif$"), 12, 19)) # listing dates of images
month <- as.character(substr(list.files(paste0(getwd(), "/output/Sen2/Indices/"),full.names = FALSE, pattern = ".tif$"), 12, 17)) # listing month
month <- unique(month)
print("File dates are listed")
#print(files_dates)

#speeding up the raster package
rasterOptions(maxmemory = 1e+09)

#reading the rasters
temp <- list.files(paste0(getwd(), Input_dir,"/"), pattern = "*.tif", full.names = TRUE)
rs_list<- lapply(temp, stack)
#rs_list<- stack(temp)

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

#creating correct rasternames
singleString <- paste(readLines(paste0(getwd(),"/output/Sen2/used_indices.txt")), collapse=" ")
veg_names <- unlist(strsplit(singleString, " "))

#initialize progressbar
pb = txtProgressBar(min = 0, max = length(veg_names), initial = 0, style = 3)

#extracting rasterdata
for (i in 1:length(veg_names)){
  
  #set Progress
  setTxtProgressBar(pb,i)
  print(Sys.time())
  
  #reading raster Files 
  ras_files <- list.files(paste0(getwd(), Input_dir,"/"),full.names = FALSE, pattern = ".tif$") # list raster
  rs_temp <- ras_files[which(grepl(veg_names[i], ras_files) == TRUE)] #filter Index
  
  # checking if Index include more than 1 Index, teh correct Index is filtered by this if command
  if(length(which(grepl(veg_names[i],veg_names))) > 1){
    var <- veg_names[which(veg_names[i] == veg_names)] # getting the correct Index
    test <- veg_names[which(grepl(veg_names[i],veg_names))] # getting all seleced Index
    fake <- test[which(test != var)] #get wrong Index
    rs_temp <- rs_temp[which(grepl(fake, rs_temp) == FALSE)] # remove wrong index from rasterlist
  }
  
  # Stack raster into correct extraction format
  rs <- stack(paste0(getwd(), Input_dir,"/", rs_temp)) # stack
  
  #save colnames for extracted data
  names <-names(rs)
  
  #extracting raster values
  print(paste0("Starting extracting raster data for observation points: ", Sys.time()))
  vx_rs <- velox::velox(rs) # create velox object
  df <- as.data.frame(vx_rs$extract_points(spdf))# extract data at observation points
  colnames(df) <-  names # rename colnames of extracted data
  print(paste0("Finished extracting raster data for observation points: ", Sys.time()))
  obs <- cbind(obs,df)
}

#writing out pca stats
write.csv(obs,file = paste0(getwd(),"/output/Sen2/pca/PCA_Extracted_data",start,end,".csv"))
