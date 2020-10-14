########## Sentinel 2 Indices calculation and cropping ##########
#---
# title: "Sentinel 2 Indices calculation and cropping"
# Author: Marvin Müsgen
# Description: This Script calculate vegetation indices on Sentinel 2 images and crop then to exten of interest
#---

#Set paths
setwd("D:/Marvin/BB_rf") # working directory
pckgs <- c("raster", "rgdal","rgeos","stringr","terra","sp","gdalUtils") # needed packages
funfold <-  paste0(getwd(),"/functions") #path to function folder
area <- paste0(getwd(),"/input/observations/NP_Boundary.shp") # path to area of interes
Input_dir <- paste0(getwd(),"/input/Sentinel_2_l2a/")# path to the input directory where senitnel 2 l2a images are located

# Set Variables
sentinel_tile <- "32UMU" # tiles of interes
pckgs <- c("raster", "rgdal","rgeos","stringr","terra","sp","gdalUtils") # needed packages
vegetation_indices <- c("NDRE", "DSWI4", "MSI", "CIrededge","MSR670", "NGRDI", "MVI", "ExGR", "GLI", "MCARI", "GNDVI", "NDVI") # Indices which you want to calculate

# loading functions
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/calc_indices.R"))
source(paste0(funfold,"/gdal_resample.R"))

# reading librarys
check_library(pckgs)

# reading area of interest
studyarea <- readOGR(area)

# creating names and dates of the images
path_sentinel_data <- Input_dir
filenames <- list.files(path_sentinel_data)
files_dates <- as.character(substr(list.files(path_sentinel_data), 12, 19))

# Get paths to folders of sentinel scenes
paths_sentinel_tiles <- list.files(path_sentinel_data, pattern = sentinel_tile, full.names = TRUE, recursive = FALSE)

# remove potential non-sentinel paths in the data folder 
paths_sentinel_tiles <- grep(".SAFE", paths_sentinel_tiles, value = TRUE)

#create a rasterlist for checking
rlist<- lapply(paths_sentinel_tiles, function(x){
  searchPattern_10m <- "_10m.jp2$"
  path_band_10m <- list.files(file.path(x, "GRANULE"), pattern = searchPattern_10m, full.names = TRUE, recursive = TRUE)[1]
})

# create progressbar
pb = txtProgressBar(min = 0, max = length(paths_sentinel_tiles), initial = 0, style = 3)

#starting processing
for(i in 1:length(paths_sentinel_tiles)){ # iterate over all sentinel scene folders in the sentinel data folder
  
  # set progressbar
  setTxtProgressBar(pb,i)
  
  # Set the search pattern for bands to process.
  searchPattern_10m <- "_10m.jp2$" 
  searchPattern_20m <- "_20m.jp2$"
  
  # Get paths with images containing "searchPattern_10m"
  paths_bands_10m <- list.files(file.path(paths_sentinel_tiles[i], "GRANULE"), pattern = searchPattern_10m, full.names = TRUE, recursive = TRUE)
  paths_bands_20m <- list.files(file.path(paths_sentinel_tiles[i], "GRANULE"), pattern = searchPattern_20m, full.names = TRUE, recursive = TRUE)
  
  # define band layers of interest
  bandnames_10m <- c("B02", "B03","B04","B08") #10m resolution
  bandnames_20m <- c("B05","B8A","B11","B12") #20m resolution
  
  # get Paths of bandnames_10
  paths_bands_10m<- lapply(bandnames_10m,function(x){
    grep(x, paths_bands_10m, value = TRUE)
  })
  
  #get Paths of bandnames_10
  paths_bands_20m<- lapply(bandnames_20m,function(x){
    grep(x, paths_bands_20m, value = TRUE)
  })
  
  #stack bands into resolution stacks
  bands_10m <- stack(paths_bands_10m)
  bands_20m <- stack(paths_bands_20m)
  
  #resampling raster into 10m resolution
  for (j in 1:nlayers(bands_20m)){
    print(paste(Sys.time(), "Resampling raster", j, "of", nlayers(bands_20m),"..."))
    if (j == 1){
      l <-list()
      l[[1]] <- gdal_resample(bands_20m[[j]],bands_10m[[1]], method = "near")
      names(l[[1]]) <- names(bands_20m)[[1]]
    }
    else{
      l[[j]] <- gdal_resample(bands_20m[[j]],bands_10m[[1]], method = "near")
      names(l[[j]]) <- names(bands_20m)[[j]]
    }
    # if an error occurs:  Error in rgdal::putRasterData(x@file@transient, v, band = 1, offset = off) : 
    #Failure during raster IO
    # Please check your temp memory. It is full! tempfile()
  }
  
  # stack resampled bands
  bands_resampl <- stack(l)
  
  # Create stack of  all bands off interest
  bands <- stack(bands_10m, bands_resampl)
  print("bands stacked")
  ##ATTENTION:
  #possible Error when a gdal version without the plugin (Jpeg2000openJPeg) is not installed. 
  #change the linked gdal version to a 3.0 version

  # Rename bands to band name and resolution (only for long names)
  regs_matched <- gregexpr("_..._.*$", names(bands)) 
  tempname <- as.character(regmatches(names(bands), regs_matched))
  names(bands) <- substr(tempname, 2, nchar(tempname))
  
  # Reproject study area to Sentinel CRS
  studyarea <-  spTransform(studyarea, projection(bands))
  
  # Crop and clip Sentinel tile with study area polygon
  bands_crop <- crop(bands, studyarea)
  bands_mask <- mask(bands_crop, studyarea, updatevalue = NA, updateNA= TRUE)
  
  #checking if Values are in the area of itnerest, if not the loop jump to start
  if (bands_mask@data@min == 0 && bands_mask@data@max == 0){
    print(paste0("Sentinel Image: ",filenames[i]," does not have any Values in studyarea. Loop continues with next image"))
    next
  }
  print("bands masked")
  
  # Set vegetation index and calculate them
  veg_indices <- vegetation_indices
  RS_indices <- calc_Indices(bands_crop, rgbi = veg_indices) #new indices calc function, input to function must be changed
  print("indices calculated")
  
  # Write Indices as Images to disk
  jpeg(filename = paste0(getwd(), "/output/graphics/indices","/",str_sub(filenames[i],end = -6), ".jpg"), 
       width = 1600, height = 1400, pointsize = 24) # open jpg data
  plot(RS_indices) # print in jpg data
  dev.off() # close and save data
  print("images printed")
  
  #create output file
  fname <- paste0(getwd(),"/output/Sen2/indices/",str_sub(filenames[i],end = -6),"_indices.tif")
  
  #write indices as raster out
  terra::writeRaster(RS_indices, filename = fname, overwrite = TRUE)
  print("raster are written out")
}

#writing used indices out in a text file for later use
fileConn <- file(paste0(getwd(),"/output/Sen2/used_indices.txt"))
writeLines(vegetation_indices, fileConn)
close(fileConn)

