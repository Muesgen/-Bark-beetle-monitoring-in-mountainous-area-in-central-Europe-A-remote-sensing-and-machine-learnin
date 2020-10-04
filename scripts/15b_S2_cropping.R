#---
#title: "Extraction of vegetation indices from Sentinel 2A scenes for a particular study area considering cloud coverage"
#authors: "Dirk Zeuss, Fortunata Msoffe"
#date: "1 September 2020"
#---
rm(list = ls(all = TRUE)) # clean workspace
setwd("D:/Marvin/BB_rf")
pckgs <- c("raster", "rgdal","rgeos","stringr","terra","sp","gdalUtils") # needed packages
funfold= paste0(getwd(),"/functions") 
area <- paste0(getwd(),"/input/observations/NP_Boundary.shp")
"D:\Marvin\BB_rf\input\observations\NP_Boundary.shp"
Input_dir <- paste0(getwd(),"/input/Sentinel_2_l2a/")
# Set sentinel tiles to process (all scenes matching the string below in the sentinel data folder will be processed)
sentinel_tile <- "32UMU" 

source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/check_overlap.R"))
source(paste0(funfold,"/calc_indices.R"))
source(paste0(funfold,"/gdal_resample.R"))

## Praeambel ---------------------------------------------------------------

source(paste0(funfold,"/check_library.R"))
check_library(pckgs)

# Set study area -------------------------------------------------------

studyarea <- readOGR(area) # Vector polygon defining the study area within the sentinel tile set above
#plot(studyarea)

## Set and get paths ---------------------------------------------------------------

# Set path to the directory with sentinel scenes
path_sentinel_data <- Input_dir
filenames <- list.files(path_sentinel_data)
files_dates <- as.character(substr(list.files(path_sentinel_data), 12, 19))

# Get paths to folders of sentinel scenes
paths_sentinel_tiles <- list.files(path_sentinel_data, pattern = sentinel_tile, full.names = TRUE, recursive = FALSE)
# remove potential non-sentinel paths in the data folder 
paths_sentinel_tiles <- grep(".SAFE", paths_sentinel_tiles, value = TRUE)

## Get a rasterlist for checking if they overlapping the studyarea --------------------------------

#create a rasterlist for checking
rlist<- lapply(paths_sentinel_tiles, function(x){
  searchPattern_10m <- "_10m.jp2$"
  path_band_10m <- list.files(file.path(x, "GRANULE"), pattern = searchPattern_10m, full.names = TRUE, recursive = TRUE)[1]
})
#create a rasterstack for checking
#RS_area_check <- stack(rlist)

#checking if the rasters overlap
#check_overlap(RS_area_check, studyarea)

## Batch processing -----------------------------------------------------
# Loop over all scenes in the sentinel data folder to calculate vegetation indices and cloud coverage for the study area


# create progressbar

pb = txtProgressBar(min = 0, max = length(paths_sentinel_tiles), initial = 0, style = 3)

for(i in 1:length(paths_sentinel_tiles)){ # iterate over all sentinel scene folders in the sentinel data folder
  
  # Select bands  -------------------------------------------------------
  setTxtProgressBar(pb,i)
  
  # Set the search pattern for bands to process. In this case only the 10m bands are selected.
  searchPattern_10m <- "_10m.jp2$" 
  searchPattern_20m <- "_20m.jp2$"
  
  # Get paths with images containing "searchPattern_10m"
  paths_bands_10m <- list.files(file.path(paths_sentinel_tiles[i], "GRANULE"), pattern = searchPattern_10m, full.names = TRUE, recursive = TRUE)
  paths_bands_20m <- list.files(file.path(paths_sentinel_tiles[i], "GRANULE"), pattern = searchPattern_20m, full.names = TRUE, recursive = TRUE)
  bandnames_10m <- c("B02", "B03","B04","B08")
  bandnames_20m <- c("B05","B8A","B11","B12")
  paths_bands_10m<- lapply(bandnames_10m,function(x){
    grep(x, paths_bands_10m, value = TRUE)
  })
  paths_bands_20m<- lapply(bandnames_20m,function(x){
    grep(x, paths_bands_20m, value = TRUE)
  })
  bands_10m <- stack(paths_bands_10m)
  bands_20m <- stack(paths_bands_20m)
  #bands_20m <- resample(bands_20m,bands_10m[[1]], method="ngb")
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
    # Please check your temp memory. It is full!
  }
  bands_resampl <- stack(l)
  # Create stack of bands
  bands <- stack(bands_10m, bands_resampl)
  print("bands stacked")
  ################possible Error when a gdal version without the plugin (Jpeg2000openJPeg) is not installed. change the linked gdal version to a 3.0 version

    # Rename bands to band name and resolution (only for long names)
  regs_matched <- gregexpr("_..._.*$", names(bands)) 
  tempname <- as.character(regmatches(names(bands), regs_matched))
  names(bands) <- substr(tempname, 2, nchar(tempname))
  
  # Reproject, crop and clip  -------------------------------------------------------
  
  # Reproject study area to Sentinel CRS
  studyarea <-  spTransform(studyarea, projection(bands))
  
  # Crop and clip Sentinel tile with study area polygon
  bands_crop <- crop(bands, studyarea)
  bands_mask <- mask(bands_crop, studyarea, updatevalue = NA, updateNA= TRUE)
  
  print("bands masked")
  # Index calculation -------------------------------------------------------
  veg_indices <- c("LWCI","VARI","NDTI","RI","BI","SI", "TGI", "GLI","NGRDI", "ExGR","VEG","CIVE","CCCI","CVI","GNDVI","MCARI","NDVI","NDRE","MSI","NDVWI","BRI","LCI","AFRI1600","MSR670","CIrededge","CIgreen","MVI","NDMI","DSWI4")
  RS_indices <- calc_Indices(bands_crop, rgbi = veg_indices)
  print("indices calculated")
  
  # Calculate summary statistics for each index layer
  #indices_res <- sapply(names(Index_layer), function (x) cellStats(Index_layer[[x]], stat = "mean")) # Note that the mean vegetation index over the whole study area is calculated here. Change, if you need other summary statistics.
  
  
  # Cloud coverage in study area   -------------------------------------------------------
  
  # Get path to cloud mask
  path_cloud_mask <- list.files(file.path(paths_sentinel_tiles[i], "GRANULE"), pattern = "MSK_CLOUDS_B00.gml", full.names = TRUE, recursive = TRUE)
  
  # Import cloud mask
  cloud_mask <- readOGR(path_cloud_mask, disambiguateFIDs = TRUE, verbose = FALSE)
  
  # Mask clouds in study area for coverage calculation
  clouds <- mask(bands_mask[[1]], cloud_mask, updatevalue = NA)
  
  # Calculate cloud coverage
  all_pixels_study_area <- length(na.omit(raster::values(bands_mask[[1]]))) # number of all pixels in the study area
  cloud_pixels_study_area <- length(na.omit(clouds@data@values)) # number of cloud pixels in the study area
  
  cloud_coverage <- cloud_pixels_study_area / all_pixels_study_area
  
  names(cloud_coverage) <- "cloud_coverage"
  
  print("clouds calculated")
  # Cloud coverage in full scene   -------------------------------------------------------
  
  # Get path to metadata
  path_overall_cloud_coverage <- list.files(file.path(paths_sentinel_tiles[i], "HTML"), pattern = "UserProduct_index.html", full.names = TRUE, recursive = TRUE)
  
  # Read in metadata
  metadata <- readChar(path_overall_cloud_coverage, file.info(path_overall_cloud_coverage)$size)
  
  # Extract cloud coverage of the full scene
  search_pattern <- ".*(Cloud Coverage Assessment: )"
  temp_string <- sub(search_pattern, "", metadata) 
  cloud_cover_full_scene <- as.numeric(substr(temp_string, 1,5)) / 100
  
  
  # Visual checks  -------------------------------------------------------
  
  # Visually check cloud coverage calculation
  # graphics.off()
  #plotRGB(bands_10m, r = 3, g = 2, b = 1, stretch = "lin")
  #plot(studyarea, add = TRUE)
  #plot(cloud_mask, add=TRUE)
  #plot(clouds, add=TRUE, col="red")
  
  # Visually check index layer
  # title(main=i)
  
  # Write out visual checks to disk
  ##dir.create(file.path(getwd(), "output/graphics/indices"), recursive = T, showWarnings = FALSE) 
  #dir.create(file.path(getwd(), "output/graphics/cloud_coverage"), recursive = T, showWarnings = FALSE) 
  
  jpeg(filename = paste0(getwd(), "/output/graphics/indices","/",str_sub(filenames[i],end = -6), ".jpg"), 
       width = 1600, height = 1400, pointsize = 24)
  plot(RS_indices)
  dev.off()
  
  jpeg(filename = paste0(getwd(), "/output/graphics/cloud_coverage","/",str_sub(filenames[i],end = -6), ".jpg"), width = 800, height = 600)
  plotRGB(bands_10m, r = 4, g = 3, b = 2, stretch = "lin")
  plot(studyarea, add = TRUE)
  plot(clouds, add=TRUE, col="red")
  dev.off()
  print("images printed")
  
  # Collect results  -------------------------------------------------------
  
  # Show some output on the status of the batch processing
  #cat(names(indices_res), ":", indices_res, ",", 
  #    "cloud coverage:", cloud_coverage,  ",", 
  #    "cloud coverage full scene:", cloud_cover_full_scene,  
  #    "\n")
  
  # Concatenate index results and corresponding Sentinel scene
  #result[[(i)]] <- c(RS_indices, cloud_coverage, cloud_cover_full_scene = cloud_cover_full_scene)
  
  # Clean up
  #rm(cloud_mask, clouds, bands_10m); gc() # if you run into memory issues, try this line.
  fname <- paste0(getwd(),"/output/Sen2/",str_sub(filenames[i],end = -6),"_indices.tif")
  terra::writeRaster(RS_indices, filename = fname)
  print("raster are written out")
}

## End  --------------------------------------------------------------------------------