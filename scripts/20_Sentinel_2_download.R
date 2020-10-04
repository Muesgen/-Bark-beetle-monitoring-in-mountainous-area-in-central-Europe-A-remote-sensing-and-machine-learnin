########## Sentinel 2 Download ##########
#---
# title: "Sentinel 2  Download"
# Author: Marvin Müsgen
# Description: This Script Downloads Sentinel 2 l2a images from the copernicus scihub
#---

# Set Paths
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf")
setwd("D:/Marvin/BB_rf") # working directory
funfold <- paste0(getwd(),"/functions") # path to function folder
area <- paste0(getwd(),"/input/observations/NP_Boundary.shp") # Path to area of interest
path_sentinel_data <- paste0(getwd(),"/input/Sentinel_2_l2a") # Set data directory for sentinel files

#Set Variables
pckgs <- c("sen2r", "rgdal", "sf") # needed packages
tiles <- "32UMU" # Sentinel tiles which you want to download
cloudcover <- 100 # cloudcover Filtering
start_date <- "2018-04-01"
end_date <- "2018-09-30"

#load functions
source(paste0(funfold,"/check_library.R"))

# reading librarys
check_library(pckgs)

# reading studyarea
studyarea <- readOGR(area) # read studyarea

# create a list of available senitnel 2 images
sen_list <- s2_list(spatial_extent = sf::st_as_sf(studyarea), 
                    tile = tiles, 
                    time_interval = as.Date(c(start_date, end_date)), 
                    level = "L2A",
                    max_cloud = cloudcover)

# Download scenes
s2_download(s2_prodlist = sen_list,
            downloader = "builtin",
            apihub = NA,
            outdir = path_sentinel_data,
            order_lta = TRUE,
            overwrite = FALSE)

#create request object for Download from long term archive
l <- safe_is_online("/Users/grasprak/.sen2r/lta_orders/lta_20200922_160215.json") #command for requesting if its online
n <- which(l == FALSE) # variable for checking if its online


#Download from long term archive; repeats requesting until all files are ready for Download
while(l[n] != TRUE){
  
  #checking if products are online
  print("Requesting last products from copernicus hub")
  l <- safe_is_online("/Users/grasprak/.sen2r/lta_orders/lta_20200922_160215.json")# store information into variable
  
  if(l[n] == TRUE){
    
    #if images are ready for download the download starts for the images
    s2_download(s2_prodlist = sen_list,
                downloader = "builtin",
                apihub = NA,
                outdir = path_sentinel_data,
                order_lta = TRUE,
                overwrite = FALSE)
    print("worked")
    
    if(length(which(l == TRUE))){
      
      # if all images were downloaded succesfully
      print("All files were downloaded")
    }
    else{
      
      # if not all images which were requested from the long term archive
    n <- which(l == FALSE) # geting Files which are not downloaded yet
    }
  }
  
  else{
    
    #if images are not ready for download it will check in a minute again
    print(paste0(Sys.time()," failed, try again in a minute"))
  }
  Sys.sleep(60)
}

