########## Sentinel 1 Raster statistics and Principal Components Analasys ##########
#---
# title: "Sentinel 1 Raster statistics and Principal Components Analasys"
# Author: Marvin Müsgen
# Description: This Script calculate raster statistics on Sentinel 1 Images and analyse the principal components
#---

#Set paths
observations <- "o_total.csv" # name of observation data
setwd("D:/Marvin/BB_rf") # setting working directory
Input_dir <- "/input" # inpiut directory
Sat_dir <- "/sentinel_1_preproc" # satellite direcotry
sub_dir <- "/Descending" # sub directory ASCENDING
Output_dir <- "/output/Sen1" # output directory
funfold= paste0(getwd(),"/functions") # function director
ext <- "/observations/NP_Boundary.shp" # File in Input_dir: Polygon Shapefile with the extent of the area of Interest


# Set Environment
pckgs <- c("raster","terra","pbapply","RStoolbox", "factoextra", "stringi") # needed packages
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes
xcoordcolnum <- 3 # column number of the x coordinate of your obseravtion date
ycoordcolnum <- 4 # column number of the y coordinate of your observation data
projObs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"  # Projection of the x and y column of observation file
pols <- c("VH","VV","ratio") # SAR Polarizations

#source functions
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/statistic_function.R"))

#creating correct rasternames
singleString <- paste(readLines(paste0(getwd(),"/output/Sen2/used_indices.txt")), collapse=" ")
veg_names <- unlist(strsplit(singleString, " "))

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
  #stacking rasters
  print("Extents are equal")
  rs <- stack(ras_list)
}

#renaming the rasterlayer by their polarization
n <- length(names(rs)) / length(files_dates)
name <- names(rs)
for (i in 1: length(files_dates)){
  #renaming colnames with dates and polarization
  name[n*i] <- paste0(name[n*i],"ratio")
  name[n*i-1] <- paste0(name[n*i-1],"VV")
  name[n*i-2] <- paste0(name[n*i-2],"VH")
}

# calculating raster statistics
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

#calculate pca
pblapply(pols, function(x){
  #reading rasterstack for pricincipal components analysis
  ras_files <- list.files(paste0(getwd(), Output_dir, sub_dir,"/"),full.names = FALSE, pattern = ".tif$") # list raster
  rs_temp <- ras_files[which(grepl(x, ras_files) == TRUE)] #filter polarization
  rs_temp <- rs_temp[which(grepl(start, rs_temp) == TRUE)] # filter start date
  rs_temp <- rs_temp[which(grepl(end, rs_temp) == TRUE)] # filer end date
  rs <- stack(paste0(getwd(), Output_dir, sub_dir, "/", rs_temp)) # stack
  
  print(paste0("start calculating and analyzing pca of", x,"from", start, "to", end, " || actual time:", Sys.time()))
  
  #calculate pca
  pca <- rasterPCA(rs, spca = FALSE, ncomp = 3)
  
  #save pca model
  saveRDS(pca, file = paste0(getwd(),Output_dir,sub_dir,"/pca/pca_dsc_",x,"_from_",start,"to",end,".rds"))
  
  ###plot pca
  eigvalue.pca <- factoextra::get_eigenvalue(pca$model) #get eigenvalues of the PCA Model
  
  #plot variances distributed over the components - ggplot integration
  barplot <- factoextra::fviz_eig(pca$model, choice = c("variance"), ylim=c(0,100), geom = c("bar", "line"), barfill = "#9DC3E6", 
                                  linecolor = "#000000", hjust = 0, addlabels = FALSE, main = "Variances", 
                                  ggtheme = ggplot2::theme_gray())+
    ggplot2::theme(plot.title = element_text(hjust=0.5), axis.text = element_text(size = 20),axis.title.x = element_text(size = 20, margin = margin(t = 40, r = 0, b = 0, l = 0)), 
                   axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 40, b = 0, l = 0)), title = element_text(size = 22))
  
  #writing plot
  png(paste0(getwd(), "/output/graphics/pca/pca_dsc_variances",x,start,"to",end,".png"), res = 500, width=20, height = 20, units = "in") #setup png device
  print(barplot) #plot output to the png device
  dev.off() #deactivate png device and default back to "R plot window"
  
  #plot variable contribution by dimension
  var.pca <- factoextra::get_pca_var(pca$model) #get variable contribution
  rownames(var.pca$contrib) <- stringi::stri_sub(rownames(var.pca$contrib),1,-30)
  colnames(var.pca$contrib) <- gsub(".", " ", colnames(var.pca$contrib), fixed = TRUE)
  corrplot::corrplot(var.pca$contrib,is.corr = FALSE, method = "circle", type = "full", col = NULL, #plot variable contribution to grids
                     outline = TRUE, diag = TRUE, tl.col = "#000000", tl.cex = 1)
  #writing plot
  png(paste0(getwd(), "/output/graphics/pca/pca_dsc_Var_contribution",x,start,"to",end,".png"), res=500, width=20, height = 20, units = "in")
  print(corrplot::corrplot(var.pca$contrib,is.corr = FALSE, method = "circle", type = "full", col = NULL,
                           outline = TRUE, diag = TRUE, tl.col = "#000000", tl.cex = 1))
  dev.off()
  
  # plot rose
  rose <- factoextra::fviz_pca_var(pca$model, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                   alpha.var = 1,# add transparency according to cos2-values
                                   repel = TRUE,habillage ="none")+
    ggplot2::theme(plot.title = element_text(hjust=0.5), axis.text = element_text(size = 16),axis.title.x = element_text(size = 16, margin = margin(t = 40, r = 0, b = 0, l = 0)), 
                   axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 40, b = 0, l = 0)), title = element_text(size = 24))
  #writng plot
  png(paste0(getwd(), "/output/graphics/pca/pca_dsc_rose",x,start,"to",end,".png"), res=350, width=10, height = 10, units = "in")
  print(rose)
  dev.off()
  
  #writing raster
  print(paste0("writing pca raster for: ", x, "from", start,"to",end," || actual time:", Sys.time()))
  terra::writeRaster(pca$map$PC1, paste0(getwd(),Output_dir,sub_dir,"/pca/pca_dsc",x,"from",start,"to",end,".tif"), overwrite=TRUE)
  print("raster succesfully written")
  
  #test if pca 1 explains enough of information; if not also write out pca2
  test <-  eigvalue.pca$variance.percent[1] #percentage of the explaination of information from all layers
  if (test < 80) {
    print(paste0("ATTENTION: PCA1 only explains: ", test," so the 2 PCA will be written out to"))
    print(paste0("writing pca2 raster for: ", x, "from", start,"to",end," || actual time:", Sys.time()))
    terra::writeRaster(pca$map$PC2, paste0(getwd(),Output_dir,sub_dir,"/pca/pca2_dsc",x,"from",start,"to",end,".tif"), overwrite=TRUE)
    print("raster succesfully written")      
  }
  

  #calculate pca for every month month
  pblapply(month, function(y){
    rm_temp <- ras_files[which(grepl(paste0(y,".tif"), ras_files) == TRUE)] # filter month date
    rm_temp <- rm_temp[which(grepl(x, rm_temp) == TRUE)] # filter polarization
    rm <- stack(paste0(getwd(), Output_dir, sub_dir, "/", rm_temp)) # stack
    
    print(paste0("start calculating and analyzing pca of", x,"from month", y, " || actual time:", Sys.time()))
    
    #calculate pca
    pca <- rasterPCA(rm, spca = FALSE, ncomp = 3)
    
    #save pca model
    saveRDS(pca, file = paste0(getwd(),Output_dir,sub_dir,"/pca/pca_dsc_",x,"from",y,".rds"))
    
    #plot pca
    eigvalue.pca <- factoextra::get_eigenvalue(pca$model) #get eigenvalues of the PCA Model
    
    #plot variances distributed over the components - ggplot integration
    barplot <- factoextra::fviz_eig(pca$model, choice = c("variance"), ylim=c(0,100), geom = c("bar", "line"), barfill = "#9DC3E6", 
                                    linecolor = "#000000", hjust = 0, addlabels = FALSE, main = "Variances", 
                                    ggtheme = ggplot2::theme_gray())+
      ggplot2::theme(plot.title = element_text(hjust=0.5), axis.text = element_text(size = 20),axis.title.x = element_text(size = 20, margin = margin(t = 40, r = 0, b = 0, l = 0)), 
                     axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 40, b = 0, l = 0)), title = element_text(size = 22))
    
    #writing plot
    png(paste0(getwd(), "/output/graphics/pca/pca_dsc_variances",x,y,".png"), res = 500, width=20, height = 20, units = "in") #setup png device
    print(barplot) #plot output to the png device
    dev.off() #deactivate png device and default back to "R plot window"
    
    #plot variable contribution by dimension
    var.pca <- factoextra::get_pca_var(pca$model) #get variable contribution
    rownames(var.pca$contrib) <- stringi::stri_sub(rownames(var.pca$contrib),1,-30)
    colnames(var.pca$contrib) <- gsub(".", " ", colnames(var.pca$contrib), fixed = TRUE)
    corrplot::corrplot(var.pca$contrib,is.corr = FALSE, method = "circle", type = "full", col = NULL, #plot variable contribution to grids
                                outline = TRUE, diag = TRUE, tl.col = "#000000", tl.cex = 1)
    
    #writing plot
    png(paste0(getwd(), "/output/graphics/pca/pca_dsc_Var_contribution",x,y,".png"), res=500, width=20, height = 20, units = "in")
    print(corrplot::corrplot(var.pca$contrib,is.corr = FALSE, method = "circle", type = "full", col = NULL,
                             outline = TRUE, diag = TRUE, tl.col = "#000000", tl.cex = 1))
    dev.off()
    # plot rose
    rose <- factoextra::fviz_pca_var(pca$model, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                     alpha.var = 1,# add transparency according to cos2-values
                                     repel = TRUE,habillage ="none")+
      ggplot2::theme(plot.title = element_text(hjust=0.5), axis.text = element_text(size = 16),axis.title.x = element_text(size = 16, margin = margin(t = 40, r = 0, b = 0, l = 0)), 
                         axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 40, b = 0, l = 0)), title = element_text(size = 24))
    
    #writing plot
    png(paste0(getwd(), "/output/graphics/pca/pca_dsc_rose",x,y,".png"), res=350, width=10, height = 10, units = "in")
    print(rose)
    dev.off()
    
    #write pca raster
    print(paste0("writing pca raster for: ", x, "from",y," || actual time", Sys.time()))
    terra::writeRaster(pca$map$PC1, paste0(getwd(),Output_dir,sub_dir,"/pca/pca_dsc",x,"from",y,".tif"), overwrite=TRUE)
    print("raster succesfully written out")
    
    #Test if the explaination of PCA 1 is high enough 
    test <-  eigvalue.pca$variance.percent[1] #percentage of the explaination of information from all layers
    if (test < 80) {
      print(paste0("ATTENTION: PCA1 only explains: ", test," so the 2 PCA will be written out to"))
      print(paste0("writing pca2 raster for: ", x, "from",y," || actual time", Sys.time()))
      terra::writeRaster(pca$map$PC2, paste0(getwd(),Output_dir,sub_dir,"/pca/pca2_dsc",x,"from",y,".tif"), overwrite=TRUE)
      print("raster succesfully written out")
    }
  })
})
