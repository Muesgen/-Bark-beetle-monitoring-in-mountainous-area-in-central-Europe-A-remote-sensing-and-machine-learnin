########## Sentinel 2 cloud removal, raster statistics calculation##########
#---
# title: "Sentinel 2  calculating raster pca"
# Author: Marvin Müsgen
# Description: For reducing computing time a raster pca over the whole period for all Vegetation Indices will be calculated
# Also the PcA for every vegetation Index and month is calculated
#---

#Set paths
setwd("D:/Marvin/BB_rf") # setting working directory
Input_dir <- "/output" # input directory
Sat_dir <- "/Sen2" # satellite direcotry
sub_dir <- "/rasterstats" # sub directory
Output_dir <- "/output/Sen2/pca" # output directory
funfold= paste0(getwd(),"/functions") # function director

# Set Environment
pckgs <- c("raster","terra","pbapply","RStoolbox", "factoextra", "stringi") # needed packages
start <- as.Date("2018-04-01") # Start date of the time of interest
end <- as.Date("2018-09-30") #End date of the time of interes

# getting used indices
singleString <- paste(readLines(paste0(getwd(),"/output/Sen2/used_indices.txt")), collapse=" ")
veg_names <- unlist(strsplit(singleString, " "))


# source functions
source(paste0(funfold,"/check_library.R"))

# load librarys
check_library(pckgs)

#Listing Dates
files_dates<- as.character(substr(list.files(paste0(getwd(), Input_dir,Sat_dir,"/Indices/"),full.names = FALSE, pattern = ".tif$"), 12, 19)) # listing dates of images
month <- as.character(substr(list.files(paste0(getwd(), Input_dir,Sat_dir,"/Indices/"),full.names = FALSE, pattern = ".tif$"), 12, 17)) # listing month
month <- unique(month)
print("File dates are listed")
print(files_dates)

#speeding up the raster package
rasterOptions(maxmemory = 1e+09)


#calculate pca
pblapply(veg_names, function(x){
  #reading rasterstack for pricincipal components analysis
  ras_files <- list.files(paste0(getwd(), Input_dir, Sat_dir,sub_dir,"/"),full.names = FALSE, pattern = ".tif$") # list raster
  rs_temp <- ras_files[which(grepl(x, ras_files) == TRUE)] #filter Index
  
  # if Index include more than 1 Index, teh correct Index is filtered by this if command
  if(length(which(grepl(x,veg_names))) > 1){
  var <- veg_names[which(x == veg_names)] # getting the correct Index
  test <- veg_names[which(grepl(x,veg_names))] # getting all seleced Index
  fake <- test[which(test != var)] #get wrong Index
  rs_temp <- rs_temp[which(grepl(fake, rs_temp) == FALSE)] # remove wrong index from rasterlist
  }
  
  rs_temp <- rs_temp[which(grepl(start, rs_temp) == TRUE)] # filter start date
  rs_temp <- rs_temp[which(grepl(end, rs_temp) == TRUE)] # filer end date
  rs <- stack(paste0(getwd(), Input_dir, Sat_dir, sub_dir, "/", rs_temp)) # stack
  
  print(paste0("start calculating and analyzing pca of", x,"from", start, "to", end, " || actual time:", Sys.time()))
  
  #calculate pca
  pca <- rasterPCA(rs, spca = FALSE, ncomp = 3)
  
  #save pca model
  saveRDS(pca, file = paste0(getwd(),Output_dir,"/pca_Sen2_",x,"_from_",start,"to",end,".rds"))
  
  ###plot pca
  eigvalue.pca <- factoextra::get_eigenvalue(pca$model) #get eigenvalues of the PCA Model
  
  #plot variances distributed over the components - ggplot integration
  barplot <- factoextra::fviz_eig(pca$model, choice = c("variance"), ylim=c(0,100), geom = c("bar", "line"), barfill = "#9DC3E6", 
                                  linecolor = "#000000", hjust = 0, addlabels = FALSE, main = "Variances", 
                                  ggtheme = ggplot2::theme_gray())+
    ggplot2::theme(plot.title = element_text(hjust=0.5), axis.text = element_text(size = 20),axis.title.x = element_text(size = 20, margin = margin(t = 40, r = 0, b = 0, l = 0)), 
                   axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 40, b = 0, l = 0)), title = element_text(size = 22))
  
  #writing plot
  png(paste0(getwd(), "/output/graphics/pca/pca_Sen2_variances",x,start,"to",end,".png"), res = 500, width=20, height = 20, units = "in") #setup png device
  print(barplot) #plot output to the png device
  dev.off() #deactivate png device and default back to "R plot window"
  
  #plot variable contribution by dimension
  var.pca <- factoextra::get_pca_var(pca$model) #get variable contribution
  rownames(var.pca$contrib) <- stringi::stri_sub(rownames(var.pca$contrib),1,-30)
  colnames(var.pca$contrib) <- gsub(".", " ", colnames(var.pca$contrib), fixed = TRUE)
  corrplot::corrplot(var.pca$contrib,is.corr = FALSE, method = "circle", type = "full", col = NULL, #plot variable contribution to grids
                     outline = TRUE, diag = TRUE, tl.col = "#000000", tl.cex = 1)
  #writing plot
  png(paste0(getwd(), "/output/graphics/pca/pca_Sen2_Var_contribution",x,start,"to",end,".png"), res=500, width=20, height = 20, units = "in")
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
  png(paste0(getwd(), "/output/graphics/pca/pca_Sen2_rose",x,start,"to",end,".png"), res=350, width=10, height = 10, units = "in")
  print(rose)
  dev.off()
  
  #writing raster
  print(paste0("writing pca raster for: ", x, "from", start,"to",end," || actual time:", Sys.time()))
  terra::writeRaster(pca$map$PC1, paste0(getwd(),Output_dir,"/pca_Sen2_",x,"from",start,"to",end,".tif"), overwrite=TRUE)
  print("raster succesfully written")
  
  #test if pca 1 explains enough of information; if not also write out pca2
  test <-  eigvalue.pca$variance.percent[1] #percentage of the explaination of information from all layers
  if (test < 80) {
    print(paste0("ATTENTION: PCA1 only explains: ", test," so the 2 PCA will be written out to"))
    print(paste0("writing pca2 raster for: ", x, "from", start,"to",end," || actual time:", Sys.time()))
    terra::writeRaster(pca$map$PC2, paste0(getwd(),Output_dir,"/pca2_Sen2_",x,"from",start,"to",end,".tif"), overwrite=TRUE)
    print("raster succesfully written")      
  }
  
  
  #calculate pca for every month month
  pblapply(month, function(y){
    rm_temp <- ras_files[which(grepl(paste0(y,".tif"), ras_files) == TRUE)] # filter month date
    rm_temp <- rm_temp[which(grepl(x, rm_temp) == TRUE)] # filter vegetation index
    
    # if Index include more than 1 Index, teh correct Index is filtered by this if command
    if(length(which(grepl(x,veg_names))) > 1){
      var <- veg_names[which(x == veg_names)] # getting the correct Index
      test <- veg_names[which(grepl(x,veg_names))] # getting all seleced Index
      fake <- test[which(test != var)] #get wrong Index
      rm_temp <- rm_temp[which(grepl(fake, rm_temp) == FALSE)] # remove wrong index from rasterlist
    }
    
    rm <- stack(paste0(getwd(), Input_dir, Sat_dir, sub_dir, "/", rm_temp)) # stack inex of index and month
    
    print(paste0("start calculating and analyzing pca of", x,"from month", y, " || actual time:", Sys.time()))
    
    #calculate pca
    pca <- rasterPCA(rm, spca = FALSE, ncomp = 3)
    
    #save pca model
    saveRDS(pca, file = paste0(getwd(),Output_dir,"/pca_Sen2_",x,"from",y,".rds"))
    
    #plot pca
    eigvalue.pca <- factoextra::get_eigenvalue(pca$model) #get eigenvalues of the PCA Model
    
    #plot variances distributed over the components - ggplot integration
    barplot <- factoextra::fviz_eig(pca$model, choice = c("variance"), ylim=c(0,100), geom = c("bar", "line"), barfill = "#9DC3E6", 
                                    linecolor = "#000000", hjust = 0, addlabels = FALSE, main = "Variances", 
                                    ggtheme = ggplot2::theme_gray())+
      ggplot2::theme(plot.title = element_text(hjust=0.5), axis.text = element_text(size = 20),axis.title.x = element_text(size = 20, margin = margin(t = 40, r = 0, b = 0, l = 0)), 
                     axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 40, b = 0, l = 0)), title = element_text(size = 22))
    
    #writing plot
    png(paste0(getwd(), "/output/graphics/pca/pca_Sen2_variances",x,y,".png"), res = 500, width=20, height = 20, units = "in") #setup png device
    print(barplot) #plot output to the png device
    dev.off() #deactivate png device and default back to "R plot window"
    
    #plot variable contribution by dimension
    var.pca <- factoextra::get_pca_var(pca$model) #get variable contribution
    rownames(var.pca$contrib) <- stringi::stri_sub(rownames(var.pca$contrib),1,-30)
    colnames(var.pca$contrib) <- gsub(".", " ", colnames(var.pca$contrib), fixed = TRUE)
    corrplot::corrplot(var.pca$contrib,is.corr = FALSE, method = "circle", type = "full", col = NULL, #plot variable contribution to grids
                       outline = TRUE, diag = TRUE, tl.col = "#000000", tl.cex = 1)
    
    #writing plot
    png(paste0(getwd(), "/output/graphics/pca/pca_Sen2_Var_contribution",x,y,".png"), res=500, width=20, height = 20, units = "in")
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
    png(paste0(getwd(), "/output/graphics/pca/pca_Sen2_rose",x,y,".png"), res=350, width=10, height = 10, units = "in")
    print(rose)
    dev.off()
    
    #write pca raster
    print(paste0("writing pca raster for: ", x, "from",y," || actual time", Sys.time()))
    terra::writeRaster(pca$map$PC1, paste0(getwd(),Output_dir,"/pca_Sen2_",x,"from",y,".tif"), overwrite=TRUE)
    print("raster succesfully written out")
    
    #Test if the explaination of PCA 1 is high enough 
    test <-  eigvalue.pca$variance.percent[1] #percentage of the explaination of information from all layers
    if (test < 80) {
      print(paste0("ATTENTION: PCA1 only explains: ", test," so the 2 PCA will be written out to"))
      print(paste0("writing pca2 raster for: ", x, "from",y," || actual time", Sys.time()))
      terra::writeRaster(pca$map$PC2, paste0(getwd(),Output_dir,"/pca2_Sen2",x,"from",y,".tif"), overwrite=TRUE)
      print("raster succesfully written out")
    }
  })
})
