##### This function checks if the studyarea and a rasterstack do overlap #####
# output is the index of the rasters which: overlap, just intersect a part of the area, not overlap

check_overlap <- function(x, studyarea){
  for (i in 1:length(x)){
    Rnotoverlap <- c()
    Rintersect <- c()
    Rcontains < c()
    dummi <- as(extent(x), "SpatialPolygons")
    if (gContainsProperly(studyarea, dummi)){
      print(paste0("Raster ",i," extent is fully within the Area of interest"))
      a <- i
      Rcontains <- c(Rcontains, a)
      next
    }else if(gIntersects(studyarea, dummi)){
      print(paste0("Raster ",i," extent is not fully within the Area of interest"))
      b <- i
      Rintersect <- c(Ritersect, b)
      next
    }else{
      print(paste0("Raster ",i," extent is fully outside the Area of interest"))
      c <- i
      Rnotoverlap <- c(Rnotoverlap, c)
      next
    }
  }
}

check_overlap <- function(RS_area_check, studyarea){
  Rnotoverlap <- c()
  Rintersect <- c()
  Rcontains <- c()
  for (i in 1:nlayers(RS_area_check)){
    dummi <- as(extent(RS_area_check[[i]]), "SpatialPolygons")
    crs(dummi) <- projection(RS_area_check[[i]])
    studyarea<- spTransform(studyarea, projection(RS_area_check[[i]]))
    if (gContains(dummi,studyarea)){
      print(paste0("Raster ",i," extent is fully within the Area of interest"))
      a <- i
      Rcontains <- c(Rcontains, a)
      next
    }else if(gIntersects(studyarea, dummi)){
      print(paste0("Raster ",i," extent is not fully within the Area of interest"))
      b <- i
      Rintersect <- c(Rintersect, b)
      next
    }else{
      print(paste0("Raster ",i," extent is fully outside the Area of interest"))
      c <- i
      Rnotoverlap <- c(Rnotoverlap, c)
      next
    }
  }
}
spTransform(studyarea, projection(bands_10m))
crs(dummi) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
check_overlap()
plot(dummi)
plot(studyarea, add = TRUE)
studyarea
spTransform(dummi,"+proj=longlat +datum=WGS84 +no_defs") 
