#---
#title: "Extraction of vegetation indices from Sentinel 2A scenes for a particular study area considering cloud coverage"
#authors: "Dirk Zeuss, Fortunata Msoffe"
#date: "1 September 2020"
#---
gdal_resample <- function(r, r_base, method = 'bilinear') {
  
  #Geometry attributes
  t1 <- c(xmin(r_base), ymin(r_base), 
          xmax(r_base), ymax(r_base))
  res <- res(r_base)
  
  #Temporal files
  tmp_outname <- sprintf('%s.tif', tempfile())
  tmp_inname <- sprintf('%s.tif', tempfile())
  writeRaster(r, tmp_inname)
  
  #GDAL time!
  gdalwarp(tmp_inname, tmp_outname, 
           tr = res, te = t1, r = method)
  resample_raster = raster(tmp_outname)
  
  return(resample_raster)
}

