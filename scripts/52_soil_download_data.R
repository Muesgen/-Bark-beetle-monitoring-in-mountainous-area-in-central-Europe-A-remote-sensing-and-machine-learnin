library(XML)
library(rgdal)
library(gdalUtils)
library(raster)

#We set other variables necessary for the WCS call for all kinds of requests
voi <- "ocs" #variable of interest; description of the variable names find here: https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean
depth <-"0-30cm"
quantile <- "mean"

voi_layer <- paste(voi,depth, sep="_") # layer of interest

wcs_path <- paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service <- "SERVICE=WCS"
wcs_version <- "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.


#getting extent and projection of the area of interest
ext_ras <- raster("D:/Marvin/BB_rf/output/Sen2/pca/pca_Sen2_CIrededgefrom2018-04-01to2018-09-30.tif")
ext <- extent(ext_ras)
prj <- ext_ras$pca_Sen2_CIrededgefrom2018.04.01to2018.09.30@crs@projargs

#setting extent and projection for download
bb <- c(ext@xmin,ext@xmax,ext@ymin,ext@ymax) # Example bounding box (area)
igh <- prj # proj string  projection


wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

#creating xml file for command
l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", voi_layer , parent=l1)

# Save to local disk
xml.out = "D:/Marvin/BB_rf/scripts/ocs_soil_grid_Download.xml"
saveXML(l1, file = xml.out)


# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- paste0("D:/Marvin/BB_rf/input/observations/soil/",voi_layer,".tif")

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES","GDAL_HTTP_UNSAFESSL=YES"),
               verbose=TRUE
)

###############

library(XML)
library(rgdal)
library(gdalUtils)

#Set variables of interest
#We define the variables for the soil property and layer of interest. See here for the naming conventions
voi = "ocs" # variable of interest
depth = "5-15cm"
quantile = "Q0.5"



voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 
#We set other variables necessary for the WCS call for all kinds of requests
wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.
#NOTE wcs_version = "VERSION=2.0.1" works for gdal >=2.3; wcs_version = "VERSION=1.1.1" works with gdal < 2.3.

#Example 1: Describe the coverage layer

#1. First we define the request as DescribeCoverage and we create a string for the full request using also the variables previously defined
wcs_request = "DescribeCoverage" 

wcs = paste(wcs_path, wcs_service, wcs_version, wcs_request, sep="&")

#2. Then we create a XML that can be used with the gdalinfo utility after being saved to disk:
  l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", voi_layer, parent=l1)

# Save to local disk
xml.out = "./sg.xml"
saveXML(l1, file = xml.out)

#3. Finally we use gdalinfo to get the description of the layer
gdalinfo("./sg.xml")

#Example 2: Download a Tiff for a region of interest (ROI)

#1. First we define the request with the parameters for the region of interest, projection, format, resolution
#An example (Ghana) using Homolosine projection
bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
#We paste all the components of the WCS request:
  wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

#NOTE
#For gdal < 2.3 an extra & is necessary at the end of the string: wcs = paste0(wcs,"&")

#2. Then we create a XML that can be used with the gdal utility after being saved to disk:
  l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "ocs_5-15cm_Q0.5", parent=l1)

# Save to local disk
xml.out = "./sg.xml"
saveXML(l1, file = xml.out)

#3. Finally we use gdal_translate to get the geotiff locally.
# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './test.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)
#The downloaded file can then be read into R.
#See this page for examples on how to reproject

#Common errors:
 # If you get an SSL error (for example: SSL certificate problem: self signed certificate in certificate chain) please try to add the following config parameter to the gdal call: --config GDAL_HTTP_UNSAFESSL YES:
  gdal_translate(xml.out, file.out,
                 tr=c(250,250), projwin=bb,
                 projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES","GDAL_HTTP_UNSAFESSL=YES"),
                 verbose=TRUE
  )
