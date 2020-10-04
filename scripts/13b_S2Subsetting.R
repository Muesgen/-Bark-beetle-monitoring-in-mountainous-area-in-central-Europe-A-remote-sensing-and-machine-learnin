### subsetting

#setting the parameter
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf") # working directory
Input_dir <- "/input" # input directory
Sat_dir <- "/Volumes/MarvinLaCie/Marvin/BB_rf/input/Sentinel_2_l2a/" # sub directory
Output_dir <- "/input/sentinel_2_subsetted/" # output direcotry
pckgs <- c("pbapply", "XML","sp","rgdal","wicket") # needed packages
funfold= paste0(getwd(),"/functions") # function folder directory
area <- "/Volumes/MarvinLaCie/Marvin/NP_Boundary.shp" #file to the area of interest as Polygon
layer <- "NP_Boundary"
gptfile <- "/Applications/snap/bin/gpt" # file to the gpt operator

# reading packages
source(paste0(funfold,"/check_library.R"))
check_library(pckgs)

#reading the area of interest and convert to wkt
shape <- readOGR(dsn=path.expand(area), layer= layer)
wkt <- sp_convert(shape, group = TRUE)

#listing sen2 image folders
Sen2folder <- list.files(paste0(getwd(), Sat_dir),pattern = ".SAFE")
#defining resample operator
prefix.xml <-     paste0("<graph id='Subsetting'>
                    <version>1.0</version>
                    <node id='Subsetting'>
                      <operator>Subset</operator>
                      <sources>
                        <source>${source}</source>
                      </sources>
                      <parameters>
                        <sourceBands></sourceBands>
                        <region></region>
                        <geoRegion>",wkt,"</geoRegion>
                        <subSamplingX></subSamplingX>
                        <subSamplingY></subSamplingY>
                        <fullSwath></fullSwath>
                        <tiePointGridNames></tiePointGridNames>
                        <copyMetadata></copyMetadata>
                      </parameters>
                    </node>
                  </graph>")

#creating xml Tree
doc <- xmlTreeParse(prefix.xml, useInternalNodes = T) # PARSE STRING
root <- xmlRoot(doc) #Find Roots  
print(doc)
#save operator as xml
saveXML(doc, file= "S2Subset.xml")
f <- paste0(getwd(),"/S2Subset.xml") #path to the operator as xm file

#execute resampling
pblapply(Sen2folder, function(x){
  print(paste("Starting subsetting",x,Sys.time()))
  system(paste(gptfile,f,"-t",paste0(getwd(),Output_dir,x,"_subset.tif"),paste0(getwd(),Input_dir,Sat_dir,"/",x)))
  print(paste("Finish subsetting",x,Sys.time()))
})
