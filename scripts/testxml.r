### resampling
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf")
Input_dir <- "/input"
Sat_dir <- "/sentinel_2_preproc"
Output_dir <- "/input/resampled/"
pckgs <- c("pbapply", "XML")
funfold= paste0(getwd(),"/functions")
gptfile <- "/Applications/snap/bin/gpt"


source(paste0(funfold,"/check_library.R"))
check_library(pckgs)

Sen2folder <- list.files(paste0(getwd(), Input_dir, Sat_dir,"/"))

pblapply(Sen2folder, function(x){
  prefix.xml <- "<graph id='someGraphId'>
                  <version>1.0</version>
                  <node id='Resample'>
                    <operator>Resample</operator>
                    <sources>
                      <sourceProduct>${sourceProduct}</sourceProduct>
                    </sources>
                    <parameters>
                      <referenceBand>B2</referenceBand>
                      <upsampling>Nearest</upsampling>
                      <downsampling>First</downsampling>
                      <flagDownsampling>First</flagDownsampling>
                      <resampleOnPyramidLevels>true</resampleOnPyramidLevels>
                    </parameters>
                   </node>
               </graph>"
  
  doc <- xmlTreeParse(prefix.xml, useInternalNodes = T) # PARSE STRING
  root <- xmlRoot(doc) #Find Roots  
  f <- paste0(getwd(),"/scipts/XMLfiles/","xtresample.xml")
  print(doc)
  cat(saveXML(doc), file= "/Volumes/MarvinLaCie/Marvin/BB_rf/scipts/testi.xml")
  
  system(paste(gptfile,f,"-t",paste0(getwd(),Output_dir,x,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",x)))
})

/Volumes/MarvinLaCie/Marvin/BB_rf/scripts/XMLfiles



# VIEW XML
print(doc)

# SAVE XML TO FILE
saveXML(doc, file="/Volumes/MarvinLaCie/Marvin/BB_rf/scripts/test2.xml")

system("/Applications/snap/bin/gpt /Volumes/MarvinLaCie/Marvin/BB_rf/scripts/test2.xml -t /Volumes/MarvinLaCie/Marvin/BB_rf/input/sentinel_2_resampledtest2.dim /Volumes/MarvinLaCie/Marvin/BB_rf/input/sentinel_2_preproc/S2A_MSIL2A_20180415T103021_N0206_R108_T32UMU_20180415T142301.SAFE")
