### resampling

#setting the parameter
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf") # working directory
Input_dir <- "/input" # input directory
Sat_dir <- "/sentinel_2_preproc" # sub directory
Output_dir <- "/input/sentinel_2_resampled/" # output direcotry
pckgs <- c("pbapply", "XML") # needed packages
funfold= paste0(getwd(),"/functions") # function folder directory
gptfile <- "/Applications/snap/bin/gpt" # file to the gpt operator

# reading packages
source(paste0(funfold,"/check_library.R"))
check_library(pckgs)
#listing sen2 image folders
Sen2folder <- list.files(paste0(getwd(), Input_dir, Sat_dir,"/"))
#defining resample operator
prefix.xml <-   "<graph id='sen2resampling'>
                  <version>1.0</version>
                  <node id='sen2resampling'>
                    <operator>S2Resampling</operator>
                    <sources>
                      <sourceProduct>${sourceProduct}</sourceProduct>
                    </sources>
                    <parameters>
                      <resolution>10</resolution>
                      <upsampling>Nearest</upsampling>
                      <downsampling>First</downsampling>
                      <flagDownsampling>First</flagDownsampling>
                      <resampleOnPyramidLevels>true</resampleOnPyramidLevels>
                    </parameters>
                  </node>
                </graph>"

#creating xml Tree
doc <- xmlTreeParse(prefix.xml, useInternalNodes = T) # PARSE STRING
root <- xmlRoot(doc) #Find Roots  
print(doc)
#save operator as xml
saveXML(doc, file= "S2resample.xml")
f <- paste0(getwd(),"/S2resample.xml") #path to the operator as xm file

#execute resampling
pblapply(Sen2folder, function(x){
  print(paste("Starting resampling",x,Sys.time()))
  system(paste(gptfile,f,"-t",paste0(getwd(),Output_dir,x,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",x)))
  print(paste("Finish resampling",x,Sys.time()))
})# you do not have to think about the warnings. https://forum.step.esa.int/t/warning-message-while-resampling-the-sentinel-2-data/13533
#1 argument: the gpt operator
#2 argument: the resampling operator
#3 argument: -t the outputfile
#4 argument: the input file


#############Working on possible parallelization
#cl <- parallel::makeCluster(7)
#parallel::parLapply(cl, Sen2folder, function(x){
#  print(Sys.time())
#  system(paste(gptfile,f,"-t",paste0(getwd(),Output_dir,x,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",x)," & ",paste(gptfile,f,"-t",paste0(getwd(),Output_dir,y,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",y))))
#  print(Sys.time())
#  })

#writeLines(text = paste(gptfile,f,"-t",paste0(getwd(),Output_dir,x,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",x)), "commandstest.txt")
#system(command = "chmod +x commands.txt & ./commands.txt")
#system(eval(parse(paste(gptfile,f,"-t",paste0(getwd(),Output_dir,Sen2folder,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",Sen2folder)))))
#eval(shell(paste(gptfile,f,"-t",paste0(getwd(),Output_dir,Sen2folder,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",Sen2folder))))

#system(sprintf(paste(gptfile,f,"-t",paste0(getwd(),Output_dir,x,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",x))))

#z <- paste(gptfile,f,"-t",paste0(getwd(),Output_dir,x,"_resampled.dim"),paste0(getwd(),Input_dir,Sat_dir,"/",x))

#parallel::stopCluster(cl)# you do not have to think about the warnings. https://forum.step.esa.int/t/warning-message-while-resampling-the-sentinel-2-data/13533
#1 argument: the gpt operator
#2 argument: the resampling operator
#3 argument: -t the outputfile
#4 argument: the input file