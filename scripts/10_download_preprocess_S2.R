###Download and preprocess Sentinel 2 Images from Google Cloud Storage

#Setting the path
setwd("D:/Marvin/BB_rf")
Input_dir <- "/input"
Sat_dir <- "/sentinel_2_data"
Output_dir <- "/output/Sen2"
funfold= paste0(getwd(),"/functions")
Google = "YES" # if your data is frome google

#needed functions
source(paste0(funfold,"/check_library.R"))
source(paste0(funfold,"/do_Download_S2_Google.R"))
pckgs <- c("pbapply","sen2r","parallel")


#needed Variables
tiles <- "32UMU" # Tiles you want to download



####Download_data
do_Download_S2_Google(tilename = tiles, folder = paste0(getwd(),Input_dir,Sat_dir,"/"), Level2 = FALSE, correctExpression ="S2\\D_MSIL\\d\\D_201([8]|[8][0][4-9]).*.SAFE/$")

check_library(pckgs)

#preprocess the sentinel 2 data. Following steps: https://elib.dlr.de/107381/1/LPS2016_sm10_3louis.pdf


#when you are downloading data from google, the folder structure is not the same like the structure from the copernicus hub
# At most times in the google Data two folders are missing which sen2cor needs for execution.
# At this step the folders are created so that sen2cor can run on it
# The issue is explained here: https://github.com/ranghetti/sen2r/issues/348
if (Google == "YES"){
inDir <- paste0(getwd(), Input_dir, Sat_dir,"/",tiles,"/")
S2_folder<- list.files(paste0(getwd(), Input_dir, Sat_dir,"/",tiles,"/"))
lapply(S2_folder, function(x){
  mainDir <- paste0(inDir, x,"/")
  Aux_dir <- paste0(mainDir, "AUX_DATA/")
  Html_dir <- paste0(mainDir,"HTML/")
  if(!dir.exists(Aux_dir)){
    dir.create(Aux_dir)
  } else {
    print(paste0(Aux_dir ," already exists"))
  }
  if(!dir.exists(Html_dir)){
    dir.create(Html_dir)
  } else {
    print(paste0(Html_dir ," already exists"))
  }
})
}else {
  print("Data is not from Google and do not need a correction in their folder structure")
}
##running the preprocessing
n <- detectCores()
pblapply(S2_folder, function(x){
sen2r::sen2cor(l1c_prodlist = x, l1c_dir = paste0(getwd(),Input_dir,Sat_dir,"/",tiles), 
               outdir = paste0(getwd(), Input_dir, "/sentinel_2_preproc"),
               parallel= (n-1), overwrite = TRUE)
})

print(Sys.time())
sen2r::sen2cor(l1c_prodlist = S2_folder[2], l1c_dir = paste0(getwd(),Input_dir,Sat_dir,"/",tiles), 
              outdir = paste0(getwd(), Input_dir, "/sentinel_2_preproc"),
              proc_dir = paste0(getwd(), Input_dir),
              use_dem = TRUE , parallel = 7, overwrite = TRUE)
print(Sys.time())

sen2r::load_binpaths()
file.info(sen2r::load_binpaths()$sen2cor)
system(paste(sen2r::load_binpaths()$sen2cor,"-h"))
