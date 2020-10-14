#Download Sentinel 2 Images from Google.
#
#This function needs Google Cloud Storage SDK, you can download and install this SDK from: https://cloud.google.com/sdk/install 
#make sure the system PATH is set so you can use 'gsutil' tool directly in command line.

# DownloadSentinel2("32UMU", "D:/", Level2 = TRUE, regExpr = "S2\\D_MSIL\\d\\D_201([8]|[8][0][4-9]).*.SAFE/$")
do_Download_S2_Google <- function(tilename, folder, Level2 = FALSE, regExpr = NULL) {
  gcLink <- ifelse(Level2 == TRUE, file.path("gs://gcp-public-data-sentinel-2/L2/tiles", substr(tilename, 1, 2),
                                            substr(tilename, 3, 3), substr(tilename, 4, 5)),
                  file.path("gs://gcp-public-data-sentinel-2/tiles", substr(tilename, 1, 2),
                            substr(tilename, 3, 3), substr(tilename, 4, 5)))
  pck <- "pbapply"
  for (i in 1:length(pck)){
      if(pck[i] %in% rownames(installed.packages()) == FALSE) {install.packages(pck[i])}
    }
    lapply(pck, require, character.only = TRUE)
  
  com <- paste("gsutil ls", gcLink)
  #create list
  files_list <- system(com, intern = TRUE) 
  # create destination folder
  destination_dir <- folder
  if (dir.exists(file.path(destination_dir, tilename)) == FALSE) dir.create(file.path(destination_dir, tilename))
  
  # regular expression filter
  if (!is.null(regExpr)) files_list <- grep(regExpr, files_list, value = TRUE)
  
  # check if there is any data in the storage
  if (identical(files_list, character(0)) | length(files_list) == 0) {
    print("Did nott find any data matching the search!")
    return(NULL)
  }
  
  # start downloading
  pbapply:pblapply(files_list, function(x){
    temp <- unlist(strsplit(x, "/"))
    dirname <- temp[length(temp)]
    if (dir.exists(file.path(destination_dir, tilename, dirname)) == FALSE) {
      system(paste("gsutil -m cp -r", x, file.path(destination_dir, tilename, ".")))
    }
  })
}
