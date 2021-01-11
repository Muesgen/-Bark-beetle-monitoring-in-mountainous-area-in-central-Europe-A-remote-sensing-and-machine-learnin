########## Setup Script ##########
#---
# title: "Setup Script"
# Author: Marvin Müsgen
# Description: This Script create an environment for hte whole process
#---

#set working directory
setwd("/Volumes/MarvinLaCie/Marvin/BB_rf")
setwd("D:/Marvin/BB_rf")
root_folder <- getwd()

#Set librarys
devtools::install_github("r-spatial/link2GI", ref = "master")
libs = c("link2GI", "raster","devtools")
lapply(libs, require, character.only = TRUE)

# Set project specific subfolders
project_folders <- c("input/",                                 # data folders
                        "input/observations/",
                        "input/sentinel_1_data/",
                        "input/sentinel_1_preproc/",
                        "input/sentinel_2_preproc/",
                        "input/sentinel_2_resampled/",
                        "input/sentinel_2_subsetted/",
                    "output/", 
                        "output/Sen1/", 
                          "output/Sen1/Descending/", 
                          "output/Sen1/Ascending/", 
                            "output/Sen1/Descending/pca/",
                            "output/Sen1/Ascending/pca",
                        "output/Sen1_2/",
                          "output/Sen1_2/Ascending/",
                          "output/Sen1_2/Descending",
                        "output/graphics/",
                          "output/graphics/indices/",
                        "output/Sen2/",
                        "output/Sen2/Indices/",
                        "output/Sen2/rasterstats/",
                        "output/Sen2/pca/",
                    "functions/",
                    "scripts/",
                    "results/",
                    "doc/") 

#create environment
envrmt <- link2GI::initProj(projRootDir = root_folder,
                           projFolders = project_folders, 
                           global = FALSE)

