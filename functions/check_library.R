###function for automatized package checking, installing, and requiring

# "check library" is a function for automated checking if librarys are installed and required, if they are not it will automatically run it
# Example function Input: pck = c("base","raster","tidyverse")

check_library <- function(pck){
  for (i in 1:length(pck)){
    if(pck[i] %in% rownames(installed.packages()) == FALSE) {install.packages(pck[i])}
  }
  lapply(pck, require, character.only = TRUE)
} 