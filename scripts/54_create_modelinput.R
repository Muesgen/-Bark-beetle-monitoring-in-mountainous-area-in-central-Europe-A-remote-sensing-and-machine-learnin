# model all 2 classes #2

paramdata <- read.csv2("/Volumes/MarvinLaCie/Marvin/BB_rf/output/paramet_data2.csv")
read.csv2(paste0(getwd(),"/output/paramet_data2.csv"))
sen2data <- list.files("/Volumes/MarvinLaCie/Marvin/BB_rf/modelinput1/sen2", pattern = ".csv", full.names = TRUE)

for(i in 1:length(sen2data)){
  x<- sen2data[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  paramdata <- cbind(paramdata, df)
  }

sen1ascdata <- list.files("/Volumes/MarvinLaCie/Marvin/BB_rf/modelinput1/Ascending", pattern = ".csv", full.names = TRUE)

for(i in 1:length(sen1ascdata)){
  x<- sen1ascdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_Asc")
  paramdata <- cbind(paramdata, df)
}
sen1descdata <- list.files("/Volumes/MarvinLaCie/Marvin/BB_rf/modelinput1/Descending", pattern = ".csv", full.names = TRUE)

for(i in 1:length(sen1descdata)){
  x<- sen1descdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_desc")
  paramdata <- cbind(paramdata, df)
}

write.csv(paramdata , "/Volumes/MarvinLaCie/Marvin/BB_rf/modelinput1/all_inputs_model1.csv")
ncol(paramdata)
