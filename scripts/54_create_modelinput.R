# model all 2 classes #2

paramdata <- read.csv2("/home/mohammad/Desktop/Marvin/modelinput_20_01_2021/paramet_data2.csv")
sen2data <- list.files("/home/mohammad/Desktop/Marvin/modelinput_20_01_2021/Sen_2", pattern = ".csv", full.names = TRUE)
sen2data<- sen2data[-87]
sen2data<- sen2data[-1]
sen2data<- sen2data[-1]
#test <- read.csv2(sen2data[74], sep =",")
#sen2data_whole_period<- sen2data[1:12]

for(i in 1:length(sen2data)){
  x<- sen2data[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  paramdata <- cbind(paramdata, df)
}
print("loaded param")
for(i in 1:length(sen2data_whole_period)){
  x<- sen2data_whole_period[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  paramdata <- cbind(paramdata, df)
}
print("loaded Sen2")
#sen2data_monthly <- sen2data[13:length(sen2data)]
#sen2data[1,c(2:5)]
sen1ascdata <- list.files("/home/mohammad/Desktop/Marvin/modelinput_20_01_2021/Sen_1_2/Ascending", pattern = ".csv", full.names = TRUE)
sen1ascdata<- sen1ascdata[-1]
for(i in 1:length(sen1ascdata)){
  x<- sen1ascdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_Asc")
  paramdata <- cbind(paramdata, df)
}

print("loaded ascending")
sen1descdata <- list.files("/home/mohammad/Desktop/Marvin/modelinput_20_01_2021/Sen_1_2/Descending", pattern = ".csv", full.names = TRUE)
sen1descdata <- sen1descdata[-1]
for(i in 1:length(sen1descdata)){
  x<- sen1descdata[i]
  df <- read.csv2(x, sep = ",")
  df <- df[,c(2:5)]
  colnames(df) <- paste(colnames(df), "_desc")
  paramdata <- cbind(paramdata, df)
}
print("loaded descending")
#write.csv(paramdata , "/home/mohammad/Desktop/Marvin/modelinput_20_01_2021/all_inputs_model_2var_sep.csv", sep=",")
ncol(paramdata)
