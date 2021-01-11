#preprocessing 

topo <- list.files("/Volumes/MarvinLaCie/Marvin/BB_rf/input/observations/dem", pattern = ".sdat", full.names = TRUE) 
dem <- raster("/Volumes/MarvinLaCie/Marvin/BB_rf/input/observations/dem/dem_10.tif")
climate <- stack("/Volumes/MarvinLaCie/Marvin/BB_rf/output/Parameter/climate.tif")
soil <- list.files("/Volumes/MarvinLaCie/Marvin/BB_rf/input/observations/soil/", pattern=".tif", full.names = TRUE)

soil <- stack(soil)
topo_files<- topo[seq(1,length(topo), 2)]
topo <- stack(topo_files)
plot(soil[[2]])

dummy <- readOGR("/Volumes/MarvinLaCie/Marvin/NP_Boundary.shp")
plot(dummy, add=TRUE)

dummy <- sp::spTransform(dummy, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
dem <- crop(dem, dummy)
climate <- crop(climate, dummy)
soil <- projectRaster(soil, crs = crs(dummy))
soil_10 <- resample(soil, dem, method= "ngb")
soil <- crop(soil_10,dummy)
names(dem)
names(climate) <- names(climate_resample)
names(soil)
topo <- crop(topo, dummy)
param <- stack(dem, soil)
param <- stack(param, topo)
param <- stack(param, climate)
rasterOptions(maxmemory = 1e+09)

#read in the observations
obs <-read.csv("/Volumes/MarvinLaCie/Marvin/BB_rf/input/observations/Befall_gesamt/barkbeetle_obs_x_to_2018_2class.csv")
print("observations loaded")
head(obs)
#obs[,5:ncol(obs)] <- NULL
#defining coordinates, Lat = Y Long = X
xy <- obs[,c(xcoordcolnum,ycoordcolnum)]
xy <-as.list(xy)
obs[,xcoordcolnum] <- as.numeric(obs[,xcoordcolnum])
obs[,ycoordcolnum] <- as.numeric(obs[,ycoordcolnum])
#f <-paste0(getwd(),"/output/Sen2/used_indices.txt")

dummy <- readOGR("/Volumes/MarvinLaCie/Marvin/NP_Boundary.shp")
dummy@proj4string@projargs
if (is.null(projObs) == TRUE){
  #creating a spatioalpointdataframe
  spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                                 proj4string = CRS(rs_list[[1]]@crs@projargs))
  print("Spatialdataframe is created")
  head(spdf)
  print("POSSIBLE ERROR: Please check the outwritten DataFrame, could be with NAs")
  Print("SOlution: check your coordinates in the observation DF and give the correct Projection to the projOBS argument")
} else{
  #creating a spatioalpointdataframe
  spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                                 proj4string = CRS(dummy@proj4string@projargs))
  print("Spatialdataframe is created")
  head(spdf)
}
#dummy <- readOGR("/Volumes/MarvinLaCie/Marvin/NP_Boundary.shp")
#transforming into correct projection
r_crs<- rs_list[[1]]@crs
#dummy <- sp::spTransform(dummy, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
spdf <- sp::spTransform(spdf, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))

#creating correct rasternames
names(param)
#initialize progressbar
pb = txtProgressBar(min = 0, max = nlayers(param), initial = 0, style = 3)

#extracting rasterdata
for (i in 1:nlayers(param)){
  setTxtProgressBar(pb,i)
  #extracting raster values
  print(paste0("Starting extracting raster data for observation points: ", Sys.time()))
  vx_rs <- velox::velox(param[[i]])
  df <- as.data.frame(vx_rs$extract_points(spdf))
  colnames(df) <-names(param[[i]])
  print(paste0("Finished extracting raster data for observation points: ", Sys.time()))
  obs <- cbind(obs,df)
}
 writeRaster()
writeRaster(param, paste0(getwd(),"/output/paramet_data2.tif"))
write.csv2(obs,file = paste0(getwd(),"/output/paramet_data2.csv"))
obs_test<- read.csv2(paste0(getwd(),"/output/paramet_data2.csv"))
colnames(obs_test)
plot(climate[[1]])
plot(spdf[6,], add= TRUE)
