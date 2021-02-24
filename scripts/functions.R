####### Functions for R and Python

#### R Functions
# Function to select on how many cores next function should be executed
autoStopCluster <- function(cl) {
  stopifnot(inherits(cl, "cluster"))
  env <- new.env()
  env$cluster <- cl
  attr(cl, "gcMe") <- env
  reg.finalizer(env, function(e) {
    message("Finalizing cluster ...")
    message(capture.output(print(e$cluster)))
    try(parallel::stopCluster(e$cluster), silent = FALSE)
    message("Finalizing cluster ... done")
  })
  cl
}
#---
#Titel: Calculate Vegetation Indices
#Author: Marvin Muesgen
#Last change: 28.08.2020
#---
### function for calculating vegetation indices on sentinel 2 or aerial Images RED GREEN BLUE
#if you want to calulate only rgb please use the sentinel sensor and put the blue band on band2,
#green on band3 and red on band4

# also Landsat is possible, please check bandnumber by wavelength

#For using this function you have to make the choice in the "sensor" argument if you want to use:
#"Sentinel2" or "Landsat7"
#Please put the band arguments correctly, the argument must be a raster:

#Sentinel2:
#band1 = 442nm Wavelength AEROSOL
#band2 = 492nm Wavelength BLUE
#band3 = 559nm wavelength GREEN
#band4 = 664nm wavelength RED
#band5 = 704nm wavelength RED-edge
#band8 = 832nm Wavelength NIR
#band8a = 864nm Wavelength NIR
#band9 = 945nm Wavelength Water Vapor absorption
#band10 = 1375nm Wavelength thin cirrus Detection
#band11 = 1614 nm Wavelength SWIR
#band12 = 2202 nm Wavelength SWIR

#Landsat7:
#band1 = 485nm Wavelength BLUE
#band2 = 570nm Wavelength GREEN
#band3 = 660nm Wavelength RED
#band4 = 840nm  Wavelength NIR
#band5 = 1650nm Wavelength SWIR1
#band6 = 10400nm Wavelength Thermal
#band7 = 2220nm Wavelength SWIR2
#band8 = 520-900nm Wavelength Panchromatic

#In the argument "rgbi" you can choose one of the indices
#you want to calculate
#feel free to add new indices you are working on

#rgbi = Vegetation Index you want to calculate
#so far implemented vegetation indices: VVI, VARI, NDTI, RI,
#CI, BI, SI, HI, TGI, NGRDI, ExG, ExGR, VEG, CIVE, COM, CEV,
#CEV_weight, mcfesti,
# CCCI, CVI, GNDVI, MCARI, NDVI, NDRE, MSI, NDVWI, BRI, LCI,
#LWCI, GLI, EVI, AFRI1600, MSR670, CIrededge, CIgreen, MVI, NDMI, DSWI4

#Example: calc_Indices(sensor = "Sentinel2", band1 = stack[[1]],
#band2 = stack[[2]], band3 = stack[[3]], band4 = stack[[4]]..., rgbi = "NDVI")


calc_Indices<- function(sensor = "Sentinel2",
    band1 = NULL, band2 = NULL, band3 = NULL, band4 = NULL, band5 = NULL,
    band6 = NULL, band7 = NULL, band8 = NULL, band8a = NULL,band9 = NULL,
    Band10 = NULL, band11 = NULL, band12 = NULL, rgbi=c("VVI","VARI",
    "NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI", "ExGR", "VEG")){

  if (sensor == "Landsat7"){
    #translate into Landsat bands
    band12 <- band7
    bandPanchr <- band8
    bandThermal <- band6
    band11 <- band5
    band8 <- band4
    band4 <- band3
    band3 <- band2
    band2 <- band1
    band1 <- NULL
    band5 <- NULL
    band6 <- NULL
    band7 <- NULL
    band8a <- NULL
    band9 <- NULL
    band10 <- NULL
  }

  ## compatibility check
  if (raster::nlayers(bands) < 3)
    stop("Argument 'bands' needs to be a Raster* object with at
         least 3 layers (usually red, green and blues 0r something like:
         NIR, SWIR...).")

  ### processing

  indices <- lapply(rgbi, function(item){
    ## calculate Visible Vegetation Index vvi
    if (item=="VVI"){
      print("\ncalculate Visible Vegetation Index (VVI)")
      VVI <- (1 - abs((band4 - 30) / (band4 + 30))) * (1 - abs((band3 - 50)
            / (band3 + 50))) * (1 - abs((band2 - 1) / (band2 + 1)))
      names(VVI) <- "VVI"
      return(VVI)

    } else if (item=="VARI"){
      # calculate Visible Atmospherically Resistant Index (VARI)
      print("\ncalculate Visible Atmospherically Resistant Index (VARI)")
      VARI<- (band3-band4)/ (band3+band4-band2)
      names(VARI) <- "VARI"
      return(VARI)

    } else if (item=="NDTI"){
      ## Normalized difference turbidity index
      print("\ncalculate Normalized difference turbidity index (NDTI)")
      NDTI<-(band4-band3)/(band4+band3)
      names(NDTI) <- "NDTI"
      return(NDTI)

    } else if (item=="RI"){
      # redness index
      print("\ncalculate redness index (RI)")
      RI<-band4**2/(band2*band3**3)
      names(RI) <- "RI"
      return(RI)

    } else if (item=="CI"){
      # CI Soil Colour Index
      print("\ncalculate Soil Colour Index (CI)")
      CI<-(band4-band3)/(band4+band3)
      names(CI) <- "CI"
      return(CI)

    } else if (item=="BI"){
      #  Brightness Index
      print("\ncalculate Brightness Index (BI)")
      BI<-sqrt((band4**2+band3**2+band2*2)/3)
      names(BI) <- "BI"
      return(BI)

    } else if (item=="SI"){
      # SI Spectra Slope Saturation Index
      print("\ncalculate Spectral Slope Saturation Index (SI)")
      SI<-(band4-band2)/(band4+band2)
      names(SI) <- "SI"
      return(SI)

    } else if (item=="HI"){
      # HI Primary colours Hue Index
      print("\ncalculate Primary colours Hue Index (HI)")
      HI<-(2*band4-band3-band2)/(band3-band2)
      names(HI) <- "HI"
      return(HI)

    } else if (item=="TGI"){
      # Triangular greenness index
      print("\ncalculate Triangular greenness index (TGI)")
      TGI <- -0.5*(190*(band4 - band3)- 120*(band4 - band2))
      names(TGI) <- "TGI"
      return(TGI)

    } else if (item=="NGRDI"){
      # NGRDI Normalized green red difference index
      print("\ncalculate Normalized green red difference index  (NGRDI)")
      NGRDI<-(band3-band4)/(band3+band4)
      names(NGRDI) <- "NGRDI"
      return(NGRDI)

    } else if (item=="ExG"){
      # ExG Excess Green Index
      print("\ncalculate Excess Green Index (ExG)")
      ExG <- 2*band3 - band4 - band2
      names(ExG) <- "ExG"
      return(ExG)

    } else if (item=="ExGR"){
      # ExGR Difference between Excess Green Index Excess Red Index
      print("\ncalculate  Excess Green Index - Excess Red Index (ExGR)")
      ExGR<- (2*band3 - band4 - band2) - (1.4*band4 - band3)
      names(ExGR) <- "ExGR"
      return(ExGR)

    } else if (item=="VEG"){
      # VEG Vegetative Index
      print("\ncalculate  Vegetative Index (VEG)")
      VEG<- band3 / (band4**0.667 * band2**0.333)
      names(VEG) <- "VEG"
      return(VEG)

    } else if (item=="CIVE"){
      # CIVE Color Index of Vegetation Extraction
      print("\ncalculate Color Index of Vegetation Extraction (CIVE)")
      CIVE<- 0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745
      names(CIVE) <- "CIVE"
      return(CIVE)

    } else if (item=="COM"){
      # COM Combined Index
      print("\ncalculate Combined Index (COM)")
      COM<- 0.25*(2*band3 - band4 - band2) + 0.3*
        ((2*band3 - band4 - band2) - (1.4*band4 - band3)) +
        0.33* (0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745)
      + 0.12* (band3 / (band4**0.667 * band2**0.333))
      names(COM) <- "COM"
      return(COM)

    } else if (item=="CEV"){
      # CEV Combined Index
      print("\ncalculate Combination from Ponti et al. (CEV)")
      CEV<- 0.33*((1 - abs((band4 - 30) / (band4 + 30))) *
                    (1 - abs((band3 - 50) / (band3 + 50))) *
                    (1 - abs((band2 - 1) / (band2 + 1)))) +
        0.33*(2*band3 - band4 - band2) + 0.33*(0.441*band4 -
            0.881*band3 + 0.385*band2 + 18.78745)
      names(CEV) <- "CEV"
      return(CEV)

    } else if (item=="CEV_weight"){
      # CEV_weight weighted COmbined Index
      print("\ncalculate Combination from Ponti et al. (CEV_weight)")
      CEV_weight<- 0.4*((1 - abs((band4 - 30) / (band4 + 30))) *
                          (1 - abs((band3 - 50) / (band3 + 50))) *
                          (1 - abs((band2 - 1) / (band2 + 1)))) +
        0.2*(2*band3 - band4 - band2) + 0.4*(0.441*band4 - 0.881*band3 +
                                               0.385*band2 + 18.78745)
      names(CEV_weight) <- "CEV_weight"
      return(CEV_weight)

    } else if (item=="mcfesti"){
      #mcfesti RGB PCA test (MArvin M?sgen Philipps University Marburg)
      print("\ncalculate mcfesti (mcfesti)")
      mcfesti <- 0.25*(sqrt((band4**2+band3**2+band2*2)/3))
      + 0.25*(0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745)
      + 0.25*((1 - abs((band4 - 30) / (band4 + 30))) *
                (1 - abs((band3 - 50) / (band3 + 50))) *
                (1 - abs((band2 - 1) / (band2 + 1)))) +
        0.25*(-0.5*(190*(band4 - band3)- 120*(band4 - band2)))
      names(mcfesti) <- "mcfesti"
      return(mcfesti)
    }

    else if (item=="CCCI"){
      #CCCI Canopy Chlorphyll COntent Index
      print("\ncalculate Canopy Chlorophyll Content Index (CCCI)")
      CCCI <- ((band8a - band5) / (band8a + band5)) /
        ((band8a - band4) / (band8a + band4))
      names(CCCI) <- "CCCI"
      return(CCCI)
    }

    else if (item=="CVI"){
      #CVI Chlorpohlyy Vegetation Index
      print("\ncalculate Chlorophyll vegetation index  (CVI)")
      CVI <- band8a * (band5 / band3)
      names(CVI) <- "CVI"
      return(CVI)
    }
    else if (item=="GNDVI"){
      #GNDVI  Green Normalized Difference Vegetation Index
      print("\ncalculate Green Normalized Difference Vegetation Index (GNDVI)")
      GNDVI <- (band8a - band3)/(band8a + band3)
      names(GNDVI) <- "GNDVI"
      return(GNDVI)
    }
    else if (item=="MCARI"){
      #MCARI Modified Chlorophyll Absorption in Reflectance Index
      print("\ncalculate Modified Chlorophyll Absorption in
            Reflectance Index (MCARI)")
      MCARI <- ((band5 - band4)-0.2*(band5 - band3))*(band5 / band4)
      names(MCARI) <- "MCARI"
      return(MCARI)
    }
    else if (item=="NDVI"){
      #NDVI Normalized Differenced Vegetation Index
      print("\ncalculate Normalized Differenced Vegetation Index (NDVI)")
      NDVI <- ((band8 - band4) / (band8 + band4))
      names(NDVI) <- "NDVI"
      return(NDVI)
    }
    else if (item=="NDRE"){
      #NDRE Normalized Difference NIR/Rededge Normalized Difference Red-Edge
      print("\ncalculate Normalized Difference NIR/Rededge
            Normalized Difference Red-Edge (NDRE)")
      NDRE <- ((band8a - band5) / (band8a + band5))
      names(NDRE) <- "NDRE"
      return(NDRE)
    }
    else if (item=="MSI"){
      #MSI Moisture Stress Index
      print("\ncalculate Moisture Stress Index (MSI)")
      MSI <- band11/band8
      names(MSI) <- "MSI"
      return(MSI)
    }
    else if (item=="NDVWI"){
      #NDVWI multi-temporal normalized differencevegetation-water index
      print("\ncalculate multi-temporal normalized differencevegetation-water
            index (NDVWI)")
      NDVWI <- ((band8-band4)/(band8+band4))+((band8-band11)/(band8+band11))
      names(NDVWI) <- "NDVWI"
      return(NDVWI)
    }
    else if (item=="BRI"){
      #BRI  Browning Reflectance Index
      print("\ncalculate Browning Reflectance Index (BRI)")
      BRI <- ((1 / band3) - (1 / band5)) / band8a
      names(BRI) <- "BRI"
      return(BRI)
    }
    else if (item=="LCI"){
      # LCI Leaf Chlorophyll Index
      print("\ncalculate Leaf Chlorophyll Index (LCI)")
      LCI <- (band8  -band5)/(band8  +band4)
      names(LCI) <- "LCI"
      return(LCI)
    }
    else if (item=="LWCI"){
      # LWCI Leaf Water Content Index
      print("\ncalculate Leaf Water Content Index (LWCI)")
      LWCI<- log(1 - (band8a - band11), 1) / (-log(1 - (band8a - band11), 1))
      names(LWCI) <- "LWCI"
      return(LWCI)
    }
    else if (item=="GLI"){
      #GLI Green Leaf Index
      print("\ncalculate Green Leaf Index (GLI)")
      GLI <- ((2*band3) - band4 - band2)/((2*band3) + band4 + band2)
      names(GLI) <- ("GLI")
      return(GLI)
    }
    else if (item=="EVI"){
      # EVI Enhanced Vegetation Index
      print("\ncalculate Enhanced Vegetation Index (EVI)")
      EVI <- 2.5 * ((band8a - band5)/((band8a + (6*band4) - (7.5*band2)) + 1))
      names(EVI) <- "EVI"
      return(EVI)
    }
    else if (item=="AFRI1600"){
      #AFRI1600 Aerosol free vegetation Index 1600
      print("\ncalculate Aerosol free vegetation Index 1600 (AFRI1600)")
      AFRI1600 <- band8a - (0.66*(band11 / (band8a + (0.66 * band11))))
      names(AFRI1600) <- "AFRI1600"
      return(AFRI1600)
    }
    else if (item=="MSR670"){
      #MSR670  Modified Simple Ratio 670,800
      print("\ncalculate Modified Simple Ratio 670,800 (MSR670)")
      MSR670 <- (band8 / band4 - 1) / sqrt(band8 / band4 +1)
      names(MSR670) <- "MSR670"
      return(MSR670)
    }
    else if (item=="CIrededge"){
      #CIrededge Chlorophyll IndexRedEdge
      print("\ncalculate Chlorophyll IndexRedEdge (CIrededge)")
      CIrededge <- band8a/band5 -1
      names(CIrededge) <- "CIrededge"
      return(CIrededge)
    }
    else if (item=="CIgreen"){
      #CIgreen 	Chlorophyll Index Green
      print("\ncalculate  	Chlorophyll Index Green (CIgreen)")
      CIgreen <- band8a/band3 -1
      names(CIgreen) <- "CIgreen"
      return(CIgreen)
    }
    else if (item=="MVI"){
      #MVI  Mid-infrared vegetation index
      print("\ncalculate Mid-infrared vegetation index (MVI)")
      MVI <- band8a / band11
      names(MVI) <- "MVI"
      return(MVI)
    }
    else if (item=="NDMI"){
      #NDMI Normalized Difference Moisture Index
      print("\ncalculate Normalized Difference Moisture Index (NDMI)")
      NDMI <- (band8 - band11) / (band8 + band11)
      names(NDMI) <- "NDMI"
      return(NDMI)
    }
    else if(item=="DSWI4"){
      #DSWI4  Disease-Water Stress Index 4
      print("\ncalculate Disease-Water Stress Index 4 (DSWI4)")
      DSWI4 <- band3 / band4
      names(DSWI4) <- "DSWI4"
      return(DSWI4)
    }
  })
  return(raster::stack(indices))
}
###function for automatized package checking, installing, and requiring

# "check library" is a function for automated checking if librarys
#are installed and required, if they are not it will automatically run it
# Example function Input: pck = c("base","raster","tidyverse")

check_library <- function(pck){
  for (i in 1:length(pck)){
    if(pck[i] %in% rownames(installed.packages()) == FALSE) {
      install.packages(pck[i])}
  }
  lapply(pck, require, character.only = TRUE)
}
##### This function checks if the studyarea and a rasterstack do overlap #####
# output is the index of the rasters which: overlap, just intersect a part of
#the area, not overlap

check_overlap <- function(RS_area_check, studyarea){
  Rnotoverlap <- c()
  Rintersect <- c()
  Rcontains <- c()
  for (i in 1:nlayers(RS_area_check)){
    dummi <- as(extent(RS_area_check[[i]]), "SpatialPolygons")
    crs(dummi) <- projection(RS_area_check[[i]])
    studyarea<- spTransform(studyarea, projection(RS_area_check[[i]]))
    if (gContains(dummi,studyarea)){
      print(paste0("Raster ",i," extent is fully within the Area of interest"))
      a <- i
      Rcontains <- c(Rcontains, a)
      next
    }else if(gIntersects(studyarea, dummi)){
      print(paste0("Raster ",i," extent is not fully
                   within the Area of interest"))
      b <- i
      Rintersect <- c(Rintersect, b)
      next
    }else{
      print(paste0("Raster ",i," extent is fully outside the Area of interest"))
      c <- i
      Rnotoverlap <- c(Rnotoverlap, c)
      next
    }
  }
}
#### functions for calculationg raster sattistics
#functions can be parallelice with clusterR()
#https://cran.rstudio.com/web/packages/raster/raster.pdf#page.50

## 75th quantile
fq75 <-function(x) {
  calc(x, fun = function(a) {quantile(a,probs = .75,na.rm=TRUE)})
}

## 50th quantile
fq50 <- function(x) {
  calc(x, fun = function(a) {quantile(a,probs = .5,na.rm=TRUE)})
}

## 25th quantile
fq25 <- function(x) {
  calc(x, fun = function(a) {quantile(a,probs = .25,na.rm=TRUE)})
}

## standard deviation

fqsd <- function(x) {
  calc(x, fun=sd, na.rm=TRUE)
}

############Python Functions

# Need to configure Python to use the SNAP-Python (snappy)
#interface(https://senbox.atlassian.net/wiki/spaces/SNAP/pages/
#50855941/Configure+Python+to+use+the+SNAP-Python+snappy+interface)
# Read in unzipped Sentinel-1 GRD products (EW and IW modes)
# Parameters to provide: path, outpath, region_code

import datetime
import time
from snappy import ProductIO
from snappy import HashMap
from snappy import jpy
import os, gc
from snappy import GPF

def do_ratio_VH_VV(source):
  print('\calc ratio between VH_VV...')
parameters = HashMap()
BandDescriptor = jpy.get_type(
  'org.esa.snap.core.gpf.common.BandMathsOp$BandDescriptor')
targetBand_1 = BandDescriptor()
targetBand_1.name = 'Sigma0_ratio_db'
targetBand_1.type = 'float32'
targetBand_1.expression = 'Sigma0_VH_db / Sigma0_VV_db'
targetBands = jpy.array(
  'org.esa.snap.core.gpf.common.BandMathsOp$BandDescriptor', 1)
targetBands[0] = targetBand_1
parameters.put('targetBands', targetBands)
output = GPF.createProduct('BandMaths', parameters, source)
return output

def do_merge_products(source_1,source_2):
  print('\merging products...')
parameters = HashMap()
NodeDescriptor = jpy.get_type(
  'org.esa.snap.core.gpf.common.MergeOp$NodeDescriptor')
band_1 = NodeDescriptor()
band_1.setProductId('masterProduct')
band_1.setNamePattern('Sigma0_VH_db')

band_2 = NodeDescriptor()
band_2.setProductId('masterProduct')
band_2.setNamePattern('Sigma0_VV_db')

band_3 = NodeDescriptor()
band_3.setProductId('slaveProduct1')
band_3.setNamePattern('Sigma0_ratio_db')

included_bands = jpy.array(
  'org.esa.snap.core.gpf.common.MergeOp$NodeDescriptor', 3)
included_bands[0] = band_1
included_bands[1] = band_2
included_bands[2] = band_3

sourceProducts = HashMap()
sourceProducts.put('masterProduct', source_1)
sourceProducts.put('slaveProduct1', source_2)

parameters.put('includes', included_bands)
output = GPF.createProduct('Merge', parameters, sourceProducts)
bands = output.getBandNames()
print list(bands)
return output


def do_apply_orbit_file(source):
  print('\tApply orbit file...')
parameters = HashMap()
parameters.put('Apply-Orbit-File', True)
output = GPF.createProduct('Apply-Orbit-File', parameters, source)
return output

def do_thermal_noise_removal(source):
  print('\tThermal noise removal...')
parameters = HashMap()
parameters.put('removeThermalNoise', True)
output = GPF.createProduct('ThermalNoiseRemoval', parameters, source)
return output

def do_calibration(source, polarization, pols):
  print('\tCalibration...')
parameters = HashMap()
parameters.put('outputSigmaBand', True)
if polarization == 'DH':
  parameters.put('sourceBands', 'Intensity_HH,Intensity_HV')
elif polarization == 'DV':
  parameters.put('sourceBands', 'Intensity_VH,Intensity_VV')
elif polarization == 'SH' or polarization == 'HH':
  parameters.put('sourceBands', 'Intensity_HH')
elif polarization == 'SV':
  parameters.put('sourceBands', 'Intensity_VV')
else:
  print("different polarization!")
parameters.put('selectedPolarisations', pols)
parameters.put('outputImageScaleInDb', False)
output = GPF.createProduct("Calibration", parameters, source)
return output

def do_speckle_filtering(source):
  print('\tSpeckle filtering...')
parameters = HashMap()
parameters.put('filter', 'Lee')
parameters.put('filterSizeX', 5)
parameters.put('filterSizeY', 5)
output = GPF.createProduct('Speckle-Filter', parameters, source)
return output

def do_terrain_correction(source, proj, downsample, polarization):
  print('\tTerrain correction...')
parameters = HashMap()
parameters.put('demName', 'SRTM 3sec')
parameters.put('imgResamplingMethod', 'NEAREST_NEIGHBOUR')
parameters.put('demResamplingMethod', 'NEAREST_NEIGHBOUR')
#parameters.put('mapProjection', proj)
# comment this line if no need to convert to UTM/WGS84, default is WGS84
parameters.put('saveProjectedLocalIncidenceAngle', False)
parameters.put('saveSelectedSourceBand', True)
if polarization == 'DH':
  parameters.put('sourceBands', 'Sigma0_HH,Sigma0_HV')
elif polarization == 'DV':
  parameters.put('sourceBands', 'Sigma0_VH,Sigma0_VV')
elif polarization == 'SH' or polarization == 'HH':
  parameters.put('sourceBands', 'Sigma0_HH')
elif polarization == 'SV':
  parameters.put('sourceBands', 'Sigma0_VV')
else:
  print("different polarization!")
#while downsample == 1:                      # downsample: 1
#-- need downsample to 40m, 0 -- no need to downsample
parameters.put('pixelSpacingInMeter', 10.0)
# break
output = GPF.createProduct('Terrain-Correction', parameters, source)
return output

def do_subset(source, wkt):
  print('\tSubsetting...')
parameters = HashMap()
parameters.put('geoRegion', wkt)
output = GPF.createProduct('Subset', parameters, source)
return output

def do_convert_linear_to_db(source, polarization):
  print('\converting to db...')
parameters = HashMap()
if polarization == 'DH':
  parameters.put('sourceBands', 'Sigma0_HH,Sigma0_HV')
elif polarization == 'DV':
  parameters.put('sourceBands', 'Sigma0_VH,Sigma0_VV')
elif polarization == 'SH' or polarization == 'HH':
  parameters.put('sourceBands', 'Sigma0_HH')
elif polarization == 'SV':
  parameters.put('sourceBands', 'Sigma0_VV')
else:
  print("different polarization!")
output = GPF.createProduct("LinearToFromdB", parameters, source)
return output
