### function for calculating vegetation indices on sentinel 2 or aerial Images RED GREEN BLUE
# also Landsat is possible, please check bandnumber by wavelength


#band1 = 442nm Wavelength AEROSOL
#band2 = 492nm Wavelength BLUE
#band3 = 559nm wavelength GREEN
#band4 = 664nm wavelength RED
#band5 = 704nm wavelength RED-edge
#band8 = 832nm Wavelength NIR
#band8a = 864nm Wavelength NIR
#band11 = 1614 nm Wavelength SWIR
#band12 = 2202 nm Wavelength SWIR

#rgbi = Vegetation Index you want to calculate


calc_Indices<- function(band1 = NULL, band2 = NULL, band3 = NULL, band4 = NULL, band5 = NULL, band8 = NULL, band8a = NULL,
                        band11 = NULL, band12 = NULL, 
                        rgbi=c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI", "ExGR", "VEG")){
  
  ## compatibility check
  if (raster::nlayers(bands) < 3)
    stop("Argument 'bands' needs to be a Raster* object with at least 9 layers (usually red, green and blues, NIR, SWIR...).")
  
  ### processing

indices <- lapply(rgbi, function(item){
  ## calculate Visible Vegetation Index vvi
  if (item=="VVI"){
    print("\ncalculate Visible Vegetation Index (VVI)")
    VVI <- (1 - abs((band4 - 30) / (band4 + 30))) * (1 - abs((band3 - 50) / (band3 + 50))) * (1 - abs((band2 - 1) / (band2 + 1)))
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
     
    print("\ncalculate Excess Green Index (ExG)")
    ExG <- 2*band3 - band4 - band2 
    names(ExG) <- "ExG"
    return(ExG)
    
  } else if (item=="ExGR"){
    
    print("\ncalculate  Excess Green Index - Excess Red Index (ExGR)")
    ExGR<- (2*band3 - band4 - band2) - (1.4*band4 - band3)
    names(ExGR) <- "ExGR"
    return(ExGR)
    
  } else if (item=="VEG"){
     
    print("\ncalculate  Vegetative Index (VEG)")
    VEG<- band3 / (band4**0.667 * band2**0.333)
    names(VEG) <- "VEG"
    return(VEG)
    
  } else if (item=="CIVE"){
    
    print("\ncalculate Color Index of Vegetation Extraction (CIVE)")
    CIVE<- 0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745
    names(CIVE) <- "CIVE"
    return(CIVE)
    
  } else if (item=="COM"){
     
    print("\ncalculate Combined Index (COM)")
    COM<- 0.25*(2*band3 - band4 - band2) + 0.3* ((2*band3 - band4 - band2) - (1.4*band4 - band3)) + 0.33* (0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745) + 0.12* (band3 / (band4**0.667 * band2**0.333))
    names(COM) <- "COM"
    return(COM)
    
  } else if (item=="CEV"){
    
    print("\ncalculate Combination aus Ponti (CEV)")
    CEV<- 0.33*((1 - abs((band4 - 30) / (band4 + 30))) * (1 - abs((band3 - 50) / (band3 + 50))) * (1 - abs((band2 - 1) / (band2 + 1)))) + 0.33*(2*band3 - band4 - band2) + 0.33*(0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745)
    names(CEV) <- "CEV"
    return(CEV)
    
  } else if (item=="CEV_gew"){
    
    print("\ncalculate Combination aus Ponti (CEV_gew)")
    CEV_gew<- 0.4*((1 - abs((band4 - 30) / (band4 + 30))) * (1 - abs((band3 - 50) / (band3 + 50))) * (1 - abs((band2 - 1) / (band2 + 1)))) + 0.2*(2*band3 - band4 - band2) + 0.4*(0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745)
    names(CEV_gew) <- "CEV_gew"
    return(CEV_gew)
    
  } else if (item=="CEV_ohne"){
    
    print("\ncalculate Combination aus Ponti (CEV_ohne)")
    CEV_ohne<- ((1 - abs((band4 - 30) / (band4 + 30))) * (1 - abs((band3 - 50) / (band3 + 50))) * (1 - abs((band2 - 1) / (band2 + 1)))) + (2*band3 - band4 - band2) + (0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745)
    names(CEV_ohne) <- "CEV_ohne"
    return(CEV_ohne)
    
  } else if (item=="mcfesti"){
     
    print("\ncalculate mcfesti (mcfesti)")
    mcfesti <- 0.25*(sqrt((band4**2+band3**2+band2*2)/3)) + 0.25*(0.441*band4 - 0.881*band3 + 0.385*band2 + 18.78745) + 0.25*((1 - abs((band4 - 30) / (band4 + 30))) * (1 - abs((band3 - 50) / (band3 + 50))) * (1 - abs((band2 - 1) / (band2 + 1)))) + 0.25*(-0.5*(190*(band4 - band3)- 120*(band4 - band2)))
    names(mcfesti) <- "mcfesti"
    return(mcfesti)
  }
  
  else if (item=="CCCI"){
    
    print("\ncalculate Canopy Chlorophyll Content Index (CCCI)")
    CCCI <- ((band8a - band5) / (band8a + band5)) / ((band8a - band4) / (band8a + band4))
    names(CCCI) <- "CCCI"
    return(CCCI)
  }
  
  else if (item=="CVI"){
    
    print("\ncalculate Chlorophyll vegetation index  (CVI)")
    CVI <- band8a * (band5 / band3)
    names(CVI) <- "CVI"
    return(CVI)
  }
  else if (item=="GNDVI"){
    
    print("\ncalculate Green Normalized Difference Vegetation Index (GNDVI)")
    GNDVI <- (band8a - band3)/(band8a + band3)
    names(GNDVI) <- "GNDVI"
    return(GNDVI)
  }
  else if (item=="MCARI"){
     
    print("\ncalculate Modified Chlorophyll Absorption in Reflectance Index (MCARI)")
    MCARI <- ((band5 - band4)-0.2*(band5 - band3))*(band5 / band4)
    names(MCARI) <- "MCARI"
    return(MCARI)
  }
  else if (item=="NDVI"){
    
    print("\ncalculate Normalized Differenced Vegetation Index (NDVI)")
    NDVI <- ((band8 - band4) / (band8 + band4))
    names(NDVI) <- "NDVI"
    return(NDVI)
  }
  else if (item=="NDRE"){
    
    print("\ncalculate Normalized Difference NIR/Rededge Normalized Difference Red-Edge (NDRE)")
    NDRE <- ((band8a - band5) / (band8a + band5))
    names(NDRE) <- "NDRE"
    return(NDRE)
  }
  else if (item=="MSI"){
     
    print("\ncalculate Moisture Stress Index (MSI)")
    MSI <- band11/band8
    names(MSI) <- "MSI"
    return(MSI)
  }
  else if (item=="NDVWI"){
    
    print("\ncalculate multi-temporal normalized differencevegetation-water index (NDVWI)")
    NDVWI <- ((band8-band4)/(band8+band4))+((band8-band11)/(band8+band11))
    names(NDVWI) <- "NDVWI"
    return(NDVWI)
  }
  else if (item=="BRI"){
    
    print("\ncalculate Browning Reflectance Index (BRI)")
    BRI <- ((1 / band3) - (1 / band5)) / band8a
    names(BRI) <- "BRI"
    return(BRI)
  }
  else if (item=="LCI"){
    print("\ncalculate Leaf Chlorophyll Index (LCI)")
    LCI <- (band8  -band5)/(band8  +band4)
    names(LCI) <- "LCI"
    return(LCI)
  }
  else if (item=="LWCI"){
    print("\ncalculate Leaf Water Content Index (LWCI)")
    LWCI<- log(1 - (band8a - band11), 1) / (-log(1 - (band8a - band11), 1))
    names(LWCI) <- "LWCI"
    return(LWCI)
  }
  else if (item=="GLI"){
    print("\ncalculate Green Leaf Index (GLI)")
    GLI <- ((2*band3) - band4 - band2)/((2*band3) + band4 + band2)
    names(GLI) <- ("GLI")
    return(GLI)
  }
  else if (item=="EVI"){
    print("\ncalculate Enhanced Vegetation Index (EVI)")
    EVI <- 2.5 * ((band8a - band5)/((band8a + (6*band4) - (7.5*band2)) + 1))
    names(EVI) <- "EVI"
    return(EVI)
  }
  else if (item=="AFRI1600"){
    print("\ncalculate Aerosol free vegetation Index 1600 (AFRI1600)")
    AFRI1600 <- band8a - (0.66*(band11 / (band8a + (0.66 * band11))))
    names(AFRI1600) <- "AFRI1600"
    return(AFRI1600)
  }
  else if (item=="MSR670"){
    print("\ncalculate Modified Simple Ratio 670,800 (MSR670)")
    MSR670 <- (band8 / band4 - 1) / sqrt(band8 / band4 +1)
    names(MSR670) <- "MSR670"
    return(MSR670)
  }
  else if (item=="CIrededge"){
    print("\ncalculate Chlorophyll IndexRedEdge (CIrededge)")
    CIrededge <- band8a/band5 -1
    names(CIrededge) <- "CIrededge"
    return(CIrededge)
  }
  else if (item=="CIgreen"){
    print("\ncalculate  	Chlorophyll Index Green (CIgreen)")
    CIgreen <- band8a/band3 -1
    names(CIgreen) <- "CIgreen"
    return(CIgreen)
  }
  else if (item=="MVI"){
    print("\ncalculate Mid-infrared vegetation index (MVI")
    MVI <- band8a / band11
    names(MVI) <- "MVI"
    return(MVI)
  }
  else if (item=="NDMI"){
    print("\ncalculate Normalized Difference Moisture Index (NDMI")
    NDMI <- (band8 - band11) / (band8 + band11)
    names(NDMI) <- "NDMI"
    return(NDMI)
  }
  else if(item=="DSWI4"){
    print("\ncalculate Disease-Water Stress Index 4 (DSWI4")
    DSWI4 <- band3 / band4
    names(DSWI4) <- "DSWI4"
    return(DSWI4)
  }
})
return(raster::stack(indices))
}