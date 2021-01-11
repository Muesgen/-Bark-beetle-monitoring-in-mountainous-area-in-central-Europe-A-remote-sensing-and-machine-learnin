shp <- rgdal::readOGR("E:/Marvin/BB_rf/input/observations/NP_Boundary.shp")

if(shp@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs"){
  shp <- spTransform(shp, "+proj=longlat +datum=WGS84 +no_defs")
}
shp.wkt <- paste0("POLYGON ((",shp@bbox[1,1]," ",shp@bbox[2,2],",",shp@bbox[1,1]," ",shp@bbox[2,1],",",shp@bbox[1,2]," ",shp@bbox[2,1],",",shp@bbox[1,2],
                      " ",shp@bbox[2,2],",",shp@bbox[1,1]," ",shp@bbox[2,2],"))")

files <- list.files("E:/Marvin/BB_rf/input/sentinel_1_data/Ascending", full.names=TRUE)
files <- paste0(files,"/manifest.safe")
pbapply::pbapply(files, function(x){
  n <- length(strsplit(x, "/")[[1]])
  out <- strsplit(x, "/")[[1]][n-1]
  out <- substr(out, 1, nchar(out)-5)
  outfile <- paste0("E:/Marvin/BB_rf/input/sentinel_1_preproc2/Ascending/",out,".tif")
  
prefix.xml <- paste0("<graph id='Graph'>
                        <version>1.0</version>
                        <node id='Read'>
                          <operator>Read</operator>
                          <sources/>
                          <parameters>
                            <file>",x,"</file>
                          </parameters>
                        </node>
                        <node id='Apply-Orbit-File'>
                          <operator>Apply-Orbit-File</operator>
                          <sources>
                            <sourceProduct refid='Read'/>
                          </sources>
                          <parameters>
                            <orbitType>Sentinel Precise (Auto Download)</orbitType>
                            <polyDegree>3</polyDegree>
                            <continueOnFail>false</continueOnFail>
                          </parameters>
                        </node>
                        <node id='ThermalNoiseRemoval'>
                          <operator>ThermalNoiseRemoval</operator>
                          <sources>
                            <sourceProduct refid='Apply-Orbit-File'/>
                          </sources>
                          <parameters>
                            <selectedPolarisations>VH,VV</selectedPolarisations>
                            <removeThermalNoise>true</removeThermalNoise>
                            <reIntroduceThermalNoise>false</reIntroduceThermalNoise>
                          </parameters>
                        </node>
                        <node id='Calibration'>
                          <operator>Calibration</operator>
                          <sources>
                            <sourceProduct refid='ThermalNoiseRemoval'/>
                          </sources>
                          <parameters>
                            <sourceBands/>
                            <auxFile>Product Auxiliary File</auxFile>
                            <externalAuxFile/>
                            <outputImageInComplex>false</outputImageInComplex>
                            <outputImageScaleInDb>false</outputImageScaleInDb>
                            <createGammaBand>false</createGammaBand>
                            <createBetaBand>false</createBetaBand>
                            <selectedPolarisations>VH,VV</selectedPolarisations>
                            <outputSigmaBand>false</outputSigmaBand>
                            <outputGammaBand>false</outputGammaBand>
                            <outputBetaBand>true</outputBetaBand>
                          </parameters>
                          </node>
                          <node id='Speckle-Filter'>
                            <operator>Speckle-Filter</operator>
                            <sources>
                              <sourceProduct refid='Calibration'/>
                            </sources>
                            <parameters>
                              <sourceBands>Beta0_VH,Beta0_VV</sourceBands>
                              <filter>Lee</filter>
                              <filterSizeX>7</filterSizeX>
                              <filterSizeY>7</filterSizeY>
                              <dampingFactor>2</dampingFactor>
                              <estimateENL>true</estimateENL>
                              <enl>1.0</enl>
                              <numLooksStr>1</numLooksStr>
                              <windowSize>7x7</windowSize>
                              <targetWindowSizeStr>3x3</targetWindowSizeStr>
                              <sigmaStr>0.9</sigmaStr>
                              <anSize>50</anSize>
                            </parameters>
                          </node>
                          <node id='Terrain-Flattening'>
                            <operator>Terrain-Flattening</operator>
                            <sources>
                              <sourceProduct refid='Speckle-Filter'/>
                            </sources>
                            <parameters>
                              <sourceBands>Beta0_VH,Beta0_VV</sourceBands>
                              <demName>SRTM 1Sec HGT</demName>
                              <demResamplingMethod>BILINEAR_INTERPOLATION</demResamplingMethod>
                              <externalDEMFile/>
                              <externalDEMNoDataValue>0.0</externalDEMNoDataValue>
                              <externalDEMApplyEGM>false</externalDEMApplyEGM>
                              <outputSimulatedImage>false</outputSimulatedImage>
                              <additionalOverlap>0.1</additionalOverlap>
                              <oversamplingMultiple>1.0</oversamplingMultiple>
                            </parameters>
                          </node>
                          <node id='Terrain-Correction'>
                            <operator>Terrain-Correction</operator>
                            <sources>
                              <sourceProduct refid='Terrain-Flattening'/>
                            </sources>
                            <parameters>
                              <sourceBands>Gamma0_VH,Gamma0_VV</sourceBands>
                              <demName>SRTM 1Sec HGT</demName>
                              <externalDEMFile/>
                              <externalDEMNoDataValue>0.0</externalDEMNoDataValue>
                              <externalDEMApplyEGM>true</externalDEMApplyEGM>
                              <demResamplingMethod>BILINEAR_INTERPOLATION</demResamplingMethod>
                              <imgResamplingMethod>BILINEAR_INTERPOLATION</imgResamplingMethod>
                              <pixelSpacingInMeter>10.0</pixelSpacingInMeter>
                              <pixelSpacingInDegree>8.983152841195215E-5</pixelSpacingInDegree>
                              <mapProjection>GEOGCS[&quot;WGS84(DD)&quot;, &#xd; DATUM[&quot;WGS84&quot;, &#xd;
                                                                            SPHEROID[&quot;WGS84&quot;, 6378137.0, 298.257223563]], &#xd;
                                                                    PRIMEM[&quot;Greenwich&quot;, 0.0], &#xd;
                                                                    UNIT[&quot;degree&quot;, 0.017453292519943295], &#xd;
                                                                    AXIS[&quot;Geodetic longitude&quot;, EAST], &#xd;
                                                                    AXIS[&quot;Geodetic latitude&quot;, NORTH]]</mapProjection>
                              <alignToStandardGrid>false</alignToStandardGrid>
                              <standardGridOriginX>0.0</standardGridOriginX>
                              <standardGridOriginY>0.0</standardGridOriginY>
                              <nodataValueAtSea>true</nodataValueAtSea>
                              <saveDEM>false</saveDEM>
                              <saveLatLon>false</saveLatLon>
                              <saveIncidenceAngleFromEllipsoid>false</saveIncidenceAngleFromEllipsoid>
                              <saveLocalIncidenceAngle>false</saveLocalIncidenceAngle>
                              <saveProjectedLocalIncidenceAngle>false</saveProjectedLocalIncidenceAngle>
                              <saveSelectedSourceBand>true</saveSelectedSourceBand>
                              <outputComplex>false</outputComplex>
                              <applyRadiometricNormalization>false</applyRadiometricNormalization>
                              <saveSigmaNought>false</saveSigmaNought>
                              <saveGammaNought>false</saveGammaNought>
                              <saveBetaNought>false</saveBetaNought>
                              <incidenceAngleForSigma0>Use projected local incidence angle from DEM</incidenceAngleForSigma0>
                              <incidenceAngleForGamma0>Use projected local incidence angle from DEM</incidenceAngleForGamma0>
                              <auxFile>Latest Auxiliary File</auxFile>
                              <externalAuxFile/>
                            </parameters>
                          </node>
                          <node id='LinearToFromdB'>
                            <operator>LinearToFromdB</operator>
                            <sources>
                              <sourceProduct refid='Terrain-Correction'/>
                            </sources>
                            <parameters>
                              <sourceBands>Gamma0_VH,Gamma0_VV</sourceBands>
                            </parameters>
                          </node>
                          <node id='Subset'>
                            <operator>Subset</operator>
                            <sources>
                              <sourceProduct refid='LinearToFromdB'/>
                            </sources>
                            <parameters>
                              <sourceBands>Gamma0_VH_db,Gamma0_VV_db</sourceBands>
                              <region>0,0,0,0</region>
                              <referenceBand/>
                              <geoRegion>",shp.wkt,"</geoRegion>
                              <subSamplingX>1</subSamplingX>
                              <subSamplingY>1</subSamplingY>
                              <fullSwath>false</fullSwath>
                              <tiePointGridNames/>
                              <copyMetadata>true</copyMetadata>
                            </parameters>
                          </node>
                          <node id='Write'>
                            <operator>Write</operator>
                            <sources>
                              <sourceProduct refid='Subset'/>
                            </sources>
                            <parameters>
                              <file>",outfile,"</file>
                              <formatName>GeoTIFF-BigTIFF</formatName>
                            </parameters>
                          </node>
                        </graph>")
doc <- xmlTreeParse(prefix.xml, useInternalNodes = T) # PARSE STRING
root <- xmlRoot(doc) #Find Roots  
f <- paste0("E:/Marvin/BB_rf/input/sentinel_1_preproc2/xml/",x,".xml")
cat(saveXML(doc), file= f)
system(paste("gpt", f))
  
})