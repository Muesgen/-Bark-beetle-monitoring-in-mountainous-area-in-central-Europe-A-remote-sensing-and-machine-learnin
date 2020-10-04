# -*- coding: utf-8 -*-
########## Setup Script ##########
#---
# title: "Snappy Preprocessing"
# Author: Marvin Müsgen
# Description: Preprocess Sentinel 1 Images via the snap python interface snappy
# The scripts sources functions from the function folder. 
# Preproccessing steps are: Thermal noise removal, apply orbit file, radiometric calibration, 
# terrain correction, subsetting, converting to db and calculate polarization ratio
# The  function calling the gpt java operator which execute the commands
#---

### Make sure that the Sentinel 1 folders ar eunzipped in the given path

#import needed librarys
import datetime
import time
import snappy
import shapefile
import pygeoif
from snappy import ProductIO
from snappy import HashMap
import os, gc   
from snappy import GPF
from snappy import jpy

#importing Preproc functions
os.getcwd()
os.chdir("D:\\Marvin\\BB_rf\\functions")
from PreProcSen_1 import (do_speckle_filtering, do_calibration, do_apply_orbit_file, do_thermal_noise_removal,do_terrain_correction,do_subset, do_convert_linear_to_db, do_ratio_VH_VV, do_merge_products)
os.chdir("D:\\Marvin\\BB_rf")

#reading the area of interest
r = shapefile.Reader("D:\Marvin\NP_Boundary.shp")
g=[]
for s in r.shapes():
    g.append(pygeoif.geometry.as_shape(s)) 

m = pygeoif.MultiPoint(g)

print m.wkt

GPF.getDefaultInstance().getOperatorSpiRegistry().loadOperatorSpis()
HashMap = snappy.jpy.get_type('java.util.HashMap')

path = "D:\Marvin\BB_rf\input\sentinel_1_data\Descending" # Supperfolder where the sentinel1 image folders are in
outputpath = "D:\Marvin\BB_rf\input\sentinel_1_preproc\Descending" # setting the output path
#setting projection: default WGS84
#proj = '''PROJCS["UTM Zone 4 / World Geodetic System 1984",GEOGCS["World Geodetic System 1984",DATUM["World Geodetic System 1984",SPHEROID["WGS 84", 6378137.0, 298.257223563, AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich", 0.0, AUTHORITY["EPSG","8901"]],UNIT["degree", 0.017453292519943295],AXIS["Geodetic longitude", EAST],AXIS["Geodetic latitude", NORTH]],PROJECTION["Transverse_Mercator"],PARAMETER["central_meridian", -159.0],PARAMETER["latitude_of_origin", 0.0],PARAMETER["scale_factor", 0.9996],PARAMETER["false_easting", 500000.0],PARAMETER["false_northing", 0.0],UNIT["m", 1.0],AXIS["Easting", EAST],AXIS["Northing", NORTH]]'''
proj = "WGS84"
count = 0
for folder in os.listdir(path):
   ##Reading the Sentinel1 data
   gc.enable()
   gc.collect()
   #output = "D:\Marvin\Testing\input\sentinel_1_preproc"  
   timestamp = folder.split("_")[4] 
   date = timestamp[:8]
   loopstarttime=str(datetime.datetime.now())
   print('Start time:', loopstarttime, "loopnumber:", count)
   start_time = time.time()
   
   sentinel_1 = ProductIO.readProduct(path + "\\" + folder + "\\manifest.safe")    
   print sentinel_1
   ## Extract mode, product type, and polarizations from filename
   modestamp = folder.split("_")[1]
   productstamp = folder.split("_")[2]
   polstamp = folder.split("_")[3]

   polarization = polstamp[2:4]
   if polarization == 'DV':
       pols = 'VH,VV'
   elif polarization == 'DH':
       pols = 'HH,HV'
   elif polarization == 'SH' or polarization == 'HH':
       pols = 'HH'
   elif polarization == 'SV':
       pols = 'VV'
   else:
          print("Polarization error!")
   
   ### crop image to extent of area of interes
   #sentinel_1s = do_subset(sentinel_1, m.wkt)
   
   ### Remove thermal noise
   sentinel_1proc = do_thermal_noise_removal(sentinel_1)
   
   ### aplly orbit filter
   sentinel_1proc = do_apply_orbit_file(sentinel_1proc)
   
   ### calibrate
   sentinel_1proc = do_calibration(sentinel_1proc,polarization,pols)
   
   ### speckle Filtering
   sentinel_1proc = do_speckle_filtering(sentinel_1proc)
   
   ###Terrain Correction
   ## IW images can be downsampled from 10m to 40m (the same resolution as EW images). by changing downsample = 1
   if (modestamp == 'IW' and productstamp == 'GRDH') or (modestamp == 'EW' and productstamp == 'GRDH'):
       down_tercorrected = do_terrain_correction(sentinel_1proc, proj, 0, polarization)
       down_subset = do_subset(down_tercorrected, m.wkt)
       print("Converting_linearToFromdB...")
       band_db = do_convert_linear_to_db(down_subset, polarization)
       ratio = do_ratio_VH_VV(band_db)
       Bandmerge = do_merge_products(band_db,ratio)
       #final_preproc = do_convert_linear_to_db(Bandmerge, polarization)
       print("Writng...")
       ProductIO.writeProduct(Bandmerge, outputpath + '\\' + folder[:-5] + '_10', 'GeoTIFF')           
       del down_tercorrected
   elif modestamp == 'EW' and productstamp == 'GRDM':
       tercorrected = do_terrain_correction(sentinel_1proc, proj, 0, polarization)
       subset = do_subset(tercorrected, m.wkt)
       band_db = do_convert_linear_to_db(subset, polarization)
       ratio = do_ratio_VH_VV(band_db)
       Bandmerge = do_merge_products(band_db,ratio)
       #final_preproc = do_convert_linear_to_db(Bandmerge, polarization)
       print("Writng...")
       ProductIO.writeProduct(Bandmerge, outputpath + '\\' + folder[:-5] + '_10', 'GeoTIFF')
       del tercorrected
   else:
       print("Different spatial resolution is found.")
       down = 1
              
   print('Done.')
   sentinel_1.dispose()
   sentinel_1.closeIO()
   print("--- %s seconds ---" % (time.time() - start_time))
   count += 1

   
   
