# -*- coding: utf-8 -*-
"""
Created on Thu Aug 13 08:44:46 2020

@author: mmues
"""


# Need to configure Python to use the SNAP-Python (snappy) interface(https://senbox.atlassian.net/wiki/spaces/SNAP/pages/50855941/Configure+Python+to+use+the+SNAP-Python+snappy+interface)
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
    BandDescriptor = jpy.get_type('org.esa.snap.core.gpf.common.BandMathsOp$BandDescriptor')
    targetBand_1 = BandDescriptor()
    targetBand_1.name = 'Sigma0_ratio_db'
    targetBand_1.type = 'float32'
    targetBand_1.expression = 'Sigma0_VH_db / Sigma0_VV_db'
    targetBands = jpy.array('org.esa.snap.core.gpf.common.BandMathsOp$BandDescriptor', 1)
    targetBands[0] = targetBand_1
    parameters.put('targetBands', targetBands)
    output = GPF.createProduct('BandMaths', parameters, source)
    return output

def do_merge_products(source_1,source_2):
    print('\merging products...')
    parameters = HashMap()
    NodeDescriptor = jpy.get_type('org.esa.snap.core.gpf.common.MergeOp$NodeDescriptor')
    band_1 = NodeDescriptor()
    band_1.setProductId('masterProduct')
    band_1.setNamePattern('Sigma0_VH_db')
    
    band_2 = NodeDescriptor()
    band_2.setProductId('masterProduct')
    band_2.setNamePattern('Sigma0_VV_db')

    band_3 = NodeDescriptor()
    band_3.setProductId('slaveProduct1')
    band_3.setNamePattern('Sigma0_ratio_db')

    included_bands = jpy.array('org.esa.snap.core.gpf.common.MergeOp$NodeDescriptor', 3)
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
    #parameters.put('mapProjection', proj)       # comment this line if no need to convert to UTM/WGS84, default is WGS84
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
    #while downsample == 1:                      # downsample: 1 -- need downsample to 40m, 0 -- no need to downsample
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
