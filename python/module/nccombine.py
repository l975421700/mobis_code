##################Function to combine netcdf file

import numpy as np
import os
from netCDF4 import Dataset
import re

def nccomb(ffolder, matchstr, retfile, variable, dim = ['time', 'lat', 'lon']):
    '''
    parameter
    --------
    ffolder : folder containing nc file to be combined
    matchstr : a string used to select certain files
    retfile : the file to store the combined result
    dim : dimensions to be created
    variable : variable in the nc file to be combined
    
    Return
    --------
    
    '''
    
    flist = np.sort([s for s in os.listdir(ffolder)
                        if re.match(matchstr, s)])
    file1 = Dataset(ffolder + '/' + flist[0],'r')
    destination = Dataset(retfile, 'w')
    
    #copy the attributes from file1 to destination
    cpncatt(file1, destination, variable, dim, copyvalue = True)
    
    file1.close() 
    
    for i in range(1,len(flist)):
        file = Dataset(ffolder + '/' + flist[i],'r')
        destination.variables[variable][:] = np.concatenate(
            (destination.variables[variable][:], 
             file.variables[variable][:]), axis = 0)
        file.close()
    destination.close()
    
       
    

'''
wdir='/Users/gao/OneDrive - business/Research/Study in Korea/'
os.chdir(wdir + 'Data')
file1 = Dataset('CMIP5/GFDL/rcp26/tas_Amon_GFDL-ESM2G_rcp26_r1i1p1_200601-201012.nc','r')
destination = Dataset('CMIP5/test.nc', 'w')
'''
    
    
    

##################Function to calculate mean of netcdf file
import numpy as np
import os
from netCDF4 import Dataset 

def ncmean(ffolder, retfile, variable, nperiod, experiment):
    '''
    parameter
    --------
    ffolder : folder containing nc file to be combined
    retfile : the file to store the combined result
    variable : variable in the nc file to be combined
    nperiod : the period for calculating mean
    experiment : 'h' or 'r', representing 'historical' or 'rcp'
                    used to extract the analysis period
    
    
    Return
    --------
    
    '''
    
    flist = np.sort([s for s in os.listdir(ffolder)
                        if s.startswith(variable)])
    
    file1 = Dataset(ffolder + '/' + flist[0],'r')
    destination = Dataset(retfile, 'w')
    
    #copy the attributes from file1 to destination
    cpncatt(file1, destination, variable)
    
    
    # Copy variables' value
    destination.variables['time'][:] = np.arange(0,nperiod)
    destination.variables['lat'][:] = file1.variables['lat'][:]
    destination.variables['lon'][:] = file1.variables['lon'][:]
    if experiment == 'h':
        period = np.arange(file1.variables[variable][:].shape[0] - nperiod, 
                           file1.variables[variable][:].shape[0])
    if experiment == 'r':
        period = np.arange(0, nperiod)
    var = file1.variables[variable][:][period, :, :]
    
    file1.close() 
    
    for i in range(1,len(flist)):
        file = Dataset(ffolder + '/' + flist[i],'r')
        if experiment == 'h':
            period = np.arange(file.variables[variable][:].shape[0] - nperiod, 
                           file.variables[variable][:].shape[0])
        if experiment == 'r':
            period = np.arange(0, nperiod)
            
        var += file.variables[variable][:][period, :, :]
        file.close()
    destination.variables[variable][:] = var/len(flist)
    destination.close()
    

'''
ffolder = 'CMIP5/regridded2.5_2.5/GFDL/historical'
retfile = 'CMIP5/ensemble_mean2.5_2.5/GFDL/pr_Amon_GFDL_historical_186101-200512.nc'
variable = 'pr'
nperiod = 1740
experiment = 'h'
ncmean(ffolder, retfile, variable, nperiod, experiment)

resnc = Dataset('CMIP5/ensemble_mean2.5_2.5/GFDL/pr_Amon_GFDL_historical_186101-200512.nc','r')

ognnc1 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r1i1p1_186101-200512.nc', 'r')
ognnc2 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r2i1p1_186101-200512.nc', 'r')
ognnc3 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r3i1p1_186101-200512.nc', 'r')
ognnc4 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r4i1p1_186101-200512.nc', 'r')
ognnc5 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r5i1p1_186101-200512.nc', 'r')
ognnc6 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r6i1p1_186101-200512.nc', 'r')
ognnc7 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r7i1p1_186101-200512.nc', 'r')
ognnc8 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r8i1p1_186101-200512.nc', 'r')
ognnc9 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r9i1p1_186101-200512.nc', 'r')
ognnc10 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM2p1_historical_r10i1p1_186101-200512.nc', 'r')
ognnc11 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM3_historical_r1i1p1_186001-200512.nc', 'r')
ognnc12 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM3_historical_r2i1p1_186001-200512.nc', 'r')
ognnc13 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM3_historical_r3i1p1_186001-200512.nc', 'r')
ognnc14 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM3_historical_r4i1p1_186001-200512.nc', 'r')
ognnc15 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-CM3_historical_r5i1p1_186001-200512.nc', 'r')
ognnc16 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-ESM2G_historical_r1i1p1_186101-200512.nc', 'r')
ognnc17 = Dataset('CMIP5/regridded2.5_2.5/GFDL/historical/pr_Amon_GFDL-ESM2M_historical_r1i1p1_186101-200512.nc', 'r')
(ognnc1.variables['pr'][:][0+1739,0:5,0:5] + ognnc2.variables['pr'][:][0+1739,0:5,0:5] + 
ognnc3.variables['pr'][:][0+1739,0:5,0:5] + ognnc4.variables['pr'][:][0+1739,0:5,0:5] + 
ognnc5.variables['pr'][:][0+1739,0:5,0:5] + ognnc6.variables['pr'][:][0+1739,0:5,0:5] + 
ognnc7.variables['pr'][:][0+1739,0:5,0:5] + ognnc8.variables['pr'][:][0+1739,0:5,0:5] + 
ognnc9.variables['pr'][:][0+1739,0:5,0:5] + ognnc10.variables['pr'][:][0+1739,0:5,0:5] + 
ognnc11.variables['pr'][:][12+1739,0:5,0:5] + ognnc12.variables['pr'][:][12+1739,0:5,0:5] + 
ognnc13.variables['pr'][:][12+1739,0:5,0:5] + ognnc14.variables['pr'][:][12+1739,0:5,0:5] + 
ognnc15.variables['pr'][:][12+1739,0:5,0:5] + ognnc16.variables['pr'][:][0+1739,0:5,0:5] + 
ognnc17.variables['pr'][:][0+1739,0:5,0:5])/17 - resnc.variables['pr'][:][0+1739,0:5,0:5]




ffolder = 'CMIP5/regridded2.5_2.5/GFDL/rcp45'
retfile = 'CMIP5/ensemble_mean2.5_2.5/GFDL/tas_Amon_GFDL_rcp45_200601-204012.nc'
variable = 'tas'
nperiod = 420
experiment = 'r'
ncmean(ffolder, retfile, variable, nperiod, experiment)

resnc = Dataset('CMIP5/ensemble_mean2.5_2.5/GFDL/tas_Amon_GFDL_rcp45_200601-204012.nc','r')

ognnc1 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r1i1p1_200601-204012.nc', 'r')
ognnc2 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r2i1p1_200601-204012.nc', 'r')
ognnc3 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r3i1p1_200601-204012.nc', 'r')
ognnc4 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r4i1p1_200601-204012.nc', 'r')
ognnc5 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r5i1p1_200601-204012.nc', 'r')
ognnc6 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r6i1p1_200601-204012.nc', 'r')
ognnc7 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r7i1p1_200601-204012.nc', 'r')
ognnc8 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r8i1p1_200601-204012.nc', 'r')
ognnc9 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r9i1p1_200601-204012.nc', 'r')
ognnc10 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM2p1_rcp45_r10i1p1_200601-204012.nc', 'r')
ognnc11 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM3_rcp45_r1i1p1_200601-210012.nc', 'r')
ognnc12 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM3_rcp45_r3i1p1_200601-210012.nc', 'r')
ognnc13 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-CM3_rcp45_r5i1p1_200601-210012.nc', 'r')
ognnc14 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-ESM2G_rcp45_r1i1p1_200601-210012.nc', 'r')
ognnc15 = Dataset('CMIP5/regridded2.5_2.5/GFDL/rcp45/tas_Amon_GFDL-ESM2M_rcp45_r1i1p1_200601-210012.nc', 'r')

(ognnc1.variables['tas'][:][0,0:5,0:5] + ognnc2.variables['tas'][:][0,0:5,0:5] + 
ognnc3.variables['tas'][:][0,0:5,0:5] + ognnc4.variables['tas'][:][0,0:5,0:5] + 
ognnc5.variables['tas'][:][0,0:5,0:5] + ognnc6.variables['tas'][:][0,0:5,0:5] + 
ognnc7.variables['tas'][:][0,0:5,0:5] + ognnc8.variables['tas'][:][0,0:5,0:5] + 
ognnc9.variables['tas'][:][0,0:5,0:5] + ognnc10.variables['tas'][:][0,0:5,0:5] + 
ognnc11.variables['tas'][:][0,0:5,0:5] + ognnc12.variables['tas'][:][0,0:5,0:5] + 
ognnc13.variables['tas'][:][0,0:5,0:5] + ognnc14.variables['tas'][:][0,0:5,0:5] + 
ognnc15.variables['tas'][:][0,0:5,0:5])/15 - resnc.variables['tas'][:][0,0:5,0:5]

'''




##################Function to copy attributes of netcdf file
import numpy as np

def cpncatt(srcnc, dstnc, variable, dim = ['time', 'lat', 'lon'], 
            copyvalue = False):
    '''
    parameter
    --------
    srcnc : source netcdf file providing attributes
    dstnc : destination netcdf file needing attributes
    variable : variable to be created
    dim : dimensions to be created
    copyvalue : default False, not copy the value of variables
    
    Return
    --------
    
    '''
    # Copy global attributes 
    dstnc.setncatts({a:srcnc.getncattr(a) for a in srcnc.ncattrs()}) 
    
    #create dimensions
    dstnc.createDimension(dim[0], None)
    dstnc.createDimension(dim[1], len(srcnc.variables[dim[1]][:]))
    dstnc.createDimension(dim[2], len(srcnc.variables[dim[2]][:]))

    # create variables
    dstnc.createVariable(dim[0], np.float64, (dim[0],))
    dstnc.createVariable(dim[1], np.float64, (dim[1],))
    dstnc.createVariable(dim[2], np.float64, (dim[2],))
    dstnc.createVariable(variable, np.float32, (dim[0], dim[1], dim[2]))
  
    # Copy variables attributes
    for name, var in dstnc.variables.items():
        dstnc.variables[name].setncatts({a:srcnc.variables[name].getncattr(a) 
                                        for a in srcnc.variables[name].ncattrs()})
    if copyvalue :
        # Copy variables' value
        dstnc.variables[dim[0]][:] = srcnc.variables[dim[0]][:]
        dstnc.variables[dim[1]][:] = srcnc.variables[dim[1]][:]
        dstnc.variables[dim[2]][:] = srcnc.variables[dim[2]][:]
        dstnc.variables[variable][:] = srcnc.variables[variable][:]





