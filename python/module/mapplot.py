########Function to plot shapefile


import numpy as np
import matplotlib.pyplot as plt
from math import ceil

def shpplot(shpfile, title = 'Map of China', figsize = (8,4.5), 
            xtickspace = 10 , ytickspace =5, 
            color = 'k', linewidth = 0.1, singleplot = True): 
    '''
    parameter
    --------
    shpfile : shpfile readed by shapefile.Reader
    title : title of map
    figsize : size of figure
    xtickspace / ytickspace : space of x / y ticks
    color : color of map border
    linewidth : width of line
    singleplot : bool, if a single map of multiple map will be plotted
    '''
    
    bbox = np.array([shpfile.bbox[0]//5*5, ceil(shpfile.bbox[2]/5 - 0.001)*5, 
            shpfile.bbox[1]//5*5, ceil(shpfile.bbox[3]/5)*5],dtype = 'int32')
    #maximum latitude in shapefile of world is only 83.6, here increased to 90
    if bbox[3]==85:
        bbox[3]=90
    #Give the west longitude and south latitude name
    xticks1 = np.arange(bbox[0],bbox[1],xtickspace)
    if bbox[0] < 0:
        xticks = [str(abs(bbox[0]))+'°W'] + \
            list(abs(np.arange(bbox[0]+xtickspace,bbox[1],xtickspace))) + \
            [str(bbox[1])+'°E']
    else:
        xticks = list(np.arange(bbox[0],bbox[1],xtickspace))+[str(bbox[1])+'°E']  
    if bbox[2] < 0:
        yticks = [str(abs(bbox[2]))+'°S'] + \
            list(abs(np.arange(bbox[2]+ytickspace,bbox[3],ytickspace))) + \
            [str(bbox[3])+'°N']
    else:
        yticks = list(np.arange(bbox[2],bbox[3],ytickspace))+[str(bbox[3])+'°N']    
    
    #plot frame of map
    if singleplot:
        plt.figure(figsize = figsize)
        ax1 = plt.axes([0.05,0.05,0.9,0.9])
    plt.axis(bbox)
    if title :
        plt.title(title,fontsize=24)
    plt.xticks(np.arange(bbox[0],bbox[1]+xtickspace,xtickspace),xticks,fontsize=14)
    plt.yticks(np.arange(bbox[2],bbox[3]+ytickspace,ytickspace),yticks,fontsize=14)
    
    #plot shapefile
    for shape in list(shpfile.iterShapes()):
        npoints = len(shape.points) # total points
        nparts = len(shape.parts) # total parts
        
        if nparts == 1:
            x_lon = np.zeros((len(shape.points),1))
            y_lat = np.zeros((len(shape.points),1))
            for ip in range(len(shape.points)):
                x_lon[ip] = shape.points[ip][0]
                y_lat[ip] = shape.points[ip][1]
            plt.plot(x_lon,y_lat,color,linewidth = linewidth)
    
        else:   # loop over parts of each shape, plot separately
            for ip in range(nparts):
                i0 = shape.parts[ip]
                if ip < nparts-1:
                    i1 = shape.parts[ip+1]-1
                else:
                    i1 = npoints
                seg = shape.points[i0:i1+1]
                x_lon = np.zeros((len(seg),1))
                y_lat = np.zeros((len(seg),1))
                for ip in range(len(seg)):
                    x_lon[ip] = seg[ip][0]
                    y_lat[ip] = seg[ip][1]
                plt.plot(x_lon,y_lat,color,linewidth = linewidth)



import numpy as np
import matplotlib.pyplot as plt
from math import ceil
from matplotlib import colors
########Function to plot 2-D contour (2/3)
def contourplot(lon, lat, z, title, bounds = 0, fraction = 0.06,
                steps = 200, cmap = 'RdBu_r'):#RdBu
    '''
    parameter
    --------
    lon : np.1darray, containing longitude information
    lat : np.1darray, containing latitude information
    z : np.2darray, containing values at each grid
    title : title of the map
    bounds : breaks of the legend, default is 0, the breaks will be calculated
    steps : if bounds == 0, the steps of the breaks of legend
    '''
    if isinstance(bounds, int):
        bounds = np.linspace(np.min(z)//steps*steps, ceil(np.max(z)/steps)*steps, 
                 (ceil(np.max(z)/steps)*steps - np.min(z)//steps*steps)/steps+1)
    X, Y = np.meshgrid(lon, lat)
    plt.contourf(X, Y, z, levels = bounds, cmap = cmap)
    cbar = plt.colorbar(fraction = fraction, 
                        boundaries = bounds, ticks = bounds)
    cbar.ax.tick_params(labelsize = 14)
    if title :
        plt.title(title, fontsize=24)


'''
lon = gridx
lat = gridy
z = krZ
steps = 200
unit = 'mm'
cmap = 'RdBu_r'
'''
########Function to build a mask for krige interpolation (3.1/3)

import numpy as np
import rasterio as rio
import rasterio.mask


def mask_maker(maskshp,resolution):
    '''
    parameter
    --------
    maskshp : polygon shapefile that defines the range of interpolation region
              shapefile should be returned by function: fiona.open
    
    resolution : The intended interpolation resolution
    
    Return
    --------
    out_image[0,:,:] : np.2darray, defines the region to be interpolated
    out_transform : np.1darray, defines the bounds of interpolation region
    '''
    gridx = np.arange(maskshp.bounds[0]//5*5, (maskshp.bounds[2]//5+1)*5, resolution)
    gridy = np.arange(maskshp.bounds[1]//5*5, (maskshp.bounds[3]//5+1)*5, resolution)
    z = np.zeros((len(gridy), len(gridx)), dtype=float)
    
    with rio.open('maskraster.tif', mode='w', driver='GTiff',width=len(gridx), 
                  height=len(gridy), count=1, crs={'init': 'epsg:4326'}, 
                  transform=(resolution, 0, gridx[0],0, resolution, gridy[0]), 
                  dtype='float64') as dst:
         dst.write(z, 1)

    features = [feature["geometry"] for feature in maskshp]
    
    with rio.open('maskraster.tif','r') as mr:
        out_image, out_transform = rasterio.mask.mask(mr, 
                                   features, crop=True, nodata=np.nan)
    return out_image[0,:,:], out_transform




########Function to do krige interpolation (3.2/3)

import numpy as np
from pykrige.ok import OrdinaryKriging   

def krigeint(dataset, maskshp, resolution, title = 'Map of China', 
             plotresult = True, bounds = 0, steps = 200):
    '''
    parameter
    --------
    dataset : pd.dataframe, three columns: [longitude, latitude, record data]
    
    maskshp : polygon shapefile returned by function: fiona.open
              defines the range of interpolation region
    
    resolution : Intended interpolation resolution 
    
    title : title of map
    
    bounds : breaks of the legend, default is 0, the breaks will be calculated
    
    steps : if bounds == 0, the steps of the breaks of legend
    
    plotresult : if true, the result will be plotted
    
    Return
    --------
    gridx : np.1darray, containing longitude information
    gridy : np.1darray, containing latitude information
    krZ : np.2darray, containing values at each grid

    '''
    dataset = dataset.dropna()
    out_image, out_transform = mask_maker(maskshp, resolution)
    x = np.arange(out_transform[2], 
            out_transform[2]+out_image.shape[1]*resolution-0.0001, resolution)
    y = np.arange(out_transform[5], 
            out_transform[5]+out_image.shape[0]*resolution-0.0001, resolution)
    okresult = OrdinaryKriging(dataset.iloc[:,0], dataset.iloc[:,1], dataset.iloc[:,2],
                              variogram_model = 'spherical')
    z, sigmasq = okresult.execute('masked', x, y, mask=np.isnan(out_image))
    if plotresult:
        contourplot(lon = x, lat = y, z = z, title = title, 
                    bounds = bounds, steps = steps)
    return x, y, z

'''
dataset = seammk.iloc[:, [0,1,2]]
maskshp = ChinanoTai
resolution = 0.5

'''



import pandas as pd
def multiplot(lon_lat, data1, data2, data3, month, shpfile, maskshp, titles, 
              resolution = 0.25, figsize = (24,18), xtickspace = 10 , 
              ytickspace = 5, bounds = 0, steps = 200):
    '''
    parameter
    --------
    lon_lat : pd.dataframe, ['longitude', 'latitude']
    
    data1 : nd.array, correlation coefficient
    
    data2 : nd.array, coefficient of determination, 
            based on prediction of independent varaible
            
    data3 : nd.array, coefficient of determination, 
            based on prediction of dependent varaible
            
    month : one of 12 months, eg. 'Dec'
    
    bounds : breaks of the legend, default is 0, the breaks will be calculated
    
    steps : if bounds == 0, the steps of the breaks of legend
    '''
    plt.figure(figsize = figsize)
    plt.subplot(2, 2, 1)
    shpplot(shpfile, xtickspace = xtickspace, ytickspace = ytickspace, singleplot = False)
    gridx, gridy, krZ = krigeint(pd.concat([lon_lat, pd.Series(data1)], axis = 1, ignore_index=True), maskshp, resolution, 
                                 title = titles[0] + month, bounds = bounds, steps = 0.001)
    
    plt.subplot(2, 2, 2)
    shpplot(shpfile, xtickspace = xtickspace, ytickspace = ytickspace, singleplot = False)
    gridx, gridy, krZ = krigeint(pd.concat([lon_lat, pd.Series(data2)], axis = 1, ignore_index=True), maskshp, resolution, 
                                 title = titles[1] + month, bounds = bounds, steps = 0.05)
    
    plt.subplot(2, 2, 3)
    shpplot(shpfile, xtickspace = xtickspace, ytickspace = ytickspace, singleplot = False)
    gridx, gridy, krZ = krigeint(pd.concat([lon_lat, pd.Series(data3)], axis = 1, ignore_index=True), maskshp, resolution, 
                                 title = titles[2] + month, bounds = bounds, steps = 0.1)







