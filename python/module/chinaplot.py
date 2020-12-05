########Function to plot shapefile


import numpy as np
import matplotlib.pyplot as plt
from math import ceil

def chinaplot(shpfile, title = False, figsize = (7.5,4.5), 
            color = 'k', linewidth = 0.01, singleplot = True): 
    '''
    parameter
    --------
    shpfile : shpfile readed by shapefile.Reader
    title : title of map
    figsize : size of figure
    color : color of map border
    linewidth : width of line
    singleplot : bool, if a single map of multiple map will be plotted
    '''
    
    #plot frame of map
    if singleplot:
        plt.figure(figsize = figsize)
        ax1 = plt.axes([0.08,0.07,0.91,0.91])
    plt.axis(np.array([72.5, 135.5, 16, 54.5]))
    if title :
        plt.title(title,fontsize=24)
    plt.xticks(np.array([80, 90, 100, 110, 120, 130]), 
               [80, 90, 100, 110, 120, '130 °E'],fontsize=14)
    plt.yticks(np.array([20, 25, 30, 35, 40, 45, 50]), 
               [20, 25, 30, 35, 40, 45, '50°N'],fontsize=14)
    
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
def chinacontour(lon, lat, z, title = False, bounds = 0, 
                steps = 200, cmap = 'RdBu_r'):#RdBu
    '''
    parameter
    --------
    lon : np.1darray, containing longitude information
    
    lat : np.1darray, containing latitude information
    
    z : np.2darray, containing values at each grid
    
    title : title of the map
    
    bounds : breaks of the color, default is 0, the breaks will be calculated
    
    steps : if bounds == 0, the steps of the breaks of legend
    '''
    if isinstance(bounds, int):
        bounds = np.linspace(np.min(z)//steps*steps, ceil(np.max(z)/steps)*steps, 
                 (ceil(np.max(z)/steps)*steps - np.min(z)//steps*steps)/steps+1)

    X, Y = np.meshgrid(lon, lat)
    plt.contourf(X, Y, z, levels = bounds, cmap = cmap)
    
    cbar = plt.colorbar(cax = plt.axes([0.87, 0.12, 0.02, 0.4]),
                        boundaries = bounds, ticks = bounds)
    cbar.ax.tick_params(labelsize = 14)
    
    if title :
        plt.title(title, fontsize=24)

'''
lon = x
lat = y
z = z
title = False
fraction = 0.1
cmap = 'RdBu_r'
bounds = np.array([0, 200, 400, 800, 1200, 1600, 2500])

'''



