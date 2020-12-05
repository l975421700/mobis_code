

import shapefile
china = shapefile.Reader('/Users/gao/OneDrive - whu.edu.cn/Research/Data/GISdata/ChinaBorder/ChinaBorder.shp')


import geopandas
china_plot = geopandas.read_file('/Users/gao/OneDrive - whu.edu.cn/Research/Data/GISdata/ChinaBorder/ChinaBorder.shp')


