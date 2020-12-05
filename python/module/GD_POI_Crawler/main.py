from search_grid import *
from URLBuilder import URLBuilder
from GRID import Grid
import os
import time


dic_type = {
    # "010000": "car_service",
    # "020000": "car_sale",
    # "030000": "car_repaire",
    # "040000": "moto_service",
    # "050000": "restaurant",
    # "060000": "shopping",
    # "070000": "lifeservice",
    # "080000": "sports",
    # "090000": "medical",
    # "100000": "hotel",
    # "110000": "view",
    # "120000": "building",
    # "130000": "goverment",
    # "140000": "education",
    # "150000": "transport",
    # "160000": "finance",
    # "170000": "company",
    # "180000": "roadfacility",
    # "190000": "placename",
    # "200000": "publicefacility"

    "050100": "ChineseFood"
}

def main():
    """
    """
    import sys
    try:
        os.chdir(os.path.dirname(__file__))
    except:
        os.chdir(".")
    # Shanghai
    city= "shanghai"
    lby = 30.7
    lbx = 120.9
    rty = 31.499
    rtx = 121.9

    # city = "beijing"
    # lby = 39.6
    # lbx = 115.9
    # rty = 40.3
    # rtx = 116.9


    params = dict()
    params['offset'] = 20
    params['types'] = "150000"
    params['page'] = 1
    params['extensions'] = "all"
    params['keywords'] = ""

    grid = Grid(lbx, lby, rtx, rty)

    pois = []

    for key in dic_type.keys():        
        outputfilename = "%s_%s_%s.json" % (city, key, dic_type[key])
        if os.path.exists(outputfilename):
            continue
        params['types'] = key
        url_builder = URLBuilder("https://lbs.amap.com/dev/api?", params)
        loop_search(url_builder = url_builder, params = params, grid = grid, NumStep = 50, pois = pois, outputfilename = outputfilename, layer = 1)
        print("sleeping for 600 s")
        time.sleep(60 * 10)
   

if __name__ == '__main__':
    
    # os.chdir(r"E:\Test\GaoDePOI")
    main()