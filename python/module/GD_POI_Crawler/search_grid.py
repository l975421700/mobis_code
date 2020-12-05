# 
# 
# 

from URLBuilder import *

def loop_search(url_builder = None, params = None, grid = None, NumStep = 10, pois = [], layer = 1, outputfilename = None):
    """
    """
    grid.set_steps(NumStep, NumStep)

    for i in range(NumStep):
        for j in range(NumStep):
            params['page'] = 1
            params['polygon'] = grid.get_str_bound(i, j)

            url_builder.setParams(params)

            json_doc = url_builder.do_request()

            if "count" not in json_doc.keys():
                continue           

            # The number of POI
            if int(json_doc['count']) >= 0:
                total = int(json_doc['count'])
            else:
                continue
            
            if total < 800:
                # max page is 18
                max_page = 41
                for page_num in range(1, max_page):
                    # print(page_num)
                    params['page'] = page_num
                    url_builder.setParams(params)
                    json_doc = url_builder.do_request()

                    if "count" not in json_doc.keys():
                        continue

                    if int(json_doc['count']) == 0:
                        continue

                    output_data(json_doc, outputfilename)
                    print(url_builder.getUrl())
                    print("layer: %d, row: %d, col: %d, page: %d, poi_count: %d" % (layer, i, j, page_num, total))

            else:
                secondBound = grid.get_bound(i, j)
                # print("layer: %s, row: %d, col: %d" % (layer, i, j))
                if layer >= 5:
                    break
                loop_search(url_builder = url_builder, params = params, grid= secondBound, NumStep = 10, pois = pois, outputfilename = outputfilename, layer = layer + 1)
