
import urllib
import json
import requests
import time


def downUrlInfo(url):
    """
    """
    header = {}
    header['authority'] = 'lbs.amap.com'
    header['method'] = "POST"
    header['scheme'] = 'https'

    header['accept'] = 'application/json, text/javascript, */*; q=0.01'
    header['accept-language'] = 'en-US,en;q=0.9,zh-CN;q=0.8,zh;q=0.7'
    header['origin'] = 'https://lbs.amap.com'
    header['referer'] = 'https://lbs.amap.com/api/webservice/guide/api/search'
    
    header['User-Agent'] = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.142 Safari/537.36"
    header['x-requested-with'] = 'XMLHttpRequest'
    header['path'] = url.replace("https://lbs.amap.com", "")
    # print("header path", header['path'])
    
    payload = {"type": "place/polygon", "version": "v3"}

    while 1:
        try:
            req = requests.post(url, data = payload, timeout = 10, headers = header)
            break
        except:
            time.sleep(10)
            pass
    
    return req.text

class URLBuilder:
    """
    url construct
    """
    def __init__(self, url, params):
        self.params = params
        self.url = url
    
    def do_request(self, getUrlInfo = downUrlInfo):
        """
        """
        url = self.url + urllib.parse.urlencode(self.params)

        try:
            doc = getUrlInfo(url)

            json_doc = json.loads(doc)
            
        except requests.exceptions.Timeout:
            json_doc = {"pois": []}
            pass
        
        return json_doc
    
    def getUrl(self):
        """
        """
        return self.url + urllib.parse.urlencode(self.params)
    
    def setParams(self, params):
        """
        """
        self.params = params
        

def output_data(json_doc, outfilename):
    """
    """
    with open(outfilename, 'a', encoding='utf-8') as fp:
        for poi in json_doc['pois']:
            fp.write(json.dumps(poi) + "\n")
