##################Function to find complete data in the record value

###data : a list of record dataframe

###nstation

import numpy as np
import pandas as pd


def lastna(x):
    try:
        a = np.where(np.array(x) == True)[-1][-1]
    except IndexError:
        a = 0
    return a

def napercent(data):
    nanvalues = pd.DataFrame(np.zeros((len(data)*2, data[0].shape[1])),dtype = np.int64)
    
    for i in range(0,len(data)):
        nanvalues.iloc[i,:] = np.array((data[i].isnull()).apply(lastna),dtype = np.int64)
        print(str(i+1)+'/'+str(len(data)))
        
    for i in range(0,len(data)-1):
        nanvalues.iloc[i+len(data),:] = nanvalues.iloc[[0,i+1],:].apply(max)
    
    nanvalues.iloc[2*len(data)-1,:] = nanvalues.iloc[0:4,:].apply(max)
    return nanvalues
	
	
	







