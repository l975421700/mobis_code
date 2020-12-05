########function to do ibb analysis on monthly dataset

import pandas as pd
import numpy as np

def monibb(mondata1, mondata2, samplesize = 100, sampletimes = 100, movwin = 1):
    '''
    parameter
    --------
    mondata1 :  pd.dataframe containing the monthly independent variable
                each column represents one station
                or pd.Series containing one series data
                
    mondata2 :  pd.dataframe containing the monthly dependent variable
                each column represents one station
                
    samplesize:  size of sampled data
    sampletimes: times to sample
    movwin :  moving window to calculate running mean.

    Return
    --------
    monr :  r weight, np.3darray, shape, [nstation, timescale, totalyears - 2] 
    monsampledata : np.4darray, 
                shape, [nstation, timescale, (totalyears - 2)*sampletimes, 2] 
    monpredic : np.4darray, shape, [nstation, timescale, (totalyears - 2), 2] 
    moncorde : rsquare, np.3darray, shape, [nstation, timescale, 2] 
    moncorce : correlation coefficient, np.2darray, shape, [nstation, timescale]
    '''
    
    #store the r value
    monr = np.zeros([mondata2.shape[1], 12, int(mondata2.shape[0]/12)-1-movwin])
    
    #store the sampled data
    monsampledata = np.zeros([mondata2.shape[1], 12, 
                              int(mondata2.shape[0]/12-1-movwin) * sampletimes, 2])
    
    #store the predicted value
    monpredic = np.zeros([mondata2.shape[1], 12, 
                          int(mondata2.shape[0]/12-1-movwin), 2])
    
    #store coefficient of determination (validation score)
    moncorde = np.zeros([mondata2.shape[1], 12,2])

    #store correlation coefficient
    moncorce = np.zeros([mondata2.shape[1], 12])

    if isinstance(mondata1, pd.Series):
        data1 = pd.DataFrame(np.array(mondata1))
    
    for i in range(0, mondata2.shape[1]):
        if isinstance(mondata1, pd.DataFrame):
            data1 = pd.DataFrame(np.array(mondata1.iloc[:, i]))
        
        star, stasampledata, stapredic, stacorde, stacorce = staibb(
                pd.concat([data1, mondata2.iloc[:, i]], axis = 1), 
                12, samplesize, sampletimes, movwin)
        monr[i, :, :] = star
        monsampledata[i, :, :, :] = stasampledata
        monpredic[i, :, :, :] = stapredic
        moncorde[i, :, :] = stacorde
        moncorce[i, :] = stacorce
        print(str(i+1)+'/'+str(mondata2.shape[1]))
    return monr, monsampledata, monpredic, moncorde, moncorce
    
'''
monr, monsampledata, monpredic, moncorde, moncorce = monibb(
            Telind.iloc[0:480, 0], monPre, movwin = 10)

star, stasampledata, stapredic, stacorde, stacorce = staibb(
        pd.concat([pd.DataFrame(np.array(Telind.iloc[0:480, 0])), 
        monPre.iloc[:, 100]], axis = 1), movwin = 10)
monr[100, :, :]
star

moncorde[100, :, :]
stacorde
'''    




########function to do ibb analysis for whole dataset

import pandas as pd
import numpy as np

def resibb(dataset1, dataset2, timescale = 12, 
           samplesize = 100, sampletimes = 100, movwin = 1):
    '''
    parameter
    --------
    dataset1 :  pd.dataframe containing the independent variable
                the first three column is ['year', 'month', 'day']
                each rest column represents one station
                
    dataset2 :  pd.dataframe containing the dependent variable
                the first three column is ['year', 'month', 'day']
                each rest column represents one station
                
    timescale : timeinterval in a year, 12 represent 'month' scale
    samplesize:  size of sampled data
    sampletimes: times to sample

    Return
    --------
    resr :  r weight, np.3darray, shape, [nstation, timescale, totalyears - 2] 
    ressampledata : np.4darray, 
                shape, [nstation, timescale, (totalyears - 2)*sampletimes, 2] 
    respredic : np.4darray, shape, [nstation, timescale, (totalyears - 2), 2] 
    rescorde : rsquare, np.3darray, shape, [nstation, timescale, 2] 
    rescorce : correlation coefficient, np.2darray, shape, [nstation, timescale]
    '''
    #store the r value
    resr = np.zeros([dataset1.shape[1]-3, timescale, 
                     len(np.unique(dataset1.iloc[:,0]))-2])
    
    #store the sampled data
    ressampledata = np.zeros([dataset1.shape[1]-3, timescale, 
                    (len(np.unique(dataset1.iloc[:,0]))-2) * sampletimes, 2])
    #store the predicted value
    respredic = np.zeros([dataset1.shape[1]-3, timescale, 
                          len(np.unique(dataset1.iloc[:,0]))-2, 2])
    
    #store coefficient of determination (validation score)
    rescorde = np.zeros([dataset1.shape[1]-3, timescale, 2])
    
    #store correlation coefficient
    rescorce = np.zeros([dataset1.shape[1]-3, timescale])

    for i in range(0, dataset1.shape[1]-3):
        pairdata = pd.DataFrame(np.array(pd.concat([dataset1.iloc[:,i+3], 
                                            dataset2.iloc[:,i+3]], axis = 1)), 
                               index= [dataset1.iloc[:,0], dataset1.iloc[:,1]], 
                               columns = ['data1','data2'])
        pairdata.index.names = ['year', 'month']
        stadata = pairdata.mean(level = ['year', 'month'])
        star, stasampledata, stapredic, stacorde, stacorce = staibb(stadata, 
                        timescale, samplesize, sampletimes)
        resr[i, :, :] = star
        ressampledata[i, :, :, :] = stasampledata
        respredic[i, :, :, :] = stapredic
        rescorde[i, :, :] = stacorde
        rescorce[i, :] = stacorce
        print(str(i+1)+'/'+str(dataset1.shape[1]-3))
    return resr, ressampledata, respredic, rescorde, rescorce
    
'''
dataset1 = Tem524
dataset2 = Precip524
timescale = 12
samplesize = 100
sampletimes =100
pairdata = pd.DataFrame(np.array(pd.concat([dataset1.iloc[:,3], 
                                           dataset2.iloc[:,3]], axis = 1)), 
                       index= [dataset1.iloc[:,0], dataset1.iloc[:,1]], 
                       columns = ['data1','data2'])
pairdata.index.names = ['year', 'month']
stadata = pairdata.mean(level = ['year', 'month'])
star, stasampledata, stapredic, stacorde, stacorce = staibb(stadata, 
                        timescale, samplesize, sampletimes)
'''    
    
    
    
########function to do ibb for one station

import numpy as np

def staibb(stadata, timescale = 12, 
           samplesize = 100, sampletimes = 100, movwin = 1):
    '''
    parameter
    --------
    stadata : pd.dataframe, ['Independent variable','dependent variable']
              two index, ['year', 'month']
    timescale : timeinterval in a year, 12 represent 'month' scale
    samplesize:  size of sampled data
    sampletimes: times to sample
    movwin :  moving window to calculate running mean.

    Return
    --------
    star :  r weight, np.2darray, shape, [timescale, totalyears - 2] 
    stasampledata : np.3darray, shape, [timescale, (totalyears - 2)*sampletimes, 2] 
    stapredic : np.3darray, shape, [timescale, (totalyears - 2), 2] 
    stacorde : rsquare, np.2darray, shape, [timescale, 2] 
    stacorce : correlation coefficient, np.1darray, shape, (timescale)
    '''
    #store the r value for each station
    star = np.zeros([timescale, int(stadata.shape[0]/timescale)-1-movwin])
    
    #store the sampled data
    stasampledata = np.zeros([timescale, 
                              int(stadata.shape[0]/timescale-1-movwin) * sampletimes, 2])
    #store the predicted value
    stapredic = np.zeros([timescale, int(stadata.shape[0]/timescale-1-movwin), 2])
    
    #store coefficient of determination (validation score)
    stacorde = np.zeros([timescale,2])
    
    #store correlation coefficient
    stacorce = np.zeros(timescale)
    
    for i in range(0, timescale):
        prdr, prdsampledata, prdpredic, prdcorde, corce = prdibb(
                stadata.iloc[np.arange(0,stadata.shape[0]/timescale)*12 + i, :], 
                samplesize, sampletimes, movwin)
        star[i, :] = prdr
        stasampledata[i, :, :] = prdsampledata
        stapredic[i, :, :] = prdpredic
        stacorde[i, :] = prdcorde
        stacorce[i] = corce
    
    return star, stasampledata, stapredic, stacorde, stacorce

'''
#example: 
import pandas as pd
stadata = pd.read_csv('/Users/gao/OneDrive - business/Research/Study in Korea/Data/for_check/stadata.TXT',sep='\s+')
stadata = stadata.set_index(['year','month'])
star, stasampledata, stapredic, stacorde, stacorce = staibb(stadata, movwin = 10)

#calculate the single result
prddata = stadata.loc(axis=0)[:,12]
prdr, prdsampledata, prdpredic, prdcorde, corce = prdibb(prddata, movwin = 10)
prdr
star[11, :]
'''




########function to do ibb for one period

import numpy as np

def prdibb(prddata, samplesize = 100, sampletimes = 100, movwin = 1):
    '''
    parameter
    --------
    prddata :   pd.dataframe, ['Independent variable','dependent variable']
                data for one month or one season.
    samplesize:  size of sampled data
    sampletimes: times to sample
    movwin :  moving window to calculate running mean.
    
    Return
    --------
    prdr :  r weight, np.1darray, shape, (totalyears - 2) 
    prdsampledata : np.3darray, shape, [(totalyears - 2)*sampletimes, 2] 
    prdpredic : np.2darray, shape, [(totalyears - 2), 2] 
    prdcorde : rsquare, np.1darray, shape, (2) 
    corce : correlation coefficient
    '''
    
    #replace the NaN value using the mean
    prddata.iloc[np.where(np.isnan(prddata.iloc[:, 0]))[0], 0] = \
        np.mean(prddata.iloc[:, 0])
    
    prddata.iloc[np.where(np.isnan(prddata.iloc[:, 1]))[0], 1] = \
        np.mean(prddata.iloc[:, 1])
    
    #use moving window to group the data
    prddata1 = runmean(prddata, movwin)
    
    #store r for each period 
    #(not include the periods with highest and lowest Independent variable)
    prdr = np.zeros(prddata1.shape[0]-2)
    
    #store the sampled data
    prdsampledata = np.zeros([sampletimes*(prddata1.shape[0]-2), 2])
    
    #store the predicted value
    prdpredic = np.zeros([prddata1.shape[0]-2, 2])
    
    #correlation coefficient
    corce = prddata1.corr().iloc[0,1]
    
    prddata3 = np.array(prddata1.iloc[np.argsort(prddata1.iloc[:, 0]), :])
    
    for i in range(1, prddata3.shape[0]-1):
        resx, sampledata = ibb(np.delete(prddata3, i, 0), 
            prddata3[i,0]-np.mean(np.delete(prddata3, i, 0)[:,0]), 
            samplesize, sampletimes)
        prdr[i-1] = resx
        prdsampledata[(-100+i*100) : (i*100),:] = sampledata
        prdpredic[i-1,:] = np.mean(sampledata, axis = 0)
        
    #store coefficient of determination (validation score) for two variables
    if movwin > 1 :
        prddata2 = runmean(prddata.iloc[0:-1, :], movwin - 1)
        prddata4 = np.array(prddata2.iloc[np.argsort(prddata1.iloc[:, 0]), :])
        
        prdcorde = np.array([(np.corrcoef(
                prddata3[1:(-1),0] * movwin - prddata4[1:(-1),0] * (movwin - 1), 
                prdpredic[:,0] * movwin - prddata4[1:(-1),0] * (movwin - 1)
                )[0,1])**2, 
                (np.corrcoef(
                prddata3[1:(-1),1] * movwin - prddata4[1:(-1),1] * (movwin - 1), 
                prdpredic[:,1] * movwin - prddata4[1:(-1),1] * (movwin - 1)
                )[0,1])**2])
    
    else :
        prdcorde = np.array([(np.corrcoef(prddata3[1:(-1),0], prdpredic[:,0])[0,1])**2, 
                         (np.corrcoef(prddata3[1:(-1),1], prdpredic[:,1])[0,1])**2])
    
    return prdr, prdsampledata, prdpredic, prdcorde, corce

'''
import pandas as pd
groupdata = pd.read_csv('/Users/gao/OneDrive - business/Research/Study in Korea/Data/for_check/groupdata.TXT', 
                        sep='\s+', header='infer')
prddata = groupdata.iloc[np.where(groupdata.iloc[:,1]==1)[0],2:4]

#example1: 
prdr, prdsampledata, prdpredic, prdcorde, corce = prdibb(prddata)

prddata1 = np.array(prddata.iloc[np.argsort(prddata.iloc[:, 0]), :])
resx, sampledata = ibb(np.delete(prddata1,1,0), 
    datachange = prddata1[1,0]-np.mean(np.delete(prddata1,1,0)[:,0]), 
    samplesize = 100, sampletimes = 100)
resx == prdr[0]


#example2: 
prdr1, prdsampledata1, prdpredic1, prdcorde1, corce1 = prdibb(prddata, movwin = 10)
samplesize = 100
sampletimes = 100
movwin = 10
'''





########function to do ibb analysis

from scipy.optimize import minimize
import numpy as np
from numpy.random import choice

def ibb(obsdata, datachange, samplesize = 100, sampletimes = 100):
    '''
    parameter
    --------
    obsdata :    np.array, the observed data pair containing two columns, 
                 ['Independent variable','dependent variable']
    datachange:  the change of Independent variable
    samplesize:  size of sampled data
    sampletimes: times to sample
    
    Return
    --------
    res.x :  r weight
    sampledata : np.2darray, shape, [sampletimes,2] 
    '''
    #arrange the observed data based on independent variable
    obsdata = obsdata[np.argsort(obsdata[:, 0]), :]

    sampledata = np.zeros([sampletimes,2])
    
    mat=np.zeros([obsdata.shape[0],4])
    mat[:,0] = np.arange(1, mat.shape[0]+1, 1, dtype=np.int32)
    mat[:,1] = mat[:,0]/mat.shape[0]

    fun = lambda r: (sum(mat[:,1]**r * obsdata[:,0]) / sum(mat[:,1]**r)
                      - obsdata[:,0].mean()  - datachange)**2
    res = minimize(fun, x0 = 0, method = 'TNC', bounds = ((-10,10),))
    mat[:,2] = mat[:,1]**res.x
    mat[:,3] = mat[:,2]/sum(mat[:,2])
    
    for i in range(sampletimes):
        sampledata[i,:] = np.mean(obsdata[choice(mat.shape[0], 
                                  size = samplesize, p = mat[:,3]),:], axis=0)
    return res.x, sampledata

'''
#example: 
import pandas as pd
tldata = pd.read_csv('/Users/gao/OneDrive - business/Research/Study in Korea/Data/for_check/tldata.TXT', 
                     sep='\s+',header=None)
resx, sampledata = ibb(np.array(tldata), 0.5)
'''




#function to calculate running mean of data

import numpy as np

def runmean(data, movwin):
    '''
    parameter
    --------
    data :   pd.dataframe, each column for one series of data
    movwin : moving window to calculate running mean.
    
    Return
    --------
    res : running mean
    
    '''
    res = np.zeros([data.shape[0] - movwin + 1, 2])
    for i in range(movwin):
        res += np.array(data.iloc[i:(i + data.shape[0] - movwin + 1), :])
    return pd.DataFrame(res/movwin)
    
    
    
'''
#example: 
import pandas as pd
groupdata = pd.read_csv('/Users/gao/OneDrive - business/Research/Study in Korea/Data/for_check/groupdata.TXT', 
                        sep='\s+', header='infer')
prddata = groupdata.iloc[np.where(groupdata.iloc[:,1]==1)[0],2:4]

res = runmean(prddata, 10)
np.mean(prddata.iloc[30:40, 1]) == res.iloc[30, 1]
'''
    
    
    
    








