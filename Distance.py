
#%%
from math import*
import pandas as pd
import numpy as np
import time
def Distance(coordinate1,coordinate2):
    lat1, lng1 = map(float, coordinate1.split(", "))
    lat2, lng2 = map(float, coordinate2.split(", "))

    radlat1=radians(lat1)  
    radlat2=radians(lat2)  
    a=radlat1-radlat2  
    b=radians(lng1)-radians(lng2)  
    s = 2*asin(sqrt(pow(sin(a/2),2)+cos(radlat1)*cos(radlat2)*pow(sin(b/2),2)))  
    earth_radius=6378.137  
    s = s*earth_radius  
    if s<0:  
        return -s  
    else:  
        return s

def MinDisToInterchange(House, InterchangeList):

    dis = list([0] * len(InterchangeList))

    for i in range(0, len(InterchangeList),1):
        dis[i] = float(Distance(House, InterchangeList['Coordinate'][i]))
    return str(min(dis))

#%%
Distance("25.0661299, 121.2180622", "25.0587819, 121.2132735")

#%%
Taoyuan_InterchangeList = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/Taoyuan_InterchangeList.csv', error_bad_lines = False)
HousingPrice_Taoyuan_S = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/HousingPrice_Taoyuan_S.csv', error_bad_lines = False)

for i in range(0, len(HousingPrice_Taoyuan_S), 1):
    HousingPrice_Taoyuan_S['DistanceToIC'][i] = (MinDisToInterchange(HousingPrice_Taoyuan_S['Coordinate'][i], Taoyuan_InterchangeList))

# %%
HousingPrice_Taoyuan_S.to_csv('HousingPrice_Taoyuan_S.csv', index = False, encoding='utf-8')

# %%
### Yunlin

Yunlin_IC = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/Yunlin_IC.csv', error_bad_lines = False)
HousingPrice_Yunlin_S = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/HousingPrice_Yunlin_S.csv', error_bad_lines = False)

for i in range(0, len(HousingPrice_Yunlin_S), 1):
    HousingPrice_Yunlin_S['DistanceToIC'][i] = (MinDisToInterchange(HousingPrice_Yunlin_S['Coordinate'][i], Yunlin_IC))

# %%
HousingPrice_Yunlin_S.to_csv('HousingPrice_Yunlin_S.csv', index = False, encoding='utf-8')