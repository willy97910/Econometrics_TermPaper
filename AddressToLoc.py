#%%
# pip install selenium
# pip install beautifulsoup4

### Geocoding ### 
# Reference: https://medium.com/%E8%8A%B1%E5%93%A5%E7%9A%84%E5%A5%87%E5%B9%BB%E6%97%85%E7%A8%8B/geocoding-%E6%89%B9%E9%87%8F%E8%99%95%E7%90%86%E5%9C%B0%E5%9D%80%E8%BD%89%E6%8F%9B%E7%B6%93%E7%B7%AF%E5%BA%A6-721ab2564c88

# %%
import requests
import time
from selenium import webdriver
from selenium.webdriver.support.ui import Select
from bs4 import BeautifulSoup
options = webdriver.ChromeOptions()
options.add_argument("headless")


def get_coordinate(addr):
    browser = webdriver.Chrome(options = options)
    browser.get("http://www.map.com.tw/")
    search = browser.find_element_by_id("searchWord")
    search.clear()
    search.send_keys(addr)
    browser.find_element_by_xpath("/html/body/form/div[10]/div[2]/img[2]").click() 
    time.sleep(2)
    iframe = browser.find_elements_by_tag_name("iframe")[1]
    browser.switch_to.frame(iframe)
    coor_btn = browser.find_element_by_xpath("/html/body/form/div[4]/table/tbody/tr[3]/td/table/tbody/tr/td[2]")
    coor_btn.click()
    coor = browser.find_element_by_xpath("/html/body/form/div[5]/table/tbody/tr[2]/td")
    coor = coor.text.strip().split(" ")
    lat = coor[-1].split("：")[-1]
    log = coor[0].split("：")[-1]
    browser.quit()
    return (lat, log)

#%%
# get_coordinate("台北市大安區羅斯福路四段1號")
# get_coordinate("林口交流道")

#%%
import pandas as pd
import time
#%%
# HousingPrice_B = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/HousingPrice_B.csv', error_bad_lines = False)
# HousingPrice_Taoyuan = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/HousingPrice_Taoyuan.csv', error_bad_lines = False)
# HousingPrice_Taoyuan = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/HousingPrice_Taoyuan.csv', error_bad_lines = False)
# HousingPrice_Taoyuan_S = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/HousingPrice_Taoyuan_S.csv', error_bad_lines = False)
HousingPrice_Yunlin_S = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/HousingPrice_Yunlin_S.csv', error_bad_lines = False)

# %%
# HousingPrice_Taoyuan['Coordinate'] = "0"
'''
for i in range(7261, len(HousingPrice_Taoyuan)):
    print(i, time.time())
    HousingPrice_Taoyuan['Coordinate'][i] = get_coordinate(HousingPrice_Taoyuan['Location'][i])
'''
# %%
# HousingPrice_Taoyuan.to_csv('file_name.csv', index = False, encoding='utf-8')

# %%
# ---- Taoyuan_Sample: getting location coordinate ---- #
# HousingPrice_Taoyuan_S['Coordinate'] = "0"
for i in range(362, len(HousingPrice_Taoyuan_S)):
    print(i, time.time())
    HousingPrice_Taoyuan_S['Coordinate'][i] = get_coordinate(HousingPrice_Taoyuan_S['Location'][i])

# %%
HousingPrice_Taoyuan_S.to_csv('HousingPrice_Taoyuan_S.csv', index = False, encoding='utf-8')


# %%
# ---- Yunlin_Sample: getting location coordinate ---- #
# HousingPrice_Yunlin_S['Coordinate'] = "0"
for i in range(474, len(HousingPrice_Yunlin_S)):
    print(i, time.time())
    HousingPrice_Yunlin_S['Coordinate'][i] = get_coordinate(HousingPrice_Yunlin_S['Location'][i])

# %%
HousingPrice_Yunlin_S.to_csv('HousingPrice_Yunlin_S.csv', index = False, encoding='utf-8')


# %%
Taoyuan_InterchangeList = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/Taoyuan_InterchangeList.csv', error_bad_lines = False)
for i in range(0, len(Taoyuan_InterchangeList)):
    print(i, time.time())
    Taoyuan_InterchangeList['Coordinate'][i] = get_coordinate(Taoyuan_InterchangeList['Name'][i])

Taoyuan_InterchangeList.to_csv('Taoyuan_InterchangeList.csv', index = False, encoding='utf-8')

# %%
Yunlin_IC = pd.read_csv(r'C:/Users/willy/OneDrive/Documents/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet/Yunlin_IC.csv', error_bad_lines = False)
for i in range(0, len(Yunlin_IC)):
    print(i, time.time())
    Yunlin_IC['Coordinate'][i] = get_coordinate(Yunlin_IC['Name'][i])

Yunlin_IC.to_csv('Yunlin_IC.csv', index = False, encoding='utf-8')


# %%
