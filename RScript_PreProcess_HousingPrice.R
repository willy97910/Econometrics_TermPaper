system("defaults write org.R-project.R force.LANG zh_TW.UTF-8")

setwd("~/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet")

#------------------------------------------------------------------------------#
# B05704007 國企四 賴昱瑋
# Econometrics II 108-2
# 
#------------------------------------------------------------------------------#

#--- Import Library -----------------------------------------------------------#
# library(haven)
library(readr)
library(ggplot2)
library(plm)
library(lmtest)
library(dplyr)
library(stringr)
library(lubridate)
library(hash)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# for (i in c(0:32)) {
#   for (j in c(utf8ToInt("a"):utf8ToInt("z"))) {
#     if(i == 0 & j == as.numeric(charToRaw("a"))){
#       HousingPrice <- read_csv(paste0("HousingPrice/lvr_landcsv (", i,")/", intToUtf8(j), "_lvr_land_a.csv"), na = "NA")
#     }
#     else{
#       if(file.exists(paste0("HousingPrice/lvr_landcsv (", i,")/", intToUtf8(j), "_lvr_land_a.csv")) == FALSE)
#         next
#       HousingPrice <- rbind(HousingPrice,
#                             read_csv(paste0("HousingPrice/lvr_landcsv (", i,")/", intToUtf8(j), "_lvr_land_a.csv"),
#                                      na = "NA"))
#     }
#   }
# }
# 
# colnames(HousingPrice) <- c("TOWN", "TransactionSign", "Address", "ShiftingArea", "MetroZoneCheck", "NonMetroZoneCheck", "NonMetroCode", "Date", "TransactionCnt", "TransFloor", "TotalBuildingFloor", "BuildingType", "MainUse", "MainMaterial", "BuiltYear", "TransArea", "PresentRoomCnt", "PresentHallCnt", "PresentBathCnt", "PresentPartition", "ManagementOrNot", "TotalPrice", "UnitPrice", "ParkType", "ParkArea", "ParkTotalPrice", "Note", "SerialNumber")
# # 鄉鎮市區,交易標的,土地區段位置建物區段門牌,土地移轉總面積平方公尺,都市土地使用分區,非都市土地使用分區,非都市土地使用編定,交易年月日,交易筆棟數,移轉層次,總樓層數,建物型態,主要用途,主要建材,建築完成年月,建物移轉總面積平方公尺,建物現況格局-房,建物現況格局-廳,建物現況格局-衛,建物現況格局-隔間,有無管理組織,總價元,單價元平方公尺,車位類別,車位移轉總面積平方公尺,車位總價元,備註,編號
# HousingPrice <- subset(HousingPrice, TransactionSign != "transaction sign")
# # con <-file('HousingPrice.csv', encoding = "UTF-8")
# # write.csv(HousingPrice, file=con, row.names = FALSE)
# write_csv(HousingPrice, 'HousingPrice0526.csv')

HousingPrice <- read_csv('HousingPrice.csv', locale = locale(encoding = 'UTF-8'))
attach(HousingPrice)

#------------------------------------------------------------------------------#
HousingPrice <- subset(HousingPrice, Date > 500000)
#------------------------------------------------------------------------------#

# Deal with Trancation Date

HousingPrice$TransYear <- sapply(HousingPrice$Date, 
                                 function(x) {if(x < 1000000) {as.numeric(str_sub(x, 1, 2)) + 1911} 
                                   else{as.numeric(str_sub(x, 1, 3)) + 1911}})
HousingPrice$TransMonth <- sapply(HousingPrice$Date, 
                                  function(x) {if(x < 1000000) {as.numeric(str_sub(x, 3, 4))} 
                                    else{as.numeric(str_sub(x, 4, 5))}})
HousingPrice$TransDay <- sapply(HousingPrice$Date, 
                                function(x) {if(x < 1000000) {as.numeric(str_sub(x, 5, 6))} 
                                  else {as.numeric(str_sub(x, 6, 7))}})
HousingPrice$TransQtr <- sapply(HousingPrice$TransMonth, 
                                function(x) {if(x >=1 & x <= 3) as.numeric("1")
                                  else if (x >= 4 & x <= 6) as.numeric("2")
                                  else if (x >= 7 & x <= 9) as.numeric("3")
                                  else if (x >= 10 & x <= 12) as.numeric("4")
                                  else as.numeric("0")})
HousingPrice <- subset(HousingPrice, HousingPrice$TransQtr != 0)

#------------------------------------------------------------------------------#

# summary(factor(HousingPrice$TransactionSign))
# # 土地                 建物      房地(土地+建物)      房地(土地+建物)+車位         車位 
# # 703472                17134              1123312                  705678         39314
# 
# summary(HousingPrice$ShiftingArea)
# summary(factor(HousingPrice$MetroZoneCheck))
# # 住        其他      商       工      農     NA's 
# # 1464802  183710     297465   48300   88385  506248
# 
# summary(factor(HousingPrice$NonMetroZoneCheck)) # 都是NA
# summary(factor(HousingPrice$NonMetroCode)) # 都是NA
HousingPrice <- HousingPrice[,-c(6:7)] #移除NonMetroZoneCheck, NonMetroCode

#------------------------------------------------------------------------------#

# Deal with Transaction Type

h <- hash()
h['土地'] <- 'Land'
h['建物'] <- 'Building' 
h[c('房地(土地+建物)', '房地(土地+建物)+車位')] <- 'Building_Land'
h['車位'] <- 'Park'

HousingPrice$TransactionType <- values(h, HousingPrice$TransactionSign)
clear(h)
rm(h)

#------------------------------------------------------------------------------#

# Deal with COUNTY & COUNTY_ID

# h_County <- hash()
# h_County['臺北市'] <- 'TaipeiCity'
# h_County[c('新北市', '臺北縣')] <- 'NewTaipeiCity'
# h_County[c('桃園市', '桃園縣')] <- 'TaoyuanCity'
# h_County[c('臺中市', '臺中縣')] <- 'TaichungCity'
# h_County[c('臺南市', '臺南縣')] <- 'TainanCity'
# h_County[c('高雄市', '高雄縣')] <- 'KaohsiungCity'
# h_County[c('宜蘭縣')] <- 'YilanCounty'
# h_County[c('新竹縣')] <- 'HsinchuCounty'
# h_County[c('新竹市')] <- 'HsinchuCity'
# h_County[c('苗栗縣')] <- 'MiaoliCounty'
# h_County[c('彰化縣')] <- 'ChanghuaCounty'
# h_County[c('南投縣')] <- 'NantouCounty'
# h_County[c('雲林縣')] <- 'YunlinCounty'
# h_County[c('嘉義縣')] <- 'ChiayiCounty'
# h_County[c('嘉義市')] <- 'ChiayiCity'
# h_County[c('屏東縣')] <- 'PingtungCounty'
# h_County[c('臺東縣')] <- 'TaitungCounty'
# h_County[c('花蓮縣')] <- 'HualienCounty'
# h_County[c('澎湖縣')] <- 'PenghuCounty'
# h_County[c('基隆市')] <- 'KeelungCity'
# h_County[c('連江縣')] <- 'LienchiangCounty'
# h_County[c('金門縣')] <- 'KinmenCounty'
# 
# HousingPrice$COUNTY <- values(h_County, substr(subset(HousingPrice$Address, 
#                                                       HousingPrice$TransactionType %in% 
#                                                         c('Building', 'Building_Land', 'Park'))
#                                                , 1, 3))

# COUNTY - get County names from Address
HousingPrice$COUNTY <- substr(HousingPrice$Address, 1, 3)
HousingPrice_B <- subset(HousingPrice, HousingPrice$TransactionType %in% 
                           c('Building', 'Building_Land'))
HousingPrice_B <- subset(HousingPrice_B, HousingPrice_B$COUNTY %in% 
                           COUNTY_ID_list$COUNTY) # by COUNTY_ID_list

# COUNTY_ID
h_County_ID <- hash()
for (i in 1:nrow(COUNTY_ID_list)) {
  h_County_ID[c(COUNTY_ID_list$COUNTY[i])] <- as.character(COUNTY_ID_list$COUNTY_ID[i])
}
h_County_ID[c('新北市', '臺北縣')] <- '65000' # 縣市改制，使用新的代號
h_County_ID[c('桃園市', '桃園縣')] <- '68000'
h_County_ID[c('臺中市', '臺中縣')] <- '66000'
h_County_ID[c('臺南市', '臺南縣')] <- '67000'
h_County_ID[c('高雄市', '高雄縣')] <- '64000'
HousingPrice_B$COUNTY_ID <- values(h_County_ID, HousingPrice_B$COUNTY)
clear(h_County_ID)
rm(h_County_ID)

#------------------------------------------------------------------------------#
# Deal with TOWN & TOWN_ID

HousingPrice_B$TOWN_ID <- "0"
HousingPrice_B <- subset(HousingPrice_B, HousingPrice_B$TOWN %in% TOWN_ID_list$TOWN)

for (i in COUNTY_ID_list$COUNTY_ID) {
  h_Town_ID <- hash()
  sub_TOWN_list <- subset(TOWN_ID_list, TOWN_ID_list$COUNTY_ID == i)
  for (j in 1:nrow(sub_TOWN_list)) {
    h_Town_ID[c(sub_TOWN_list$TOWN[j])] <- as.character(sub_TOWN_list$TOWN_ID[j])
  }
  HousingPrice_B$TOWN_ID[HousingPrice_B$COUNTY_ID == i] <- values(h_Town_ID, HousingPrice_B$TOWN[HousingPrice_B$COUNTY_ID == i])
}
clear(h_Town_ID)
rm(h_Town_ID)

#------------------------------------------------------------------------------#
# Deal with TransFloor --- get num from chinese

# HousingPrice_B$TransFloor = gsub("層","",HousingPrice_B$TransFloor)
# summary(factor(HousingPrice_B$TransFloor))
# HousingPrice_B$TotalBuildingFloor = gsub("層","",HousingPrice_B$TotalBuildingFloor)
# summary(factor(HousingPrice_B$TotalBuildingFloor))

### By Excel after sampling

#------------------------------------------------------------------------------#

# Deal with Age --- calculated by TransYear - Built Year

HousingPrice_B <- subset(HousingPrice_B, BuiltYear != 'NA')
HousingPrice_B$Age <- mapply(function(TransYear, BuiltYear) {TransYear - (as.numeric(str_sub(BuiltYear, 1, 3)) + 1911)},
                             HousingPrice_B$TransYear, HousingPrice_B$BuiltYear)

## Pre-sales housing's age might be negative

#------------------------------------------------------------------------------#
# Deal with ParkIncluded

HousingPrice_B$ParkIncluded <- sapply(HousingPrice_B$ParkArea,
                                      function(x) {if(x != 0) 1
                                        else 0})

#------------------------------------------------------------------------------#
# MainUse 
## Dwelling: 集合住宅, 住宅, 住房, 農舍, 國民住宅, 自用農舍, 集村農舍
## Commercial: 住商用, 商業用, 工商用, 店舖, 店舖、住宅, 住工用
## Others: 廠房, 工業用, 列管標準廠房, 市場攤位, 共有部分, 農業用, NA, 見使用執照, 見其他登記事項, 見其它登記事項
summary(factor(HousingPrice_B$MainUse))
HousingPrice_B$MainUse[is.na(HousingPrice_B$MainUse)] <- "Others"
HousingPrice_B$MainUse[HousingPrice_B$MainUse %in% c("集合住宅", "住宅", "住房", "農舍", '國民住宅', "自用農舍", "集村農舍", "住家用", "住宅用")] <- "Dwelling"
HousingPrice_B$MainUse[HousingPrice_B$MainUse %in% c("住商用", "商業用", "工商用", "店舖", "店舖、住宅", "住工用")] <- "Commercial"
HousingPrice_B$MainUse[HousingPrice_B$MainUse %in% c("廠房", "工業用", "列管標準廠房", "市場攤位", "共有部分","停車空間", "農業用", "", "見使用執照", "見其他登記事項", "見其它登記事項")] <- "Others"

#------------------------------------------------------------------------------#
# Deal with RelatedPartyTrans (關係人交易)
HousingPrice_B$RelatedPartyTrans <- sapply(HousingPrice_B$Note,
                                      function(x) {if(grepl("特殊關係", x) == TRUE | 
                                                      grepl("二等親", x) == TRUE | 
                                                      grepl("二親等", x) == TRUE |
                                                      grepl("親友間", x) == TRUE |
                                                      grepl("親屬", x) == TRUE |
                                                      grepl("股東購屋", x) == TRUE) 1
                                        else 0})

#------------------------------------------------------------------------------#
# Deal with BuildingType
summary(factor(HousingPrice_B$BuildingType))
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("住宅大樓(11層含以上有電梯)")] <- "Building_over10"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("公寓(5樓含以下無電梯)")] <- "Apartment"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("華廈(10層含以下有電梯)")] <- "Building_under10"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("透天厝")] <- "SingleHouse"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("店面(店鋪)")] <- "Store"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("辦公商業大樓")] <- "CommercialBuilding"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("套房(1房1廳1衛)")] <- "Suite"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("倉庫", "其他", "工廠", "廠辦")] <- "Others"
HousingPrice_B$BuildingType[HousingPrice_B$BuildingType %in% c("農舍")] <- "Farmhouse"

#------------------------------------------------------------------------------#
# Deal with ZoneType
summary(factor(HousingPrice_B$MetroZoneCheck))
HousingPrice_B$ZoneType <- "0"
HousingPrice_B$MetroZoneCheck[is.na(HousingPrice_B$MetroZoneCheck)] <- "其他"
HousingPrice_B$ZoneType[HousingPrice_B$MetroZoneCheck == "住"] <- "ResidentialDistrict"
HousingPrice_B$ZoneType[HousingPrice_B$MetroZoneCheck == "商"] <- "BusinessDistrict"
HousingPrice_B$ZoneType[HousingPrice_B$MetroZoneCheck == "工"] <- "IndustrialDistrict"
HousingPrice_B$ZoneType[HousingPrice_B$MetroZoneCheck == "農"] <- "AgriculturalDistrict"
HousingPrice_B$ZoneType[HousingPrice_B$MetroZoneCheck %in% c("其他")] <- "Others"
summary(factor(HousingPrice_B$ZoneType))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Add Attributes

# sapply(HousingPrice_B, class)
HousingPrice_B$TOWN_ID <- sapply(HousingPrice_B$TOWN_ID, as.character)

HousingPrice_B <- subset(HousingPrice_B, HousingPrice_B$ShiftingArea < 300000)
HousingPrice_B <- subset(HousingPrice_B, HousingPrice_B$TotalPrice < 10^8)

HousingPrice_B$Coordinate <- "0"
HousingPrice_B$DistanceToCBD <- "0"
HousingPrice_B$DistanceToHSR <- "0"
HousingPrice_B$TrainStationsNearby <- "0"
HousingPrice_B$BusStopNearby <- "0"
HousingPrice_B$ParkNearby <- "0"
HousingPrice_B$HospitalNearby <- "0"
HousingPrice_B$SchoolNearby <- "0"

colnames(HousingPrice_B) <- c(
  "TOWN", "TransactionSign", "Address", "TransLot",        
  "MetroZoneCheck", "Date", "TransactionCnt", "TransStory",         
  "TotalBuildingStories", "BuildingType", "MainUse", "MainMaterial",
  "BuiltYear", "TransArea", "Bedrooms", "Livingrooms",  
  "Bathrooms", "PresentPartition", "ManagementOrNot", "TotalPrice",         
  "UnitPrice", "ParkType", "ParkArea", "ParkTotalPrice",     
  "Note", "SerialNumber", "TransYear", "TransMonth",         
  "TransDay", "TransQtr", "TransactionType", "COUNTY",             
  "COUNTY_ID", "TOWN_ID", "Age","ParkIncluded", "RelatedPartyTrans",  
   "ZoneType", "Coordinate", "DistanceToCBD", "DistanceToHSR",      
  "TrainStationsNearby", "BusStopNearby", "ParkNearby", "HospitalNearby", "SchoolNearby")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Sampling -- Taoyuan 68000
HousingPrice_Taoyuan <- subset(HousingPrice_B, HousingPrice_B$COUNTY_ID == "68000")

set.seed(1)
HousingPrice_Taoyuan_S <- subset(HousingPrice_Taoyuan, 
                                 HousingPrice_Taoyuan$TransYear == "2019")[sample(1:nrow(subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2019")), size = 100), ]
set.seed(1)
HousingPrice_Taoyuan_S <- rbind(HousingPrice_Taoyuan_S, 
                                subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2018")[sample(1:nrow(subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2018")), size = 100), ])
set.seed(1)
HousingPrice_Taoyuan_S <- rbind(HousingPrice_Taoyuan_S, 
                                subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2017")[sample(1:nrow(subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2017")), size = 100), ])
set.seed(1)
HousingPrice_Taoyuan_S <- rbind(HousingPrice_Taoyuan_S, 
                                subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2016")[sample(1:nrow(subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2016")), size = 100), ])
set.seed(1)
HousingPrice_Taoyuan_S <- rbind(HousingPrice_Taoyuan_S, 
                                subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2015")[sample(1:nrow(subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2015")), size = 100), ])
set.seed(1)
HousingPrice_Taoyuan_S <- rbind(HousingPrice_Taoyuan_S, 
                                subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2014")[sample(1:nrow(subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2014")), size = 100), ])
set.seed(1)
HousingPrice_Taoyuan_S <- rbind(HousingPrice_Taoyuan_S, 
                                subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2013")[sample(1:nrow(subset(HousingPrice_Taoyuan, HousingPrice_Taoyuan$TransYear == "2013")), size = 100), ])
rm(HousingPrice_Taoyuan)
#------------------------------------------------------------------------------#
# Sampling -- Yunlin 10009
HousingPrice_Yunlin <- subset(HousingPrice_B, HousingPrice_B$COUNTY_ID == "10009")

set.seed(1)
HousingPrice_Yunlin_S <- subset(HousingPrice_Yunlin, 
                                 HousingPrice_Yunlin$TransYear == "2019")[sample(1:nrow(subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2019")), size = 100), ]
set.seed(1)
HousingPrice_Yunlin_S <- rbind(HousingPrice_Yunlin_S, 
                                subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2018")[sample(1:nrow(subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2018")), size = 100), ])
set.seed(1)
HousingPrice_Yunlin_S <- rbind(HousingPrice_Yunlin_S, 
                                subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2017")[sample(1:nrow(subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2017")), size = 100), ])
set.seed(1)
HousingPrice_Yunlin_S <- rbind(HousingPrice_Yunlin_S, 
                                subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2016")[sample(1:nrow(subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2016")), size = 100), ])
set.seed(1)
HousingPrice_Yunlin_S <- rbind(HousingPrice_Yunlin_S, 
                                subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2015")[sample(1:nrow(subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2015")), size = 100), ])
set.seed(1)
HousingPrice_Yunlin_S <- rbind(HousingPrice_Yunlin_S, 
                                subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2014")[sample(1:nrow(subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2014")), size = 100), ])
set.seed(1)
HousingPrice_Yunlin_S <- rbind(HousingPrice_Yunlin_S, 
                                subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2013")[sample(1:nrow(subset(HousingPrice_Yunlin, HousingPrice_Yunlin$TransYear == "2013")), size = 100), ])
rm(HousingPrice_Yunlin)

#------------------------------------------------------------------------------#
# Sampling -- ChiayiCity 10020, ChiayiCounty 10010

HousingPrice_Chiayi <- subset(HousingPrice_B, HousingPrice_B$COUNTY_ID %in% c("10020", "10010"))

set.seed(1)
HousingPrice_Chiayi_S <- subset(HousingPrice_Chiayi, 
                                HousingPrice_Chiayi$TransYear == "2019")[sample(1:nrow(subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2019")), size = 100), ]
set.seed(1)
HousingPrice_Chiayi_S <- rbind(HousingPrice_Chiayi_S, 
                               subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2018")[sample(1:nrow(subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2018")), size = 100), ])
set.seed(1)
HousingPrice_Chiayi_S <- rbind(HousingPrice_Chiayi_S, 
                               subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2017")[sample(1:nrow(subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2017")), size = 100), ])
set.seed(1)
HousingPrice_Chiayi_S <- rbind(HousingPrice_Chiayi_S, 
                               subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2016")[sample(1:nrow(subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2016")), size = 100), ])
set.seed(1)
HousingPrice_Chiayi_S <- rbind(HousingPrice_Chiayi_S, 
                               subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2015")[sample(1:nrow(subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2015")), size = 100), ])
set.seed(1)
HousingPrice_Chiayi_S <- rbind(HousingPrice_Chiayi_S, 
                               subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2014")[sample(1:nrow(subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2014")), size = 100), ])
set.seed(1)
HousingPrice_Chiayi_S <- rbind(HousingPrice_Chiayi_S, 
                               subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2013")[sample(1:nrow(subset(HousingPrice_Chiayi, HousingPrice_Chiayi$TransYear == "2013")), size = 100), ])
rm(HousingPrice_Chiayi)

#------------------------------------------------------------------------------#
# Sampling -- TainanCity 67000

HousingPrice_Tainan <- subset(HousingPrice_B, HousingPrice_B$COUNTY_ID %in% c("67000"))

set.seed(1)
HousingPrice_Tainan_S <- subset(HousingPrice_Tainan, 
                                HousingPrice_Tainan$TransYear == "2019")[sample(1:nrow(subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2019")), size = 100), ]
set.seed(1)
HousingPrice_Tainan_S <- rbind(HousingPrice_Tainan_S, 
                               subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2018")[sample(1:nrow(subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2018")), size = 100), ])
set.seed(1)
HousingPrice_Tainan_S <- rbind(HousingPrice_Tainan_S, 
                               subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2017")[sample(1:nrow(subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2017")), size = 100), ])
set.seed(1)
HousingPrice_Tainan_S <- rbind(HousingPrice_Tainan_S, 
                               subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2016")[sample(1:nrow(subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2016")), size = 100), ])
set.seed(1)
HousingPrice_Tainan_S <- rbind(HousingPrice_Tainan_S, 
                               subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2015")[sample(1:nrow(subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2015")), size = 100), ])
set.seed(1)
HousingPrice_Tainan_S <- rbind(HousingPrice_Tainan_S, 
                               subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2014")[sample(1:nrow(subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2014")), size = 100), ])
set.seed(1)
HousingPrice_Tainan_S <- rbind(HousingPrice_Tainan_S, 
                               subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2013")[sample(1:nrow(subset(HousingPrice_Tainan, HousingPrice_Tainan$TransYear == "2013")), size = 100), ])
rm(HousingPrice_Tainan)

#------------------------------------------------------------------------------#
# Sampling -- MiaoliCounty 10005

HousingPrice_Miaoli <- subset(HousingPrice_B, HousingPrice_B$COUNTY_ID %in% c("10005"))

set.seed(1)
HousingPrice_Miaoli_S <- subset(HousingPrice_Miaoli, 
                                HousingPrice_Miaoli$TransYear == "2019")[sample(1:nrow(subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2019")), size = 100), ]
set.seed(1)
HousingPrice_Miaoli_S <- rbind(HousingPrice_Miaoli_S, 
                               subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2018")[sample(1:nrow(subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2018")), size = 100), ])
set.seed(1)
HousingPrice_Miaoli_S <- rbind(HousingPrice_Miaoli_S, 
                               subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2017")[sample(1:nrow(subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2017")), size = 100), ])
set.seed(1)
HousingPrice_Miaoli_S <- rbind(HousingPrice_Miaoli_S, 
                               subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2016")[sample(1:nrow(subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2016")), size = 100), ])
set.seed(1)
HousingPrice_Miaoli_S <- rbind(HousingPrice_Miaoli_S, 
                               subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2015")[sample(1:nrow(subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2015")), size = 100), ])
set.seed(1)
HousingPrice_Miaoli_S <- rbind(HousingPrice_Miaoli_S, 
                               subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2014")[sample(1:nrow(subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2014")), size = 100), ])
set.seed(1)
HousingPrice_Miaoli_S <- rbind(HousingPrice_Miaoli_S, 
                               subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2013")[sample(1:nrow(subset(HousingPrice_Miaoli, HousingPrice_Miaoli$TransYear == "2013")), size = 100), ])
rm(HousingPrice_Miaoli)


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Use Python for dealing the Coordinate

# write_csv(HousingPrice_B, 'HousingPrice_B.csv')
# write_csv(HousingPrice_Taoyuan, 'HousingPrice_Taoyuan.csv')
# write_csv(HousingPrice_Taoyuan_S, 'HousingPrice_Taoyuan_S.csv')
# write_csv(HousingPrice_Yunlin_S, 'HousingPrice_Yunlin_S.csv')
# write_csv(HousingPrice_Chiayi_S, 'HousingPrice_Chiayi_S.csv')
# write_csv(HousingPrice_Tainan_S, 'HousingPrice_Tainan_S.csv')
# write_csv(HousingPrice_Miaoli_S, 'HousingPrice_Miaoli_S.csv')

# HousingPrice_Taoyuan_S['DistanceToIC'] <- "0"
# HousingPrice_Yunlin_S['DistanceToIC'] <- "0"
