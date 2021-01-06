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

# Function - String to Time (Year-Quater)
str_to_d <- function(info_str) {
  YM_split <- strsplit(info_str, "Y", fixed=T)
  if(YM_split[[1]][2] == "03M"){
    result_d <- as.yearqtr(paste(as.numeric(YM_split[[1]][1])+1911, "Q1"))
  }
  else if(YM_split[[1]][2] == "06M"){
    result_d <- as.yearqtr(paste(as.numeric(YM_split[[1]][1])+1911, "Q2"))
  }
  else if(YM_split[[1]][2] == "09M"){
    result_d <- as.yearqtr(paste(as.numeric(YM_split[[1]][1])+1911, "Q3"))
  }
  else if(YM_split[[1]][2] == "12M"){
    result_d <- as.yearqtr(paste(as.numeric(YM_split[[1]][1])+1911, "Q4"))
  }
  return(result_d)
}

#------------------------------------------------------------------------------#
for (i in c(97:108)) {
  for (j in c(1:4)) {
    if (i == 97 & j ==1) {
      # var_name <- paste("Population_Village_Y", i, "Q", j, sep="")
      # Population <- assign(var_name, read_csv(paste0("Population/Population_Village_Y",
      #                                 i, "Q", " (", j, ").csv"
      #                                  ))[-c(1),])
      Population <- read_csv(paste0("Population/Population_Village_Y",
                                      i, "Q", " (", j, ").csv"
                                       ))[-c(1),]
    }
    else{
      # var_name <- paste("Population_Village_Y", i, "Q", j, sep="")
      Population <- rbind(Population, read_csv(paste0("Population/Population_Village_Y",
                                      i, "Q", " (", j, ").csv"))[-c(1),])
      # Population <- rbind(Population, assign(var_name, read_csv(paste0("Population/Population_Village_Y",
      #                                 i, "Q", " (", j, ").csv"))[-c(1),]))
    }
  }
}
attach(Population)

Population$H_CNT <- as.integer(Population$H_CNT)
Population$P_CNT <- as.integer(Population$P_CNT)
Population$M_CNT <- as.integer(Population$M_CNT)
Population$F_CNT <- as.integer(Population$F_CNT)
# Population$YearQtr <- c('')
# Population$YearQtr <- lapply(Population$INFO_TIME, str_to_d)

#------------------------------------------------------------------------------#
# 縣市代碼
COUNTY_ID_list <- Population %>% group_by(COUNTY_ID, COUNTY) %>% summarize()

# 鄉鎮代碼
TOWN_ID_list <- Population %>% 
  group_by(TOWN_ID, TOWN, COUNTY_ID, COUNTY) %>% 
  summarize()
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

