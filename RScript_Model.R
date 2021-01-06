#system("defaults write org.R-project.R force.LANG zh_TW.UTF-8")

setwd("~/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet")

#------------------------------------------------------------------------------#
# B05704007 國企四 賴昱瑋
# Econometrics II 108-2
# 
#------------------------------------------------------------------------------#

#--- Import Library -----------------------------------------------------------
library(haven)
library(readr)
library(ggplot2)
library(plm)
library(lmtest)
library(dplyr)
library(stringr)
library(lubridate)
library(hash)
library(car)
library(stats)
library(MASS)
library(moments)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------#

HousingPrice_Taoyuan_S <- read_csv('HousingPrice_Taoyuan_S.csv', 
                                   locale = locale(encoding = 'UTF-8'), 
                                   col_types = cols(
                                     TotalPrice = col_double(),
                                     TransYear = col_integer(),
                                     TransQtr = col_integer(),
                                     Age = col_integer(),
                                     TransArea = col_double(),
                                     TransLot = col_double(),
                                     Bedrooms = col_integer(),
                                     Livingrooms = col_integer(),
                                     Bathrooms = col_integer(),
                                     ParkIncluded = col_integer(),
                                     RelatedPartyTrans = col_integer(),
                                     DistanceToCBD = col_double(),
                                     DistanceToHSR = col_double(),
                                     TrainStationsNearby = col_integer(),
                                     BusStopNearby = col_integer(),
                                     ParkNearby = col_integer(),
                                     HospitalNearby = col_integer(),
                                     SchoolNearby = col_integer(),
                                     DistanceToIC = col_double()
                                   ))
attach(HousingPrice_Taoyuan_S)

# ------------------------------------------------------------------------------
# 敘述性統計 - Cont.Var.
apply(HousingPrice_Taoyuan_S[c("TotalPrice", "Age", "TransArea", "TransLot",
                               "Bedrooms", "Livingrooms", "Bathrooms",
                               "DistanceToCBD", "DistanceToHSR", "DistanceToIC")], 2, summary)
apply(HousingPrice_Taoyuan_S[c("TotalPrice", "Age", "TransArea", "TransLot",
                               "Bedrooms", "Livingrooms", "Bathrooms",
                               "DistanceToCBD", "DistanceToHSR", "DistanceToIC")], 2, var)
# 敘述性統計 - Discrete Var.
apply(HousingPrice_Taoyuan_S[c("ZoneType", "BuildingType", "ParkIncluded", "RelatedPartyTrans",
                               "TrainStationsNearby", "BusStopNearby", "ParkNearby",
                               "HospitalNearby", "SchoolNearby")], 2,
                              function(x) {summary(as.factor(x))})
# ------------------------------------------------------------------------------
# Model 1
fit1 <- lm(TotalPrice ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + 
             BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
             factor(ParkIncluded) + factor(RelatedPartyTrans) + 
             DistanceToCBD + DistanceToHSR + DistanceToIC +
             factor(TrainStationsNearby) + factor(BusStopNearby) +
             factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby), 
           data = HousingPrice_Taoyuan_S)

summary(fit1)
plot(fit1$residuals)
qqnorm(residuals(fit1), ylab="Sample Quantiles")
qqline(residuals(fit1))
hist(fit1$residuals, 100)

# Box Cox Transformation
boxcox.list <- boxcox(fit1)
boxcox.list

lambda <- boxcox.list$x[which.max(boxcox.list$y)]
lambda

# Fit after transformation
fit_bc <- lm(TotalPrice^lambda ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + 
             BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
             factor(ParkIncluded) + factor(RelatedPartyTrans) + 
             DistanceToCBD + DistanceToHSR + DistanceToIC +
             factor(TrainStationsNearby) + factor(BusStopNearby) +
             factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby), 
           data = HousingPrice_Taoyuan_S)

summary(fit_bc)
plot(fit_bc$residuals)
qqnorm(residuals(fit_bc), ylab="Sample Quantiles")
qqline(residuals(fit_bc))
hist(fit_bc$residuals, 30)

kurtosis(fit_bc_step$residuals)
skewness(fit_bc_step$residuals)
#------------------------------------------------------------------------------#

fit2 <- plm(log(TotalPrice) ~ factor(TOWN_ID) + TransactionType + (TransYear) + factor(TransQtr) + Age + 
             BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
             factor(ParkIncluded) + factor(RelatedPartyTrans) + 
             DistanceToCBD + DistanceToHSR + DistanceToIC +
             factor(TrainStationsNearby) + factor(BusStopNearby) +
             factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby),
            index = c("TOWN_ID"), 
            model = "within",
            data = HousingPrice_Taoyuan_S)

summary(fit2)
plot(fit2$residuals)
qqnorm(residuals(fit2), ylab="Sample Quantiles")
qqline(residuals(fit2))
hist(fit2$residuals, 50)

#------------------------------------------------------------------------------#


fit_bc_step <- step(fit_bc, direction = "backward")
summary(fit_bc_step)

plot(fit_bc_step$residuals)
qqnorm(residuals(fit_bc_step), ylab="Sample Quantiles")
qqline(residuals(fit_bc_step))
hist(fit_bc_step$residuals, 30)

dwtest(fit_bc_step)


fit1 <- lm((fit_bc_step$residuals)^2 ~ fit_bc_step$fitted.values + I(fit_bc_step$fitted.values^2))
summary(fit1)

chi_stat = 700 * summary(fit1)$r.squared
chi_stat
qchisq(0.95, df = 671) 

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
HousingPrice_Yunlin_S <- read_csv('HousingPrice_Yunlin_S.csv', 
                                   locale = locale(encoding = 'UTF-8'), 
                                   col_types = cols(
                                     TotalPrice = col_double(),
                                     TransYear = col_integer(),
                                     TransQtr = col_integer(),
                                     Age = col_integer(),
                                     TransArea = col_double(),
                                     TransLot = col_double(),
                                     Bedrooms = col_integer(),
                                     Livingrooms = col_integer(),
                                     Bathrooms = col_integer(),
                                     ParkIncluded = col_integer(),
                                     RelatedPartyTrans = col_integer(),
                                     DistanceToCBD = col_double(),
                                     DistanceToHSR = col_double(),
                                     TrainStationsNearby = col_integer(),
                                     BusStopNearby = col_integer(),
                                     ParkNearby = col_integer(),
                                     HospitalNearby = col_integer(),
                                     SchoolNearby = col_integer(),
                                     DistanceToIC = col_double()
                                   ))
attach(HousingPrice_Yunlin_S)
#------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------
# 敘述性統計 - Cont.Var.
apply(HousingPrice_Yunlin_S[c("TotalPrice", "Age", "TransArea", "TransLot",
                               "Bedrooms", "Livingrooms", "Bathrooms",
                               "DistanceToCBD", "DistanceToHSR", "DistanceToIC")], 2, summary)
apply(HousingPrice_Yunlin_S[c("TotalPrice", "Age", "TransArea", "TransLot",
                               "Bedrooms", "Livingrooms", "Bathrooms",
                               "DistanceToCBD", "DistanceToHSR", "DistanceToIC")], 2, var)
# 敘述性統計 - Discrete Var.
apply(HousingPrice_Yunlin_S[c("ZoneType", "BuildingType", "ParkIncluded", "RelatedPartyTrans",
                               "TrainStationsNearby", "BusStopNearby", "ParkNearby",
                               "HospitalNearby", "SchoolNearby")], 2,
      function(x) {summary(as.factor(x))})
# ------------------------------------------------------------------------------
# Model 1
Yunlin_fit1 <- lm(TotalPrice ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + 
             BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
             factor(ParkIncluded) + factor(RelatedPartyTrans) + 
             DistanceToCBD + DistanceToHSR + DistanceToIC +
             factor(TrainStationsNearby) + factor(BusStopNearby) +
             factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby), 
           data = HousingPrice_Yunlin_S)

summary(Yunlin_fit1)
plot(Yunlin_fit1$residuals)
qqnorm(residuals(Yunlin_fit1), ylab="Sample Quantiles")
qqline(residuals(Yunlin_fit1))

hist(Yunlin_fit1$residuals, 100)

# Box Cox Transformation
boxcox.list <- boxcox(Yunlin_fit1)
boxcox.list

lambda <- boxcox.list$x[which.max(boxcox.list$y)]
lambda

# Fit after transformation
fit_bc <- lm(TotalPrice^lambda ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + 
               BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
               factor(ParkIncluded) + factor(RelatedPartyTrans) + 
               DistanceToCBD + DistanceToHSR + DistanceToIC +
               factor(TrainStationsNearby) + factor(BusStopNearby) +
               factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby), 
             data = HousingPrice_Yunlin_S)

summary(fit_bc)
plot(fit_bc$residuals)
qqnorm(residuals(fit_bc), ylab="Sample Quantiles")
qqline(residuals(fit_bc))
hist(fit_bc$residuals, 30)

#------------------------------------------------------------------------------#

Yunlin_fit2 <- plm(log(TotalPrice) ~ factor(TOWN_ID) + TransactionType + (TransYear) + factor(TransQtr) + Age + 
              BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
              factor(ParkIncluded) + factor(RelatedPartyTrans) + 
              DistanceToCBD + DistanceToHSR + DistanceToIC +
              factor(TrainStationsNearby) + factor(BusStopNearby) +
              factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby),
            index = c("TOWN_ID"), 
            model = "within",
            data = HousingPrice_Yunlin_S)

summary(Yunlin_fit2)
plot(Yunlin_fit2$residuals)
qqnorm(residuals(Yunlin_fit2), ylab="Sample Quantiles")
qqline(residuals(Yunlin_fit2))
hist(Yunlin_fit2$residuals, 50)

#------------------------------------------------------------------------------#

Yunlin_fit_bc_step <- step(fit_bc, direction = "backward")
summary(Yunlin_fit_bc_step)

plot(Yunlin_fit_bc_step$residuals)
qqnorm(residuals(Yunlin_fit_bc_step), ylab="Sample Quantiles")
qqline(residuals(Yunlin_fit_bc_step))
hist(Yunlin_fit_bc_step$residuals, 30)

dwtest(Yunlin_fit_bc_step)

# Test
Yunlin_fitTest0 <- lm((Yunlin_fit1$residuals)^2 ~ Yunlin_fit1$fitted.values + I(Yunlin_fit1$fitted.values^2))
summary(Yunlin_fitTest0)

chi_stat = 700 * summary(Yunlin_fitTest0)$r.squared
chi_stat
qchisq(0.95, df = 671) 


Yunlin_fitTest <- lm((Yunlin_fit_bc_step$residuals)^2 ~ Yunlin_fit_bc_step$fitted.values + I(Yunlin_fit_bc_step$fitted.values^2))
summary(Yunlin_fitTest)

chi_stat = 700 * summary(Yunlin_fitTest)$r.squared
chi_stat
qchisq(0.95, df = 671) 

# plot
plot(log(TotalPrice) ~ factor(Date))


# Model log
Yunlin_fit1 <- lm(log(TotalPrice) ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + 
                    BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
                    factor(ParkIncluded) + factor(RelatedPartyTrans) + 
                    DistanceToCBD + DistanceToHSR + DistanceToIC +
                    factor(TrainStationsNearby) + factor(BusStopNearby) +
                    factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby), 
                  data = HousingPrice_Yunlin_S)

summary(Yunlin_fit1)
plot(Yunlin_fit1$residuals)
qqnorm(residuals(Yunlin_fit1), ylab="Sample Quantiles")
qqline(residuals(Yunlin_fit1))
identify(Yunlin_fit1$residuals)
hist(Yunlin_fit1$residuals, 80)
