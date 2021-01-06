#system("defaults write org.R-project.R force.LANG zh_TW.UTF-8")

setwd("~/2020 Spring/Econometrics II/Econometrics_TermPaper/DataSet")

#------------------------------------------------------------------------------#
# B05704007 國企四 賴昱瑋
# Econometrics II 108-2
# 
#------------------------------------------------------------------------------#

#--- Import Library -----------------------------------------------------------#
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
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------


#Read Dataset   ----------------------------------------------------------------
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
                               "DistanceToCBD", "DistanceToHSR")], 2, summary)
apply(HousingPrice_Taoyuan_S[c("TotalPrice", "Age", "TransArea", "TransLot",
                               "Bedrooms", "Livingrooms", "Bathrooms",
                               "DistanceToCBD", "DistanceToHSR", "DistanceToIC")], 2, var)
# 敘述性統計 - Discrete Var.
apply(HousingPrice_Taoyuan_S[c("ZoneType", "BuildingType", "ParkIncluded", "RelatedPartyTrans",
                               "TrainStationsNearby", "BusStopNearby", "ParkNearby",
                               "HospitalNearby", "SchoolNearby")], 2,
      function(x) {summary(as.factor(x))})

summary(factor(HousingPrice_Taoyuan_S$TransactionType))


# 移除極值 TransLot TransArea --------------------------------------------------
nrow(subset(HousingPrice_Taoyuan_S, HousingPrice_Taoyuan_S$TransLot > quantile(HousingPrice_Taoyuan_S$TransLot, probs = 0.75) + 5*var(HousingPrice_Taoyuan_S$TransLot)^(1/2)))
HousingPrice_Taoyuan_S <- subset(HousingPrice_Taoyuan_S, TransLot < quantile(HousingPrice_Taoyuan_S$TransLot, probs = 0.75) + 5*var(HousingPrice_Taoyuan_S$TransLot)^(1/2))
nrow(subset(HousingPrice_Taoyuan_S, HousingPrice_Taoyuan_S$TransLot > quantile(HousingPrice_Taoyuan_S$TransArea, probs = 0.75) + 5*var(HousingPrice_Taoyuan_S$TransArea)^(1/2)))

# ------------------------------------------------------------------------------
# Model 1
HousingPrice_Taoyuan_S$Age_sq <- HousingPrice_Taoyuan_S$Age^2
fit1 <- lm(TotalPrice ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + Age_sq +
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

bptest(fit1)

# Box Cox Transformation
boxcox.list <- boxcox(fit1)
boxcox.list

lambda <- boxcox.list$x[which.max(boxcox.list$y)]
lambda

HousingPrice_Taoyuan_S$Age_sq <- HousingPrice_Taoyuan_S$Age^2
# Fit after transformation
fit_bc <- lm(TotalPrice^lambda ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + Age_sq +
               BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +
               factor(ParkIncluded) + factor(RelatedPartyTrans) + 
               DistanceToCBD + DistanceToHSR + DistanceToIC +
               factor(TrainStationsNearby) + factor(BusStopNearby) +
               factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby), 
             data = HousingPrice_Taoyuan_S)

summary(fit_bc)
re <- as.data.frame(fit_bc$residuals)

qqnorm(residuals(fit_bc), ylab="Sample Quantiles")
qqline(residuals(fit_bc))
hist(fit_bc$residuals, 30)
bptest(fit_bc)

identify(fit_bc$residuals)
# ------------------------------------------------------------------------------

# ---移除關係人交易

HousingPrice_Taoyuan_S_norelated <- subset(HousingPrice_Taoyuan_S, HousingPrice_Taoyuan_S$RelatedPartyTrans != 1)

# Model 1
fit_log <- lm(log(TotalPrice) ~  factor(ZoneType) + TransactionType + factor(TransYear) + factor(TransQtr) + Age + Age_sq +
             BuildingType + TransArea + TransLot + Bedrooms + Livingrooms + Bathrooms +factor(ParkIncluded) + 
             DistanceToCBD + DistanceToHSR+ DistanceToIC +
             factor(TrainStationsNearby) + factor(BusStopNearby) +
             factor(ParkNearby) + factor(HospitalNearby) + factor(SchoolNearby),
           data = HousingPrice_Taoyuan_S_norelated)

res <- as.data.frame(fit_log$residuals)
colnames(res) <- "res"
HousingPrice_Taoyuan_S_norelated <- cbind(HousingPrice_Taoyuan_S_norelated, res)

summary(fit_log)
plot(fit_log$residuals)
plot(HousingPrice_Taoyuan_S_norelated$res~(HousingPrice_Taoyuan_S_norelated$DistanceToHSR))
qqnorm(residuals(fit_log), ylab="Sample Quantiles")
qqline(residuals(fit_log))
hist(fit_log$residuals, 20)

identify(fit_log$residuals)
