data = read.csv("DriversData.csv")
library(dplyr)
library(mlbench)
library(caret)
library(readr)
require(mosaic)
library(plyr)
library(randomForest)
library(geepack)
library(gee)

#should get around 30000 observations: if more than person involved in an accident 

small <- data %>% dplyr::select(V_V_CONFIG, V_TRAV_SP, V_DEFORMED, V_DR_DRINK, V_PREV_ACC, 
                                V_PREV_SUS, V_PREV_DWI, V_PREV_SPD, V_VALIGN, V_VPAVETYP, V_VSURCOND,
                                A_LGT_COND, A_FATALS, A_DRUNK_DR, A_WEATHER, A_WEATHER1, A_WEATHER2,
                                A_WRK_ZONE, A_MAN_COLL, P_AGE, P_SEX, P_INJ_SEV, P_SEAT_POS, 
                                P_AIR_BAG, P_EJECTION, P_EJ_PATH, P_DRINKING, P_DRUGS, 
                                P_LAG_HRS, P_DOA, A_COUNTY, A_STATE, A_CITY)

filter <- unique(small) #now 48577 observations after filtering (originally were 48613 observations)
small <- filter

countiesL <- tally(small$A_COUNTY)
data4 <- data.frame(countiesL)
data6 <- rename(data5, c("X" = "A_COUNTY"))
data7 <- merge(data6, small) #merge the frequencies of the counties 
data7 <- unique(data7) #unique observations include 48577 total 

#create new variable here: only counties 
data7$A_COUNTY<- as.character(data7$A_COUNTY)

data8 <- data7 %>%
  filter(Freq > 10)
data8$A_COUNTY <- as.factor(data8$A_COUNTY)
nrow(data8) #48346 observations: works 


data8$P_SEX = as.factor(data8$P_SEX)
data8$P_DRUGS = as.factor(data8$P_DRUGS)
data8$P_DOA = as.factor(data8$P_DOA)
data8$V_DEFORMED = as.factor(data8$V_DEFORMED)
data8$V_DR_DRINK = as.factor(data8$V_DR_DRINK)

data8 <- rename(data8, c("A_STATE" = "state"))
data8 <- rename(data8, c("A_COUNTY" = "county"))
data8$state = as.factor(data8$state)
data8$county = as.factor(data8$county)

data8 <- data8 %>% mutate(TravSpeed = as.numeric(V_TRAV_SP),
                          Age = as.numeric(P_AGE),
                          PDWI = as.numeric(V_PREV_DWI),
                          PSuspension = as.numeric(V_PREV_SUS),
                          PCrash = as.numeric(V_PREV_ACC),
                          PSpeed = as.numeric(V_PREV_SPD))

data9 <- data8

food <- load("FoodStampCounties.Rda")
pop <- load("TotalPopulation.Rda")
pov <- load("PovIncomeRatioCounties.Rda")
data9 <- unique(data9)
foodACS <- unique(s2)
PovACS <- unique(s3)
PopACS <- unique(s4)
merge1 <- merge(foodACS, PovACS)
mergeACS<- merge(merge1, PopACS)

mergeACS$county <- as.factor(mergeACS$county)
#merge makes sense since just retaining information for the 810 counties with pulled data 
FinalMerge <- merge(mergeACS, data9) #9614 observations 

########Build training and test models here
mergeData <- FinalMerge
n <- nrow(mergeData)
shuffled <- mergeData[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]


#Account for correlation structure induced by the same county 
train$V_DR_DRINK <- as.numeric(train$V_DR_DRINK)
GEEmod <- summary(gee(formula = V_DR_DRINK ~ county + P_SEX + Age + P_DRUGS + 
                        P_DOA + V_DEFORMED + TravSpeed + A_FATALS + PCrash + 
                        PSuspension + PDWI + PSpeed + C17002_001E + 
                        C22001_001E + C22001_002E + C22001_003E, B01003_001E, id = (county), 
                      data = train, corstr = "independence"))
