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

filter <- unique(small) 
small <- filter

countiesL <- tally(small$A_COUNTY)
data4 <- data.frame(countiesL)
data6 <- rename(data4, c("X" = "A_COUNTY"))
data7 <- merge(data6, small) #merge the frequencies of the counties 
data7 <- unique(data7) #unique observations include 48577 total 

#create new variable here: only counties 
data7$A_COUNTY<- as.character(data7$A_COUNTY)

data8 <- data7 %>%
  filter(Freq > 10)
data8$A_COUNTY <- as.factor(data8$A_COUNTY)
nrow(data8) #48169 observations 


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

data9 <- unique(data9) #819 observations so county level data pulled only for these counties 
data9$state <- as.numeric(data9$state)
data9$county <- as.numeric(data9$county)
data91 <- data9 %>% mutate(StateCounty = ((1000*state) + county))

ACS <- load("dfTotData.csv")
data91 <- rename(data91, c("StateCounty" = "FIPSCode"))
FinalMerge2<- dfTotData %>% right_join(data91, by = "FIPSCode")

########Build training and test models here
FinalMerge2<- rename(FinalMerge2, c("B27001_001" = "HealthInsuCovTotal"))
FinalMerge2<- rename(FinalMerge2, c("B27001_002" = "HealthInsuCovMale"))
FinalMerge2<- rename(FinalMerge2, c("B27001_030" = "HealthInsuCovFemale"))
FinalMerge2<- rename(FinalMerge2, c("C17002_001" = "IncomeToPovRatio"))
FinalMerge2<- rename(FinalMerge2, c("B01001_002" = "TotalMale"))
FinalMerge2<- rename(FinalMerge2, c("B01001_026" = "TotalFemale"))
FinalMerge2<- rename(FinalMerge2, c("B01003_001" = "TotalPopulation"))

mergeData <- FinalMerge2
mergeData$county <- as.factor(mergeData$county)
n <- nrow(mergeData)
shuffled <- mergeData[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

train$FIPSCode <- as.factor(train$FIPSCode)
logmod <- glm(formula = V_DR_DRINK ~ P_SEX + Age + FIPSCode + P_DRUGS + TravSpeed + P_DOA + 
                V_DEFORMED + A_FATALS + PSuspension + PCrash + PDWI + PSpeed + PCrash + HealthInsuCovTotal + 
                IncomeToPovRatio + TotalMale + TotalFemale + TotalPopulation, 
              family=binomial(link='logit'), data = train)

summary(logmod)



require(randomForest)
mergeData$V_DR_DRINK <- as.numeric(mergeData$V_DR_DRINK)
treemod <- randomForest(V_DR_DRINK ~ P_SEX + Age + FIPSCode + P_DRUGS + TravSpeed + P_DOA + 
  V_DEFORMED + A_FATALS + PSuspension + PCrash + PDWI + PSpeed + PCrash + B27001_001 + 
  B27001_002 + B27001_030 + C17002_001 + B01001_002 + B01001_026, data = mergeData, ntree = 50, mtry = 3, 
  keep.forest = FALSE, importance = TRUE)


library(pscl)
pR2(logmod)

#pred <- predict(logmod, test, type = "response")
#conf <- table(test$label, pred) #building confusion matrix 
#print(sum(diag(conf)) / sum(conf)) #63% model accuracy 
