
data = read.csv("DriversData.csv")

library(plyr)
library(dplyr)
library(mlbench)
library(caret)
library(readr)
require(mosaic)
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
data6 <- rename(data5, A_COUNTY = X)
#data6 <- rename(data5, c("X" = "A_COUNTY"))
data7 <- merge(data6, small) #merge the frequencies of the counties 
data7 <- unique(data7) #unique observations include 48577 total 

#create new variable here: only counties 
data7$A_COUNTY<- as.character(data7$A_COUNTY)

data8 <- data7 %>%
  filter(Freq > 10)
data8$A_COUNTY <- as.factor(data8$A_COUNTY)
nrow(data8) #48346 observations: works 

#data8$CountyFreq <- as.factor(data8$CountyFreq) #since CountyFreq is actually counties 

##########Logistic Regression here 

#delete all the remaining variable labels listed here: 
#CountyFreq: is the specification of the county by label 
#FATALS: records number of fatal people in the class 
#Include economic county level variables here?

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
                        P_DOA + V_DEFORMED, id = (county), 
                         data = train, corstr = "independence"))

fitted.results <- predict(model,newdata=test) #outputs probabilities 
fitted.results <- ifelse(fitted.results > 0.5,1,0) #if the prob is greater than 0.5, assign it to be 1
misClasificError <- mean(fitted.results != test$V_DR_DRINK) #misclassification is when results not equal 
print(paste('Accuracy',1-misClasificError)) #about 83% accuracy 




train$FIPSCode <- as.factor(train$FIPSCode)
train$DriverDrinking <- as.factor(train$DriverDrinking)
logmod <- glm(formula = DriverDrinking ~ Sex + Age + ReportedDrugs + VehicleSpeed + DeathSceneStatus + 
                NumFatalities + PrevSuspensions + PrevDWIConvictions + PrevSpeeding 
              + IncomeToPovRatio + TotalPopulation + WeekdayStatus + DayStatus,
              family=binomial(link='logit'), data = train)









library(ROCR)
p <- predict(model, newdata=test,type="response")
pr <- prediction(p, test$V_DR_DRINK)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #auc is 0.78

#########Random Forest Here 

#Ran 10 models below 
#maybe include weekend vs. weekday and time of day. 
treemod1 <- randomForest(V_DR_DRINK ~ CountyFreq + P_SEX + Age + P_DRUGS + 
                           P_DOA + V_DEFORMED + TravSpeed + A_FATALS + PCrash + 
                           PSuspension + PDWI + PSpeed + C17002_001E, 
                         family=binomial(link='logit'), data = train, ntree = 10, mtry = 4, keeP.forest = TRUE, importance = TRUE)

##QQQQQ: Error: cannot allocate vector of size 663.5 Mb
##QQQQQ: Should I tranform the county ratio in a certain way to account for the count label I created?
##Ignore state level data 

data1 <- importance(treemod1, type = 1) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()
names(data1) <- c("Variable", "PercentIncMSE")
data1 <- data1 %>% 
  arrange(desc(PercentIncMSE))
data1 <- data1 %>% arrange(desc(PercentIncMSE))
data1 <- as.data.frame(data1) #Number of drugs seem the highest along with Age, and death at scene/enroute  
save(data1, file = "importanceChart.Rda")

#########Similar Model accounting for States 
#<- load("PovertyIncomeRatio.Rda")

#Have a variable for pulling population 
#antilogit model of each county 
#Restrict counties to 10 or more observations 
#subset the data only for counties with 10 or more observations 


list1<- levels(data8$CountyFreq)
list2 <- list1[1:218]

foodACS$county <- as.factor(foodACS$county)
filterFood <- foodACS %>%
  filter(county %in% list2)

levels(filterFood$county) #data available for 155 counties only from the USCensus of the 288 counties for which data is available 

#each observation as a county and then aggregate by county
#each observation is an accident 

#Aggregate county for each county 
#response is a peson variable 









#McFadden R^2 index can be used to assess the fit of the logistic regression 
##########MACHINE LEARNING HERE: 
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(P_DOA~., data=small, method="lvq", preProcess="scale", trControl=control)
#model <- glm(P_DOA ~ TravSpeed + V_DR_DRINK + PSuspension + PCrash + PSpeed + PDWI,
#family=binomial(link='logit'),data=small)
importance <- varImp(model, scale = FALSE)
print(importance)

n <- nrow(small)
shuffled <- small[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

#use PCA or factor analysis for feature section 
tree <- rpart( ~ ., train, method = "class") #use all the variables to predict the label 
pred <- predict(tree, test, type = "class") 
conf <- table(test$label, pred)
rpart.plot(tree)
library(partykit)
plot(as.party(tree)) #how to interpret the nodes 


