
data = read.csv("AccidentGeo.csv")
library(dplyr)
library(mlbench)
library(caret)
library(readr)
require(mosaic)


small <- data %>% dplyr::select(V_V_CONFIG, V_TRAV_SP, V_DEFORMED, V_DR_DRINK, V_PREV_ACC, 
                                V_PREV_SUS, V_PREV_DWI, V_PREV_SPD, V_VALIGN, V_VPAVETYP, V_VSURCOND,
                                A_LGT_COND, A_FATALS, A_DRUNK_DR, A_WEATHER, A_WEATHER1, A_WEATHER2, A_WRK_ZONE, A_MAN_COLL,
                                P_AGE, P_SEX, P_INJ_SEV, P_SEAT_POS, P_AIR_BAG, P_EJECTION, P_EJ_PATH, P_DRINKING, P_DRUGS, 
                                P_LAG_HRS, P_DOA, A_COUNTY, A_STATE, A_CITY)


small <- small %>% mutate(TravSpeed = as.numeric(V_TRAV_SP),
                          Age = as.numeric(P_AGE),
                          PDWI = as.numeric(V_PREV_DWI),
                          PSuspension = as.numeric(V_PREV_SUS),
                          PCrash = as.numeric(V_PREV_ACC),
                          PSpeed = as.numeric(V_PREV_SPD))

#small %>% group_by(A_COUNTY)
#####DATA WRANGLE: find number of tallies with a range of values 
subData <- small %>% head(100)
tally(small$A_COUNTY)
sum(tally(small$A_COUNTY) < 30) #to find number of tallies above a certain range 
#Create a new predictor CountyLabel so that if the number of counties with number of crashes is less than 30, predictor gets LessThan
#Otherwise predictor gets all actual values

small2 <- small %>%
  mutate(AccountCounty = ifelse(tally(A_COUNTY) < 30, "LessCounty", A_COUNTY))
countiesL <- tally(small$A_COUNTY)
data4 <- data.frame(countiesL)

class(small$A_COUNTY)
tally(small$A_COUNTY)
countiesL <- tally(small$A_COUNTY)
data4 <- data.frame(countiesL)
data5 <- data4[1:288,]
data6 <- rename(data5, c("X" = "A_COUNTY"))
data7 <- merge(data6, small)

#create new variable here 



#######MACHINE LEARNING HERE: 
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


