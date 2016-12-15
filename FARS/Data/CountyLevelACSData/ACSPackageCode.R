
library(acs)
library(tibble)

#Rejoining the state and county datasets to create a collective dataframe 
fips.county <- rename(fips.county, c("State" = "STATEL"))
fips.state <- rename(fips.state, c("STUSAB" = "STATEL"))
fips.county$STATEL <- as.factor(fips.county$STATEL)
fips.state$STATEL <- as.factor(fips.state$STATEL)
join <- inner_join(fips.state, fips.county, by = "STATEL")
#State.ANSI is the state code 
#County.ANSI is the county fips code 
fipsEncode <- join %>% mutate(FIPSCode = ((1000*State.ANSI) + County.ANSI))
fipsEncode1 <- fipsEncode %>% mutate(StateCounty = paste(County.Name, STATE_NAME, sep = ", "))

#TOTAL POPULATION 
x <- geo.make(state = "*", county = "*")
y <- acs.fetch(endyear=2015, geography=x, table.number="B01003", 
               key = "17b6e09794a8f4a42664535f0e519179cc06f5a7")
z <- estimate(y)
zdata <- as.data.frame(z)
dfPop <- tibble::rownames_to_column(zdata, "StateCounty")
JoinPop <- inner_join(dfPop, fipsEncode1, by = "StateCounty")


#POPULATION BY GENDER 
#B01001_002E (male), B01001_026E (female)
x5 <- geo.make(state = "*", county = "*")
y5 <- acs.fetch(endyear=2015, geography=x5, table.number="B01001", 
                key = "17b6e09794a8f4a42664535f0e519179cc06f5a7")
z5 <- estimate(y5)
z5 <- as.data.frame(z5)
z6 <- z5 %>% dplyr::select(B01001_002, B01001_026)
dfGend <- tibble::rownames_to_column(z6, "StateCounty")
JoinPopSex <- inner_join(dfGend, JoinPop, by = "StateCounty")

#INCOME TO POP RATIO  
x1 <- geo.make(state = "*", county = "*")
y1 <- acs.fetch(endyear=2015, geography=x1, table.number="C17002", key = "17b6e09794a8f4a42664535f0e519179cc06f5a7")
z1 <- estimate(y1)
z1 <- as.data.frame(z1)
z2 <- z1 %>% dplyr::select(C17002_001)
dfIncome <- tibble::rownames_to_column(z2, "StateCounty")
JoinPopIncome <- inner_join(dfIncome, JoinPopSex, by = "StateCounty")


#Health insurance coverage status by sex 
x8 <- geo.make(state = "*", county = "*")
y8 <- acs.fetch(endyear=2015, geography=x8, table.number="B27001", key = "17b6e09794a8f4a42664535f0e519179cc06f5a7")
z8 <- estimate(y8)
z8 <- as.data.frame(z8)
#select these: B27001_001 (total), B27001_002 (males), B27001_030E (females)
z3 <- z8 %>% dplyr::select(B27001_001, B27001_002, B27001_030)
dfInsur <- tibble::rownames_to_column(z3, "StateCounty")
dfTotData <- inner_join(dfInsur, JoinPopIncome, by = "StateCounty")


save(dfTotData, file = "dfTotData.csv")


##QQQ. Why does model return null values for economic predictors?