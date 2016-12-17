
library(acs)
library(tidyr)
library(dplyr)
library(data.table)


data("fips.state")
data("fips.county")

######Analysis for population 
mygeo <- geo.make(state = "*", county = "*")
x <- acs.fetch(endyear=2015, geography = mygeo, 
               table.number = "B01003", 
               key = "3dbabece4401ad72afa36a118e2cf777efa2afb3")
countypop <- as.data.frame(estimate(x))
countypopT <- setDT(countypop, keep.rownames = TRUE)[]
countypopT <- rename(countypopT, StateCounty = rn)
countypopT$StateCounty <- as.factor(countypopT$StateCounty)
levels(countypopT$StateCounty)[levels(countypopT$StateCounty)=="Doña Ana County, New Mexico"] <- "Dona Ana County, New Mexico"

countypopT <- countypopT %>%
  separate(StateCounty, c("County.Name", "STATE_NAME"), ", ") #separator by comma and space 
countypopT$STATE_NAME <- as.factor(countypopT$STATE_NAME)
fips.state$STATE_NAME <- as.factor(fips.state$STATE_NAME)
#merge county and state data 
fips.state <- rename(fips.state, State = STUSAB)
first <- merge(fips.state, fips.county, by = "State") #first merge state and county by state 
first$County.Name <- as.factor(first$County.Name)
countypopT$County.Name <- as.factor(countypopT$County.Name)
FinalPop1 <- merge(first, countypopT, by = c("County.Name", "STATE_NAME"))
FinalPop1 <- FinalPop1 %>% mutate(FIPSCode = ((1000*State.ANSI) + County.ANSI)) #find fips encoding


#####Analysis for Poverty to Income ratio 
mygeo <- geo.make(state = "*", county = "*")
x <- acs.fetch(endyear=2015, geography = mygeo, 
               table.number = "C17002", 
               key = "3dbabece4401ad72afa36a118e2cf777efa2afb3")

incomepop <- as.data.frame(estimate(x))
incomepop <- incomepop %>% dplyr::select(C17002_001)
incomepopT <- setDT(incomepop, keep.rownames = TRUE)[]
incomepopT <- rename(incomepopT, StateCounty = rn)
incomepopT$StateCounty <- as.factor(incomepopT$StateCounty)
levels(incomepopT$StateCounty)[levels(incomepopT$StateCounty)=="Doña Ana County, New Mexico"] <- "Dona Ana County, New Mexico"

incomepopT <- incomepopT %>%
  separate(StateCounty, c("County.Name", "STATE_NAME"), ", ") #separator by comma and space 
incomepopT$STATE_NAME <- as.factor(incomepopT$STATE_NAME)
incomepopT$County.Name <- as.factor(incomepopT$County.Name)
FinalIncom1 <- merge(first,incomepopT, by = c("County.Name", "STATE_NAME"))
FinalIncom1 <- FinalIncom1 %>% mutate(FIPSCode = ((1000*State.ANSI) + County.ANSI)) #find fips encoding


######Now merge FinalIncom1 and FinalPop1 by FipsCode
FinalCensus <- inner_join(FinalIncom1, FinalPop1, by = "FIPSCode")
CensusFinalD1 <- FinalCensus %>% dplyr::select(FIPSCode, C17002_001, B01003_001)
save(CensusFinalD1, file = "CensusFinalD1.Rda")
