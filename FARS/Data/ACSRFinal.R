
library(acs)
library(tidyr)
library(dplyr)
library(data.table)


data("fips.state")
data("fips.county")
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
first <- merge(fips.state, fips.county, by = "State") #merge by state here 
first$County.Name <- as.factor(first$County.Name)
countypopT$County.Name <- as.factor(countypopT$County.Name)
FinalPop1 <- merge(first, countypopT, by = c("County.Name", "STATE_NAME"))
