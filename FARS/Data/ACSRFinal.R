
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
  separate(StateCounty, c("County.Name", "STATE_NAME"), ",")
countypopT$STATE_NAME <- as.factor(countypopT$STATE_NAME)
fips.state$STATE_NAME <- as.factor(fips.state$STATE_NAME)
s1 <- merge(fips.state, countypopT, by = "STATE_NAME")
head(s1)


nrow(countypopT)
nrow(fips.county)



#Merge countypopT with fips.state and fips.county 

