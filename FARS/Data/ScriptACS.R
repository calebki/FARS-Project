Accident <- read.csv("AccidentGeo.csv")
states <- Accident$A_STATE
county <- Accident$A_COUNTY

#Accident[c(1:10),] #selects first 10 observations from accident dataset 

state1 = county[1:10] #select 10 counties 
psrc=geo.make(state=54, county=73)


data <- acs.fetch(geography=psrc, table.name="MARITAL STATUS",
                  endyear=2014, col.names="pretty")
data2 <- as.data.frame((estimate(data))) #function to extract just the estimates 
data3 <- cbind(data2, county)
#data2 <- geography(data)


