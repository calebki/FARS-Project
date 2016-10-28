gdelt <- read.csv("http://data.gdeltproject.org/analytics_user/20161027171002.23057.events.csv")
USDat <- subset(gdelt, Actor1Geo_CountryCode == "US")
