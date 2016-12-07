gdelt <- read.csv("http://data.gdeltproject.org/analytics_user/20161027171002.23057.events.csv")
USDat <- subset(gdelt, Actor1Geo_CountryCode == "US")
Links <- USDat %>%
  select(SOURCEURL) %>%
  head(20)

#keyword analysis 
d2 = data.frame()
d3 = data.frame()
for (r in Links$SOURCEURL){
  baseurl = 'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedKeywords?apikey=36569f3cb7097514fa2a5532fe4e7d05c946e2ba&outputMode=json&url=http://'
  keywords = content(GET(paste0(baseurl, as.character(r))))
  for (i in 1:length(keywords$keywords)){
    dat3 = data.frame(text = unlist(keywords$keywords[[i]]$text), 
                      relevance = unlist(keywords$keywords[[i]]$relevance))
    site2 <- rep(r, length(dat3))
    dat5 <- cbind(site2, dat3)
    d2 <- rbind(d2, dat5)
  }
  d3 <- rbind(d2, d3)
}

#taxonomy analysis
d2 = data.frame()
d3 = data.frame()
for (r in Links$SOURCEURL){
  baseurl = 'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedTaxonomy?apikey=36569f3cb7097514fa2a5532fe4e7d05c946e2ba&outputMode=json&url=http://'
  keywords = content(GET(paste0(baseurl, as.character(r))))
  for (i in 1:length(keywords$taxonomy)){
    dat3 = data.frame(text = unlist(keywords$taxonomy[[i]]$label), 
                      relevance = unlist(keywords$taxonomy[[i]]$score))
    site2 <- rep(r, length(dat3))
    dat5 <- cbind(site2, dat3)
    d2 <- rbind(d2, dat5)
  }
  d3 <- rbind(d2, d3)
}



