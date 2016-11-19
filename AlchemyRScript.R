library(dplyr)
#library(vRODBC)
#library(caret)
library(parallel)
library(rvest)
library(tm)

library(SnowballC)
library(httr)
library(reshape2)
library(dplyr)

#read in the csv file
Top30Sites <- read.csv("TopSitesPast30.csv")


#code for taxonomy extraction
results = lapply(Top30Sites$Site[1:3], function(x){
  baseurl = 'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedTaxonomy?apikey=61eebcea6c0dc3b63544a80d8416cf9e515460a7&outputMode=json&url=http://'
  keywords = content(GET(paste0(baseurl, as.character(x))))
  df = try(data.frame(matrix(unlist(keywords$keywords), nrow = length(keywords$keywords), ncol = 3, byrow = T), site=as.character(x)))
  if(class(df)!="try-error"){
    return(df) 
  }else{
    return(data.frame(X1=0,X2="keyword",X3 = "confidence", site=as.character(x)), is.null)
  }
  
})


#code for keyword extraction
results = lapply(Top30Sites$Site[1:3], function(x){
  baseurl = 'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedTaxonomy?apikey=61eebcea6c0dc3b63544a80d8416cf9e515460a7&outputMode=json&url=http://'
  keywords = content(GET(paste0(baseurl, as.character(x))))
  df = try(data.frame(matrix(unlist(keywords$keywords), nrow = length(keywords$keywords), ncol = 2, byrow = T), site=as.character(x)))
  if(class(df)!="try-error"){
    return(df) 
  }else{
    return(data.frame(X1=0,X2="keyword", site=as.character(x)), is.null)
  }
  
})







baseurl = 'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedKeywords?apikey=61eebcea6c0dc3b63544a80d8416cf9e515460a7&outputMode=json&url=http://'
keywords = content(GET(paste0(baseurl, as.character('msn.com'))))

#for taxonomy data
baseurl = 'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedTaxonomy?apikey=61eebcea6c0dc3b63544a80d8416cf9e515460a7&outputMode=json&url=http://'
keywords1 = content(GET(paste0(baseurl, as.character('msn.com'))))
keywords1
df = try(data.frame(matrix(unlist(keywords1$taxonomy), nrow = length(keywords1$taxonomy), ncol = 7, byrow = T), site=as.character('msn.com')))

df = try(data.frame(matrix(unlist(keywords1$taxonomy), nrow = length(keywords1$taxonomy), ncol = 4, byrow = T), site=as.character('msn.com')))







baseurl = 'http://gateway-a.watsonplatform.net/calls/url/URLGetRankedKeywords?apikey=61eebcea6c0dc3b63544a80d8416cf9e515460a7&outputMode=json&url=http://'
keywords = content(GET(paste0(baseurl, as.character('msn.com'))))
df = try(data.frame(matrix(unlist(keywords$keywords), nrow = length(keywords$keywords), ncol = 7, byrow = T), site=as.character('msn.com')))
#df
#Code for reading the above file as a table in R
library(data.table)
newresults<-rbindlist(results, fill=TRUE) #unbinds the lists as data frame
newresults <- transform(newresults,Date=as.Date("2016-06-28")) #add a date column
colnames(newresults)[2] <- "Keyword" #changes the row names to Keyword
colnames(newresults)[1] <- "RelevanceScore"#changes the row name to Relevance Score
names(newresults)

write.table(newresults, "SiteAlchemyResults.txt", sep="\t") #write table in a created file with sep created
write.csv(newresults, file = "SiteAlchemyResults.csv")

library(xlsx)
write.xlsx(newresults, "SiteAlchemyResults.csv")


#extract taxonomy
