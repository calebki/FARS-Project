###########creating tables for extracting county level data 
CList = paste(States, County, sep="_") #concatenate States and Counties together
CList <- CList[1:20]
df11 <- NULL
for (cty in CList){
  #print(cty)
  split <- strsplit(cty,'_',fixed=TRUE)
  state <- split[[1]][1]
  county <- split[[1]][2]
  region = paste("for=county:",county,"&in=state:",state,sep = '')
  temp.df <- getCensusApi(sf1_2010_api, vars=vars10, region=region, key=key)
  #print(temp.df)
  df11 <- rbind(df11, temp.df)
}


name <- strsplit(CList1,'_',fixed=TRUE)
name[[1]][1]
name[[1]][2]
# Create an empty data.frame to hold the results in:
df11 <- NULL
for(cty in St){
  print(cty)
  region = paste("for=county:",county,"&in=state:",state,sep = '')
  # Pull data
  temp.df <- getCensusApi(sf1_2010_api, vars=vars11, region=region, key=key)
  df11 <- rbind(df11, temp.df)
}
