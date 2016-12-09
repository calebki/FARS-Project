
#Age, gender, average distance to work and other related to statistics.
#population density 

Accident <- read.csv("AccidentGeo.csv")
States <- Accident$A_STATE

data <- acs.fetch(geography=psrc, table.name="MARITAL STATUS",
                  endyear=2014, col.names="pretty")
data2 <- as.data.frame((estimate(data))) #function to extract just the estimates 
data3 <- cbind(data2, county)

######################################################
getCensusApi <- function(data_url,vars,region,key,numeric=TRUE){
  if(length(vars)>50){
    vars <- vecToChunk(vars) # Split vars into a list
    get <- lapply(vars, function(x) paste(x, sep='', collapse=","))
    data <- lapply(vars, function(x) getCensusApi2(data_url,x,region,key, numeric=TRUE))
  } else {
    get <- paste(vars, sep='', collapse=',')
    data <- list(getCensusApi2(data_url,get,region,key,numeric=TRUE))
  }
  if(all(sapply(data, is.data.frame))){
    colnames <- unlist(lapply(data, names))
    data <- do.call(cbind,data)
    #print(data)
    names(data) <- colnames
    # Prettify the output
    # If there are nonunique colums, remove them
    data <- data[,unique(colnames, fromLast=TRUE)]
    # Reorder columns so that numeric fields follow non-numeric fields
    data <- data[,c(which(sapply(data, class)!='numeric'), which(sapply(data, class)=='numeric'))]
    return(data)
  }else{
    print('unable to create single data.frame in getCensusApi')
    return(data)
  }
}


getCensusApi2 <- function(data_url,get,region,key,numeric=TRUE){
  if(length(get)>1) get <- paste(get, collapse=',', sep='')
  api_call <- paste(data_url, 
                    'get=', get, 
                    '&', region,
                    '&key=', key,
                    sep='')
  #print(api_call)
  
  dat_raw <- try(readLines(api_call, warn="F"))
  #print(dat_raw)
  if(class(dat_raw)=='try-error') {
    print(api_call)
    return}
  dat_df <- data.frame()
  tmp <- strsplit(gsub("[^[:alnum:], _]", '', dat_raw), "\\,")
  dat_df <- as.data.frame(do.call(rbind, tmp[-1]), stringsAsFactors=FALSE)
  #print(dat_df)
  names(dat_df) <- tmp[[1]]
  if(numeric==TRUE){
    value_cols <- grep("[0-9]", names(dat_df), value=TRUE)
    for(col in value_cols) dat_df[,col] <- as.numeric(as.character(dat_df[,col]))
  }
  return(dat_df)
}

vecToChunk <- function(x, max=50){
  s <- seq_along(x)
  x1 <- split(x, ceiling(s/max))
  return(x1)
}

key = '3dbabece4401ad72afa36a118e2cf777efa2afb3'
sf1_2010_api <- 'http://api.census.gov/data/2015/acs1?'
daa3 <- getCensusApi(sf1_2010_api, vars = c("B00001_001E"),region="for=place:*&",key = key)

study_area <- data.frame(county = c('Cannon', 'Cheatham', 'Davidson', 'Dickson', 'Hickman', 'Macon', 'Maury', 
                                    'Robertson', 'Rutherford', 'Smith', 'Sumner', 'Trousdale', 'Williamson', 'Wilson'),
                         fips = c('015', '021', '037', '043', '081', '111', '119', 
                                  '147', '149', '159', '165', '169', '187', '189'),
                         stringsAsFactors=FALSE)

#Adjust this script to pull data for all counties
#Adjust this script to pull data for number of variables (found for variables)
vars1 <- paste('B01001_', sprintf('%03i', seq(1, 49)), 'E', sep='') #sex by age
vars2 <- paste('C17001_', sprintf('%03i', seq(1, 19)), 'E', sep='') #poverty status by sex, by age
vars3 <- paste('C22001_', sprintf('%03i', seq(1, 3)), 'E', sep='') #food stamp receipient 
vars5 <- paste('C27001_', sprintf('%03i', seq(1, 21)), 'E', sep='') #health insurance coverage status
vars6 <- paste('C17002_', sprintf('%03i', seq(1, 1)), 'E', sep='') #poverty to ratio income 
vars7 <- paste('C17003_', sprintf('%03i', seq(1, 11)), 'E', sep='') #poverty status by education  
vars8 <- paste('C17017_', sprintf('%03i', seq(1, 19)), 'E', sep='') #poverty status by household type
vars9 <- paste('C17022_', sprintf('%03i', seq(1, 21)), 'E', sep='') #ratio of income to poverty level of families 
vars10 <- paste('B01003_', sprintf('%03i', seq(1, 1)), 'E', sep='') #total population 
vars11 <- paste('B02001_', sprintf('%03i', seq(1, 10)), 'E', sep='') #Race 

States1<- unique(States)
length(States1)
St <- States1[1:51]

# Create an empty data.frame to hold the results in:
df7 <- NULL
for(cty in St){
  region = paste("for=state:", cty, sep='')
  # Pull data
  temp.df <- getCensusApi(sf1_2010_api, vars=vars7, region=region, key=key)
  df7 <- rbind(df7, temp.df)
}

save(df7, file="PovertyByEducation.Rda")






load("SexByAge.Rda")

#B01003_001E #total population
#B02001_001E #race
#B08013_001E #aggregate travel time to work
#B08013_002E #aggregate travel time to work for males
#B08013_003E #aggregate travel time to work for females
#C17001_001E to C17001_019E #poverty status in the past 12 months by sex, by age
#sex by age by employment status 
#C22001_001E to C22001_003E #Receipient of food stamps/stamps in the past 12 months for households
#C23001_001E to C23001_093E#Sex by Age by Employment Status for the Population 16 Years and Over
#C27001_001E to C27001_021M #Health Insurance Coverage Status by Sex by Age 
#C17002_001E to C17002_001E #Ratio of Income to Poverty Level in the Past 12 Months
#C17003_001E to C17003_011M #POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY EDUCATIONAL ATTAINMENT
#C17017_001E to C17017_019M #POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE
#C17022_001E to C17022_021M #Ratio of Income to Poverty Level of Families by Family Type
#vars <- paste('B02003_', sprintf('%03i', seq(1, 49)), 'E', sep='')
#us_places_ancestry <- getCensusApi(sf1_2010_api, vars=c("B01001_035E"), region="for=state:06",key=key)
