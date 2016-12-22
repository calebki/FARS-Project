#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(choroplethr)
library(choroplethrMaps)
library(mdsr)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(tidyr)
library(randomForest)

data("fips.state")
data("fips.county")
mygeo <- geo.make(state = "*", county = "*")
x <- acs.fetch(endyear=2015, geography = mygeo, 
               table.number = "B01003", 
               key = "17b6e09794a8f4a42664535f0e519179cc06f5a7")
countypop <- as.data.frame(estimate(x))
counties <- rownames(countypop)
rownames(countypop) <- NULL
countypop$COUNTY <- counties
names(countypop) <- c("population", "COUNTY")
countypop <- countypop %>% 
  separate(COUNTY, c("COUNTY", "STATE_NAME"), sep = ", ") %>%
  left_join(fips.state, by = "STATE_NAME") %>%
  select(population, County.Name = COUNTY, STATE_NAME, State = STUSAB) %>%
  left_join(fips.county, by = c("County.Name", "State")) %>%
  mutate(region = State.ANSI * 1000 + County.ANSI) %>%
  filter(State != "PR")

data(state.map)
# load("~/git/STAT495-Group3/FARS/Data/StatesLevelACSData/TotalPopulation.Rda")
load("TotalPopulation.Rda")
names(df10) <- c("STATE", "Population")

accidents <- read.csv("mapsaccident.csv")
codeInfo <- read.csv("GLCounty.csv")
load("logisticmodFinal.Rda")
load("modForest.Rda")
load("FinalData.Rda")
drivers <- FinalData %>% mutate(FIPSCode = readr::parse_number(FIPSCode))

accidents <- head(accidents, nrow(accidents)-1) %>%
  mutate(STATE = readr::parse_number(STATE), COUNTY = readr::parse_number(COUNTY),
         LONGITUD = readr::parse_number(LONGITUD), LATITUDE = readr::parse_number(LATITUDE),
         FATALS = readr::parse_number(FATALS), PERSONS = readr::parse_number(PERSONS),
         DRUNK_DR = readr::parse_number(DRUNK_DR), DAY_WEEK = readr::parse_number(DAY_WEEK),
         MONTH = readr::parse_number(MONTH), ROUTE = readr::parse_number(ROUTE),
         HOUR = readr::parse_number(HOUR), ST_CASE = readr::parse_number(ST_CASE),
         VE_TOTA = readr::parse_number(VE_TOTA))

addzero <- function(a) {
  return(ifelse(a < 10, paste(c("0",a), collapse = ""), a))
}

shinyServer(function(input, output) {
    
  sampleData <- reactive({
    temp <- accidents
    sample_n(tbl = temp, size = input$size)
  })
  
  output$markermap <- renderLeaflet({
    leaflet(data = sampleData()) %>%
      addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = -96.9018, lat = 38.4925, zoom = 4)
  })
  
  observe({
    colorBy <- input$color
    sizeBy <- input$pointsize
    
    if (colorBy == "DRUNK_DR") {
      colorData <- ifelse(sampleData()$DRUNK_DR >= 1, "Yes", "No")
      lab <- "Drunk Driving?"
      pal <- colorFactor("Set3", colorData)
    } 
    
    else if(colorBy == "DAY_WEEK") {
      colorData <- ifelse(sampleData()$DAY_WEEK == 7 | sampleData()$DAY_WEEK == 1, "Yes", "No")
      lab <- "Weekend?"
      pal <- colorFactor("Set3", colorData)
    }
    
    else {
      colorData <- ifelse(sampleData()$HOUR > 23, "Unknown", 
                          ifelse(sampleData()$HOUR >= 18 | sampleData()$HOUR < 6, "Night", "Day"))
      lab <- "Night or Day?"
      pal <- colorFactor("Dark2", colorData)
    }
    
    radius <- sampleData()[[sizeBy]] / max(sampleData()[[sizeBy]]) * 100000
    
    leafletProxy("markermap", data = sampleData()) %>%
      clearShapes() %>%
      addCircles(lng = ~LONGITUD, lat = ~LATITUDE, radius = radius, layerId = ~ST_CASE,
                 stroke=FALSE, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=lab,
                layerId="colorLegend")
    
  })
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("markermap") %>% clearPopups()
    event <- input$markermap_shape_click
    if (is.null(event))
      return()
    print(event)
    isolate({
      selected <- sampleData()[sampleData()$ST_CASE == event$id,]
      content <- as.character(tagList(
        tags$h4("Crash ID:", as.integer(selected$ST_CASE)),
        tags$br(),
        sprintf("Drunk driving involved?: %s", ifelse(selected$DRUNK_DR == 1, "Yes", "No")), tags$br(),
        sprintf("Number of vehicles involved: %s", as.integer(selected$VE_TOTA)), tags$br(),
        sprintf("Number of people involved: %s", as.integer(selected$PERSONS)), tags$br(),
        sprintf("Number of fatalities: %s", as.integer(selected$FATALS))
      ))
      leafletProxy("markermap") %>% addPopups(selected$LONGITUD, selected$LATITUDE, content, layerId = selected$ST_CASE)
    })
  })
  
  #Choropleth Map ====================================
  
  maptype <- reactive({
    input$state_or_county
  })
  
  counttype <- reactive({
    input$raw_or_normal
  })
  
  alcohol <- reactive({
    input$drink_or_total
  })
  
  wknd <- reactive({
    input$weekend_or_not
  })
  
  night <- reactive({
    input$night_or_day
  })
  
  statezoom <- reactive({
    input$zoom
  })
    
  output$cmap <- renderPlot({
    
    temp <- accidents
    
    if(alcohol() == "drink") {
      temp <- temp %>% filter(DRUNK_DR >= 1)
    }
    
    if(wknd() == "weekend") {
      temp <- temp %>% filter(DAY_WEEK == 7 | DAY_WEEK == 1)
    }
    
    else if(wknd() == "weekday") {
      temp <- temp %>% filter(DAY_WEEK > 1 & DAY_WEEK < 7)
    }
    
    if(night() == "night") {
      temp <- temp %>% filter(HOUR != 99) %>%
        filter(HOUR >= 18 | HOUR < 6)
    }
    
    else if(night() == "day") {
      temp <- temp %>% filter(HOUR != 99) %>%
        filter(HOUR >= 6 & HOUR < 18)
    }
    
    if(statezoom() == "No zoom") {
      z <- NULL
    }
    
    else{
      z <- statezoom()
    }
    
    if(maptype() == "county") {
      temp <- temp %>% mutate(region = STATE * 1000 + COUNTY)
      numAccidents <- temp %>% group_by(region) %>% dplyr::summarize(value = n())
      
      if(counttype() == "normal") {
        numAccidents <- numAccidents %>% left_join(countypop, by = "region") %>%
          mutate(value = value / (population/100000))
      }
      choro = CountyChoropleth$new(numAccidents)
      choro$ggplot_scale = scale_fill_brewer(name="Number of Crashes", palette=2, drop=FALSE, na.value="gray71")
      choro$set_zoom(z)
      choro$render()
    }
    
    else {
      
      numAccidents <- temp %>% group_by(STATE) %>% 
        dplyr::summarize(value = n())
      temp2 <- as.factor(sapply(numAccidents$STATE, addzero))
      numAccidents$STATE <- temp2
      numAccidents <- state.map %>% 
        select(STATE, region) %>%
        unique() %>% 
        right_join(numAccidents, by = "STATE")
      
      if(counttype() == "normal") {
        numAccidents <- numAccidents %>% left_join(df10, by = "STATE") %>%
          mutate(value = value / (Population/100000))
      }
      
      state_choropleth(numAccidents, zoom = z, legend = "Number of Crashes")
    }

  })
  
  # Second choropleth map ======================================
  
  predVal <- predict.glm(logmodnew, newdata = drivers, type = "response")
  predVal2 <- predict(modForest, newdata = drivers,type = "prob")[,2]
  drivers <- cbind(drivers, predVal, predVal2)
  
  modtype <- reactive({
    input$reg_or_forest
  })
  
  displaytype <- reactive({
    input$reg_or_diff
  })
  
  statezoom2 <- reactive({
    input$zoom2
  })
  
  actual <- drivers %>% 
    select(region = FIPSCode, DriverDrinking) %>%  
    filter(DriverDrinking == 1) %>% 
    group_by(region) %>% 
    dplyr::summarize(value = n())
  
  output$cmap2 <- renderPlot({
    
    if(statezoom2() == "No zoom") {
      z <- NULL
    }
    
    else{
      z <- statezoom2()
    }
    
    if(modtype() == "regresssion") {
      expected <- drivers %>%
        select(region = FIPSCode, predVal) %>%
        group_by(region) %>%
        dplyr::summarize(value = sum(predVal))
    }
    
    else {
      expected <- drivers %>%
        select(region = FIPSCode, predVal2) %>%
        group_by(region) %>%
        dplyr::summarize(value = sum(predVal2))
    }
    
    if(displaytype() == "actual") {
      choro = CountyChoropleth$new(actual)
      choro$title = "Actual Number of Drunk Drivers"
      choro$ggplot_scale = scale_fill_brewer(name="Number of Drivers", palette=2, drop=FALSE, na.value="gray71") 
      choro$set_zoom(z)
      choro$render() 
    }
      
    else if(displaytype() == "expected") {
      choro = CountyChoropleth$new(expected)
      choro$title = "Expected Number of Drunk Drivers"
      choro$ggplot_scale = scale_fill_brewer(name="Number of Drivers", palette = 2, drop=FALSE, na.value="gray71") 
      choro$set_zoom(z)
      choro$render() 
    }
    
    else {
      difference <- left_join(actual, expected, by = "region") %>% 
        mutate(value = value.x - value.y)
      
      if(displaytype() == "difference2") {
        difference <- difference %>% mutate(value = value/value.x)
        choro = CountyChoropleth$new(difference)
        choro$title = "(Actual - Expected)/Actual Number of Drunk Drivers"
        choro$ggplot_scale = scale_fill_brewer(name="Number of Drivers", palette = "PRGn", drop=FALSE, na.value="gray71")
        choro$set_zoom(z)
        choro$render()
      }
      
      else {
        choro = CountyChoropleth$new(difference)
        choro$title = "Actual - Expected Number of Drunk Drivers"
        choro$ggplot_scale = scale_fill_brewer(name="Number of Drivers", palette = "PRGn", drop=FALSE, na.value="gray71")
        choro$set_zoom(z)
        choro$render()
      }

    }
    
  })
    
})
