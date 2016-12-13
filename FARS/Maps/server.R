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

data(state.map)
load("~/git/STAT495-Group3/FARS/Data/StatesLevelACSData/TotalPopulation.Rda")
names(df10) <- c("STATE", "Population")

accidents <- read.csv("~/git/STAT495-Group3/FARS/Maps/mapsaccident.csv")
codeInfo <- read.csv("~/git/STAT495-Group3/FARS/Data/GLCounty.csv")

accidents <- head(accidents, nrow(accidents)-1) %>%
  mutate(STATE = readr::parse_number(STATE), COUNTY = readr::parse_number(COUNTY),
         LONGITUD = readr::parse_number(LONGITUD), LATITUDE = readr::parse_number(LATITUDE),
         FATALS = readr::parse_number(FATALS), PERSONS = readr::parse_number(PERSONS),
         DRUNK_DR = readr::parse_number(DRUNK_DR), DAY_WEEK = readr::parse_number(DAY_WEEK),
         MONTH = readr::parse_number(MONTH), ROUTE = readr::parse_number(ROUTE))

addzero <- function(a) {
  return(ifelse(a < 10, paste(c("0",a), collapse = ""), a))
}

# Define server logic required to draw a histogram
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
      colorData <- ifelse(sampleData()$DRUNK_DR >= 1, "yes", "no")
      pal <- colorFactor("Set3", colorData)
    } 
    
    else {
      colorData <- accidents[[colorBy]]
      pal <- colorFactor("Set3", colorData)
    }
    
    radius <- accidents[[sizeBy]] / max(accidents[[sizeBy]]) * 100000
    
    leafletProxy("markermap", data = sampleData()) %>%
      clearShapes() %>%
      addCircles(lng = ~LONGITUD, lat = ~LATITUDE, radius = radius,
                 stroke=FALSE, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
    
  })
  
  # Show a popup at the given location
  showPopup <- function(id, lat, lng) {
    selected <- accidents[accidents$ST_CASE == id,]
    content <- as.character(tagList(
      tags$h4("Crash ID:", as.integer(selected$ST_CASE)),
      tags$br(),
      sprintf("Drunk driving involved?: %s", ifelse(selected$DRUNK_DR == 1, "Yes", "No")), tags$br(),
      sprintf("Number of people involved: %s%%", as.integer(selected$PERSONS)), tags$br(),
      sprintf("Number of fatalities: %s", as.integer(selected$FATALS))
    ))
    leafletProxy("markermap", data = sampleData()) %>% addPopups(lng, lat, content, layerId = ST_CASE)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("markermap", data = sampleData()) %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$ST_CASE, event$LATITUDE, event$LONGITUD)
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
  
  output$cmap <- renderPlot({
    
    temp <- accidents
    
    if(alcohol() == "drink") {
      temp <- temp %>% filter(DRUNK_DR >= 1)
    }
    
    if(maptype() == "county") {
      temp <- temp %>% mutate(region = STATE * 1000 + COUNTY)
      numAccidents <- temp %>% group_by(region) %>% summarize(value = n())
      county_choropleth(numAccidents)
    }
    
    else {
      
      numAccidents <- temp %>% group_by(STATE) %>% 
        summarize(value = n())
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
      
      state_choropleth(numAccidents)
    }

  })
    
  })
