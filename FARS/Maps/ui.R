#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(mdsr)
library(leaflet)
library(choroplethrMaps)

data("state.regions")
accidents <- read.csv("~/git/STAT495-Group3/FARS/Maps/mapsaccident.csv")

vars <- c(
  "Drunk driving?" = "DRUNK_DR",
  "Weekend?" = "DAY_WEEK",
  "Night or Day?" = "TIME"
)

vars2 <- c(
  "Number of Deaths" = "FATALS",
  "Number of People Involved" = "PERSONS"
)

vars3 <- c(
  "County" = "county",
  "State" = "state"
)

vars4 <- c(
  "Raw Count" = "raw",
  "Normalized for Population (Accidents per 100000)" = "normal"
)

vars5 <- c(
  "All Accidents" = "all",
  "Only Drinking Involved Accidents" = "drink"
)

vars6 <- c(
  "All" = "all",  
  "Weekend" = "weekend",
  "Weekday" = "weekday"
)

vars7 <- c(
  "All" = "all",
  "Night" = "night",
  "Day" = "day"
)

shinyUI(navbarPage("FARS", id = "nav",
  
  tabPanel("Marker Map",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "size", label = "Number of Accidents", min = 0, max = nrow(accidents) - 1, value = nrow(accidents) - 1),
    
        selectInput(inputId = "color", "Color", vars),
    
        selectInput(inputId = "pointsize", "Size", vars2)
      ),
    
      mainPanel(  
        titlePanel("Interactive Map"),
        
        leafletOutput("markermap")
      )
    )
  ),
  
  tabPanel("Choropleth Map",
      
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "state_or_county", "Map Type", vars3),
    
        radioButtons(inputId = "raw_or_normal", "Counting Statistic", vars4),
    
        radioButtons(inputId = "drink_or_total", "Alcohol?", vars5),
    
        radioButtons(inputId = "weekend_or_not", "Weekend?", vars6),
    
        radioButtons(inputId = "night_or_day", "Time", vars7),
        
        selectInput(inputId = "zoom", "Zoom", c("No zoom", state.regions$region))
      ),
      
      mainPanel(
        titlePanel("Choropleth Map"),
        
        plotOutput("cmap")
      )
    )
  )
))