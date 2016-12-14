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

shinyUI(navbarPage("FARS", id = "nav",
  
  tabPanel("Marker Map",
  # Application title
    titlePanel("Interactive Map"),
  
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "size", label = "Number of Accidents", min = 0, max = nrow(accidents) - 1, value = 10000),
    
        selectInput(inputId = "color", "Color", vars),
    
        selectInput(inputId = "pointsize", "Size", vars2)
      ),
    
      mainPanel(  
        leafletOutput("markermap")
      )
    )
  ),
  
  tabPanel("Choropleth Map",
    
    radioButtons(inputId = "state_or_county", NULL, vars3),
    
    radioButtons(inputId = "raw_or_normal", NULL, vars4),
    
    radioButtons(inputId = "drink_or_total", NULL, vars5),
    
    titlePanel("Choropleth Map"),
    
    plotOutput("cmap")
    
  ),
  
  tabPanel("Data Exploration")
))