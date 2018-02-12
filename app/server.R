#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#load the packages
library("varhandle")
library("gplots")
library("plyr")
library("dplyr")
library("reshape2")
library("geosphere")
library("threejs")
library("rworldmap")
library("leaflet")
library("rgeos")
library("raster")
library("DT")
library("ggplot2")
library("sp")
library("ggmap")
library("knitr")
library("rglwidget")
library("rgl")
library("maptools")
library("shiny")
library("googleVis")
library("plotly")
library("grid")
library("gtable")
library("treemap")
library("RColorBrewer")
library(shiny)
library("ggmap")
library("ggplot2")

data <- read.csv("state_M2016.csv",header = TRUE,as.is = FALSE)
state.coordinate <- read.csv("state_coordinate.csv",header = TRUE,as.is = FALSE)







server<- function(input, output){
  
  ##Introduction
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Our project looks into the trade of coffee, tea, chocolate, cocoa and spices<br/>
         between the United States and the rest of the world<br/><br/><br/><br/>Group 11: Ruxue, Xiaowo, Rapha??l, Bowen, Terry")
  })
  
  
  ## Part 2

  ## 2D map
  output$usmap <- renderLeaflet({
   
    ## subset the data
    #US = data.frame(Country = "US",longitude = -95.71289,latitude = 37.09024)
    ##### subset dataframe
    tmp = data
    tmp = merge(tmp,state.coordinate, by = "STATE", all.x=T)
    tmp = subset(tmp,OCC_TITLE == as.character(input$major))
    tmp = tmp[,c("ST","STATE","OCC_TITLE","OCC_GROUP","A_MEAN","lon","lat")]
    tmp$A_MEAN = unfactor(tmp$A_MEAN)
    
    SetColor <- function(tmp) {
      sapply(tmp$A_MEAN, function(wage) {
        if(wage <= 30000) {
          "lightgray"
        } else if(wage <= 40000) {
          "lightblue"
        } else if(wage <= 50000) {
          "blue"
        } else if(wage <= 70000) {
          "darkblue"
        } else {
          "black"
        } })
    }
   
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'white',
      library = 'ion',
      markerColor = paste(SetColor(tmp))
    )
    
   # leaflet(data) %>% addTiles() %>%
    #  addAwesomeMarkers(~lon, ~lat, icon=icons, label=~as.character(A_MEAN))
    
    
    
    #index = match(input$major,c(major))
    Colors = c("lightgray","lightblue","blue","darkblue","black")
    Labels = c("<= 30000","<= 40000","<= 50000","<= 70000","> 70000")
    ##### end subset      
    leaflet(tmp)%>%addProviderTiles("Esri.WorldStreetMap")%>%
      addAwesomeMarkers(~lon, ~lat, icon=icons, label=~as.character(A_MEAN))%>%  
      setView(lng=-30,lat=28,zoom=2)%>%#put US in the centre
      addLegend("topright", colors = Colors, labels = Labels,
                title = "Wage Level<br/>From Low to High",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1)
  })
}
  ## end 2D map