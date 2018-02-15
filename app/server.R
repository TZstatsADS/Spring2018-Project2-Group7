#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#load the packages
# install.packages("varhandle")
library("varhandle")
library("leaflet")
library("shiny")
library("ggmap")
library("ggplot2")
library("maps")
library("RColorBrewer")

data <- read.csv("salary2016.csv",header = TRUE,stringsAsFactors = FALSE)



server<- function(input, output){
  
  ##Introduction
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Our project uses statistics provided by the Department of Labor <br/>
         to help with future career choices")
  })
  
  
  ## Part 2
  
  
  ## 2D map
  
  output$usmap <- renderLeaflet({
    
    tmp <- data
    
    tmp<-subset(data,OCC_TITLE == as.character(input$occupation))
    
    if(nrow(tmp)>0){
      
      ###### 
      
      # SetColor <- function(tmp) {
      #   sapply(tmp$A_MEAN, function(wage) {
      #     if(wage <= 30000) {
      #       "lightgray"
      #     } else if(wage <= 40000) {
      #       "lightblue"
      #     } else if(wage <= 50000) {
      #       "blue"
      #     } else if(wage <= 70000) {
      #       "darkblue"
      #     } else {
      #       "black"
      #     } })
      # }
      # 
      # icons <- awesomeIcons(
      #   icon = 'ios-close',
      #   iconColor = 'white',
      #   library = 'ion',
      #   markerColor = paste(SetColor(tmp))
      # )
      # 
      # 
      # 
      # Colors = c("lightgray","lightblue","blue","darkblue","black")
      # Labels = c("<= 30000","<= 40000","<= 50000","<= 70000","> 70000")
      # 
      # leaflet(tmp)%>%addProviderTiles("Esri.WorldStreetMap")%>%
      #   addAwesomeMarkers(~lon, ~lat, icon=icons, label=~as.character(A_MEAN))%>%  
      #   setView(lng=-30,lat=28,zoom=2)%>%#put US in the centre
      #   addLegend("topright", colors = Colors, labels = Labels,
      #             title = "Wage Level<br/>From Low to High",
      #             labFormat = labelFormat(prefix = "$"),
      #             opacity = 1)
      
      pal <- colorNumeric(palette="YlGnBu", domain=tmp$A_MEAN)
      
      mapStates = map("state", fill = TRUE, plot = FALSE)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>$ %g Annual Salary",
        tmp$STATE, tmp$A_MEAN) %>% 
        lapply(htmltools::HTML)
      
      leaflet(data = mapStates) %>% addTiles() %>%
        addPolygons(fillColor = ~pal(tmp$A_MEAN), 
                    weight = 2, 
                    opacity = 1, 
                    color = "white",
                    dashArray = "3", 
                    fillOpacity = 0.7, 
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend("bottomright", pal = pal, values = ~tmp$A_MEAN,
                  title = "Salary Level",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1)
    }
    
  }
  
  )
}
## end 2D map