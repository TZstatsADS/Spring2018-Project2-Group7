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
library("plotly")
library("DT")

data <- read.csv("salary2016.csv",header = TRUE,stringsAsFactors = FALSE)
gdp.aer.rpp <- read.csv("GDP_AER_RPP.csv",header = TRUE,stringsAsFactors = FALSE)
national<-read.csv("national.csv",header = T)
data2<-read.csv("state_M2016_2.csv",header = T)

si <- read.csv("State_info.csv", header = T, stringsAsFactors = F)
which_state <- function(mapData, long, lat) {
  mapData$long_diff <- mapData$long - long
  mapData$lat_diff <- mapData$lat - lat
  
  # only compare borders near the clicked point to save computing time
  mapData <- mapData[abs(mapData$long_diff) < 20 & abs(mapData$lat_diff) < 15, ]
  
  # calculate the angle between the vector from this clicked point to border and c(1, 0)
  vLong <- mapData$long_diff
  vLat <- mapData$lat_diff
  mapData$angle <- acos(vLong / sqrt(vLong^2 + vLat^2))
  
  # calculate range of the angle and select the state with largest range
  rangeAngle <- tapply(mapData$angle, mapData$region, function(x) max(x) - min(x))
  return(names(sort(rangeAngle, decreasing = TRUE))[1])
}
usaMap <- map_data("state")
plotMap <- ggplot(usaMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")
plotInfo <- ggplot(si, aes(x = Rent_Price, y = Price_Level)) + 
  geom_point(aes(size = Crime_Rate, color = Cleardays)) 


server<- function(input, output, session){
  
  ##Introduction
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Our project uses statistics provided by the Department of Labor <br/>
         to help with future career choices")
  })
  
  ## Part 1
 
  
  
  
    
    output$Plot <- renderPlotly({
      
      df1<-data.frame(Occupation=rep(national[input$major1,1],each=3),
                      Year=c(2012,2013,2014),
                      Annual_Wage=unlist(national[input$major1,2:4]))
      df2<-data.frame(Occupation=rep(national[input$major2,1],each=3),
                      Year=c(2012,2013,2014),
                      Annual_Wage=unlist(national[input$major2,2:4]))
      df3<-data.frame(Occupation=rep(national[input$major3,1],each=3),
                      Year=c(2012,2013,2014),
                      Annual_Wage=unlist(national[input$major3,2:4]))
      df<-rbind(df1,df2,df3)
      
      p<-ggplot(data=df, aes(x=Year, y=Annual_Wage, fill=Occupation)) +
        geom_bar(stat="identity",position=position_dodge())+
        scale_fill_brewer(palette="Blues")+
        theme_minimal()
      gg<-ggplotly(p)
      layout(gg, dragmode = "pan")
      
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
      
      if (input$state != "All") {
        data2 <- data2[data2$State == input$state,]
      }
      if (input$major != 00) {
        data2 <- data2[substr(data2$OCC_CODE,start = 1,stop = 2) == input$major,]
      }
      
      data2<-data2[,-2]
      data2
    }))
    
  
  
  ## Part 2
  
  
  ## 2D map
  
  output$usmap <- renderLeaflet({
    
    tmp <- data
    tmp<-subset(data,OCC_TITLE == as.character(input$occupation))
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    
    state.shortname <- as.data.frame(substr(mapStates$names, 1, 8))
    colnames(state.shortname) <- "state.shortname"
    tmp$state.shortname <- substr(tolower(tmp$STATE), 1, 8)
    tmp.ordered <- merge(state.shortname, tmp, by="state.shortname", all.x = T)
    
    if(nrow(tmp)>0){
      pal <- colorNumeric(palette="YlGnBu", domain=tmp.ordered$A_MEAN)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>$ %g Annual Salary",
        tmp.ordered$STATE, tmp.ordered$A_MEAN) %>% 
        lapply(htmltools::HTML)
      
      
      leaflet(data = mapStates) %>%
        addProviderTiles("Stamen.TonerLite") %>%  # default map, base layer
        
        addPolygons(fillColor = ~pal(tmp.ordered$A_MEAN),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    layerId = ~tmp.ordered$STATE,
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
        addLegend("bottomleft", pal = pal, values = ~tmp$A_MEAN,
                  title = "Salary Level",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1)
      
    }
    
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
    
    
  })
  
  output$state_name <- renderText(input$state_selection)
  
  output$click_gdp_trend<- renderPlotly({
    df <- as.data.frame(t(gdp.aer.rpp)[1:4,])
    colnames(df) <- gdp.aer.rpp[,1]
    df <- df[-1,]
    plot.df <- data.frame(year=2014:2016,gdp=df[,input$state_selection])
    plot_ly(x=plot.df$year,y=plot.df$gdp, type='scatter', mode = 'lines') %>%
      layout(xaxis=list(title="Years",tickfont=list(size=9)),
             yaxis=list(title="GDP",tickfont=list(size=9)))
  })
  
  output$click_amusement_pie<- renderPlotly({
    df <- as.data.frame(t(gdp.aer.rpp)[c(1,7),])
    colnames(df) <- gdp.aer.rpp[,1]
    df <- df[-1,]
    plot.df <- data.frame(year=2016,aer=df[,input$state_selection])
    plot.df$aer <- as.character(plot.df$aer)
    plot.df$aer <- as.numeric(substr(plot.df$aer,1,nchar(plot.df$aer)-1))
    plot.df$uaer <- 100-plot.df$aer
    plot.df[2,] <- c("YEAR","AER","UAER")
    plot.df <- t(plot.df)
    plot.df <- plot.df[-1,]
    plot.df[,1] <- as.numeric(as.character(plot.df[,1]))/100
    plot.df <- as.data.frame(plot.df)
    plot_ly(data=plot.df,values = ~plot.df[,1],labels = ~plot.df[,2], type='pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #dt.data <- as.data.frame(tmp[,c(3,5,6)])
  #aer.rpp_2016 <- gdp.aer.rpp[,c("Area","AER_2016","RPPs_Goods_2016","RPPs_Rents_2016")]
  #colnames(aer.rpp_2016) <- c("STATE","AER","RPP_GOODS","RPP_RENTS")
  
  # End leaflet
  
  # State info Detail
  output$map <- renderPlot({
    plotMap
    # coord_map(), do not use it. More discussion next section.
  })
  output$info <- renderPlot({
    plotInfo
  })
  
  # plot after click
  observeEvent(input$clickMap, {
    xClick <- input$clickMap$x
    yClick <- input$clickMap$y
    state <- which_state(usaMap, xClick, yClick)
    output$map <- renderPlot(
      plotMap + 
        geom_polygon(data = usaMap[usaMap$region == state,], fill = "yellow") +
        annotate("text", x = xClick, y = yClick, label = state, color = "red")
    )
    output$info <- renderPlot({
      plotInfo +
        geom_point(data = si[tolower(si$STATE) == state,],
                   size = 6, shape = 1, color = "red")
    })
  })
  
  # End State info Detail
  
  
}
## end 2D map