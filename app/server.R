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
library("leaflet")
library("shiny")
library("ggmap")
library("ggplot2")
library("maps")
library("RColorBrewer")
library("plotly")
library("DT")
library("scales")
library("dplyr")

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

mapStates = map("state", fill = TRUE, plot = FALSE)


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
    
    tmp<-subset(data,OCC_TITLE == as.character(input$occupation))
    state.shortname <- as.data.frame(substr(mapStates$names, 1, 8))
    colnames(state.shortname) <- "state.shortname"
    tmp$state.shortname <- substr(tolower(tmp$STATE), 1, 8)
    tmp.ordered <- merge(state.shortname, tmp, by="state.shortname", all.x = T)
    tmp.ordered <- cbind(mapStates$names, tmp.ordered)
    
    if(nrow(tmp.ordered)>0){
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
                    layerId = ~tmp.ordered[,1],
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
        addLegend("bottomleft", pal = pal, values = ~tmp.ordered$A_MEAN,
                  title = "Salary Level",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1)
      
    }
    
  })
  
  observeEvent(input$usmap_shape_click, {
    click <- input$usmap_shape_click
    
    tmp<-subset(data,OCC_TITLE == "Management Occupations")
    state.shortname <- as.data.frame(substr(mapStates$names, 1, 8))
    colnames(state.shortname) <- "state.shortname"
    tmp$state.shortname <- substr(tolower(tmp$STATE), 1, 8)
    tmp.ordered <- merge(state.shortname, tmp, by="state.shortname", all.x = T)
    tmp.ordered <- cbind(mapStates$names, tmp.ordered)
    state.name <- tmp.ordered[tmp.ordered[,1] == click$id, "STATE"]

    
    output$state_name <- renderText(state.name)
    
    output$click_gdp_trend<- renderPlotly({
      df <- as.data.frame(t(gdp.aer.rpp)[1:4,])
      colnames(df) <- gdp.aer.rpp[,1]
      df <- df[-1,]
      plot.df <- data.frame(year=2014:2016,gdp=df[,state.name])
      plot_ly(x=plot.df$year,y=plot.df$gdp, type='scatter', mode = 'lines') %>%
        layout(xaxis=list(title="Years",tickfont=list(size=9)),
               yaxis=list(title="GDP",tickfont=list(size=9)))
    })
    
    output$click_amusement_pie<- renderPlotly({
      df <- as.data.frame(t(gdp.aer.rpp)[c(1,7),])
      colnames(df) <- gdp.aer.rpp[,1]
      df <- df[-1,]
      plot.df <- data.frame(year=2016,aer=df[,state.name])
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
  })
  
  output$recommandationtable<-renderDataTable({
    # Data preparation
    tmp<-subset(data,OCC_TITLE == as.character(input$occupation))
    dt.data.1 <- as.data.frame(tmp[,c(5,6,8)])
    aer.rpp_2016 <- gdp.aer.rpp[,c("Area","AER_2016","RPPs_Goods_2016","RPPs_Rents_2016")]
    colnames(aer.rpp_2016) <- c("STATE","AER","RPP_GOODS","RPP_RENTS")
    dt.data.2 <- merge(dt.data.1,aer.rpp_2016,by = "STATE")
    si2 <- si[,c(2,3,6)]
    si2$STATE <- tolower(si2$STATE)
    dt.data.2$STATE <- tolower(dt.data.2$STATE)
    dt.data <- merge(dt.data.2,si2,by = "STATE")
    colname.data <- colnames(dt.data)
    dt.data$AER <- as.character(dt.data$AER)
    dt.data$AER <- as.numeric(substr(dt.data$AER,1,nchar(dt.data$AER)-1))
    dt.data[,3:8] <- apply(dt.data[,3:8], 2, as.numeric)
    dt.data.scale <- dt.data[,1:2]
    dt.data.scale[,3:8] <- apply(dt.data[,3:8], 2, rescale)
    dt.data.scale[,3:8] <- round( dt.data.scale[,3:8],2)
    colnames(dt.data.scale) <- c("STATE","Title","Salary","Recreation Level","RPP Price","RPP Rents","Crime Rate","Cleardays")
    #proper function
    proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
    dt.data.scale$STATE <- proper(dt.data.scale$STATE)
    dt.data.scale[,c(5,6,7)] <- (dt.data.scale[,c(5,6,7)])*(-1)
    
    # Assign the input value
    select1<-as.character(input$firstpreference)
    select2<-as.character(input$secondpreference)
    select3<-as.character(input$thirdpreference)
    
    # Rank calculation 
    dt.data.scale<-as.data.frame(dt.data.scale)
    dt.data.scale$score<- 0.5*dt.data.scale[,select1]+0.3*dt.data.scale[,select2]+0.2*dt.data.scale[,select3]
    order.score<-order(-dt.data.scale$score)
    dt.data.scale$TotalRank[order.score] <- 1:nrow(dt.data.scale)
    
    #sort table 
    dt.data.scale<-dt.data.scale[order(dt.data.scale$TotalRank),]
    
    dt.data.scale<-dt.data.scale %>%
      dplyr::select(TotalRank,"STATE","Title",select1,select2,select3,everything()) %>%
      dplyr::select(-score)
    
  })
  
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
  
  options(warn = -1)  # for ignoring the incompatibility among ggplot2, plotly and shiny widget IDs.
  
}


