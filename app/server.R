
################################################################
# Load the packages
################################################################

# Packages used
packages.used=c("varhandle", "leaflet", "shiny", 
                "ggmap", "ggplot2", "maps",
                "RColorBrewer", "plotly", "DT", 
                "scales", "dplyr")

# Check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))

# Install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

# Library packages
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

################################################################
# Data Preparation
################################################################

# Extract the data of occupations and salary for the map section
data <- read.csv("salary2016.csv",header = TRUE,stringsAsFactors = FALSE)

# Read the data of GDP, AER and RPP
gdp.aer.rpp <- read.csv("GDP_AER_RPP.csv",header = TRUE,stringsAsFactors = FALSE)

# Extract the data for the Choose Your Occupation section
national<-read.csv("national.csv",header = T)

# Extract the data for the Wage Information section
data2<-read.csv("basic_wage_info.csv",header = T)
si <- read.csv("state_info.csv", header = T, stringsAsFactors = F)

################################################################
# Function Definition
################################################################

# which_state for transfering the coordinates into state name
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

# Proper function for turning the first letter into upper case 
proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

################################################################
# Map Preparation for the Your Recommendation
################################################################ 

# Main map used in the Find Your Location session
mapStates = map("state", fill = TRUE, plot = FALSE)

# Map Preparation for the Your Recommendation
usaMap <- map_data("state")
plotMap <- ggplot(usaMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")
plotInfo <- ggplot(si, aes(x = Rent_Price, y = Price_Level)) + 
  geom_point(aes(size = Crime_Rate, color = Cleardays)) 

################################################################
# Server Section
################################################################

server<- function(input, output, session){
  
  
  
  ### Part 1: Homepage
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Give our professional future career choices for you
      <br/>Using statistics data from the Bureau of Labor Statistics and Bureau of Economic Analysis
         ")
  })
  
  
  
  ### Part 2: Choose Your Occupation
  output$Plot <- renderPlotly({
    # Generate the sub data frame for the visualization
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
    
    # Comparison Plot
    p<-ggplot(data=df, aes(x=Year, y=Annual_Wage, fill=Occupation)) +
      geom_bar(stat="identity",position=position_dodge())+
      scale_fill_brewer(palette="Blues")+
      theme_minimal()
    gg<-ggplotly(p)
    layout(gg, dragmode = "pan")
    
  })
  
  
  
  ### Part 3: Find Your Location
  
  ## Leaflet map
  output$usmap <- renderLeaflet({
    # Extract the data based on the user's choice
    # Subset the data to the occupation that the user has chosen
    tmp<-subset(data,OCC_TITLE == as.character(input$occupation))
    
    # Match the default state name in mapStates to the state name in our data (tmp)
    state.shortname <- as.data.frame(substr(mapStates$names, 1, 8))
    colnames(state.shortname) <- "state.shortname"
    tmp$state.shortname <- substr(tolower(tmp$STATE), 1, 8)
    tmp.ordered <- merge(state.shortname, tmp, by="state.shortname", all.x = T)
    tmp.ordered <- cbind(mapStates$names, tmp.ordered)
    
    # Only draw the map when occupation is selected (tmp is not NULL)
    if(nrow(tmp)>0){
      
      # Map the different levels of salary to different colors
      pal <- colorNumeric(palette="YlGnBu", domain=tmp.ordered$A_MEAN)
      
      # Prepare the layer labels to show state name and salary 
      # when the mouse is over certain states
      labels <- sprintf(
        "<strong>%s</strong><br/>$ %g Annual Salary",
        tmp.ordered$STATE, tmp.ordered$A_MEAN) %>% 
        lapply(htmltools::HTML)
      
      # Draw the leaflet map
      leaflet(data = mapStates) %>%
        addProviderTiles("Stamen.TonerLite") %>%  # default map, base layer
        
        # Draw color polygons over states according to salary in selected occupation
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
  
  # Output Panel
  observeEvent(input$usmap_shape_click, {
    # Observe the map shape click
    click <- input$usmap_shape_click
    
    tmp<-subset(data,OCC_TITLE == "Management Occupations")
    state.shortname <- as.data.frame(substr(mapStates$names, 1, 8))
    colnames(state.shortname) <- "state.shortname"
    tmp$state.shortname <- substr(tolower(tmp$STATE), 1, 8)
    tmp.ordered <- merge(state.shortname, tmp, by="state.shortname", all.x = T)
    tmp.ordered <- cbind(mapStates$names, tmp.ordered)
    state.name <- tmp.ordered[tmp.ordered[,1] == click$id, "STATE"]

    # display the state name in the output panel
    output$state_name <- renderText(state.name)
    
    # display the line chart of GDP trend in the output panel
    output$click_gdp_trend<- renderPlotly({
      df <- as.data.frame(t(gdp.aer.rpp)[1:4,])
      colnames(df) <- gdp.aer.rpp[,1]
      df <- df[-1,]
      plot.df <- data.frame(year=2014:2016,gdp=df[,state.name])
      plot_ly(x=plot.df$year,y=plot.df$gdp, type='scatter', mode = 'lines') %>%
        layout(xaxis=list(title="Years",tickfont=list(size=9)),
               yaxis=list(title="GDP",tickfont=list(size=9)))
    })
    
    # display the pie chart of recreation level in the output panel
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
  
  
  
  ### Part 4: Your Recommendation
  
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
  
  # Display the recommendation table based on user's inputs
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
    
    # Sort table 
    dt.data.scale<-dt.data.scale[order(dt.data.scale$TotalRank),]
    
    # Reorder the columns
    dt.data.scale<-dt.data.scale %>%
      dplyr::select(TotalRank,"STATE","Title",select1,select2,select3,everything()) %>%
      dplyr::select(-score)
    
  })
  
  
  
  ### Part 5: Basic Wage Info
  
  # Display the main data we used in the project
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
  
  options(warn = -1)  # for ignoring the incompatibility among ggplot2, plotly and shiny widget IDs.
  
}


