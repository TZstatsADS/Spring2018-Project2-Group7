
#load the packages
library("varhandle")
library("leaflet")
library("shiny")
library("ggmap")
library("ggplot2")
library("maps")



data <- read.csv("state_M2016.csv",header = TRUE,stringsAsFactors = FALSE)
data.Alabama <- data[data$STATE=="Alabama",]
major <- as.character(data.Alabama[data.Alabama$OCC_GROUP=="major","OCC_TITLE"])
data.Alabama$first2 <- substr(data.Alabama$OCC_CODE, 1, 2)

return_detail_given_major <- function(major_str){
  first2_code <- data.Alabama[data.Alabama$OCC_TITLE==major_str, "first2"]
  detail<-data.Alabama[data.Alabama$first2 == first2_code, "OCC_TITLE"]
  return(unique(detail))
}

detail_list <- sapply(major, return_detail_given_major)

data.state <- read.csv("state_coordinate.csv",header = TRUE,stringsAsFactors = FALSE)
state_list <- data.state$STATE

si <- read.csv("State_info.csv", header = T, stringsAsFactors = F)
usaMap <- map_data("state")


ui<- navbarPage(
  

  ##link to css.file
  theme = "bootstrap2.css",
  
  ##Project Title
  "iJob - Your Job Advisor",
  
  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("iJob - Your Job Advisor"),
           h4(htmlOutput("text")),
           htmlOutput("teammates")
  ),
  
  
  
  ## Find Your Location
  tabPanel("Find Your Location",
           titlePanel("Find Your Location"),
           
           leafletOutput("usmap",width = "100%", height = 600),
           
           absolutePanel(
             id = "controls", class = "panel panel-default", fixed = TRUE,
             draggable = TRUE, 
             top = 180, left = 60, right = "auto", bottom = "auto",
             width = 350, height = "auto",
             h2("Job Search"),
             
             selectizeInput(inputId = "occupation",
                            label  = "Select the Occupations",
                            choices = detail_list,
                            selected ='Management Occupations'),
            
             selectizeInput(inputId = "state_selection",
                            label  = "Select the State",
                            choices = state_list,
                            selected ='New York')
           ),
             
           absolutePanel(id = "controls", class = "panel panel-default", fixed= FALSE, draggable = TRUE,
                         top = 120, left = "auto", right = 20, bottom = "auto", width = 320, height = "auto",
                         h2("State Overview"),
                         p(strong("State:"),strong(textOutput("state_name"))),
                         h3(strong("GDP Trend:")),
                         plotlyOutput("click_gdp_trend", height="150"), #click_complaint_timedist
                         h3(strong("% of AER in All Industry:")), # "Percentage of Arts, entertainment, recreation, accommodation, and food services in All industry"
                         plotlyOutput("click_amusement_pie",height="200"),
                         p("% of Arts, entertainment, recreation, accommodation, and food services in All industry in 2016")
           )  
           
  ),
  
  
  
  tabPanel("State Info Detail",
           titlePanel("State Info Detail"),
           column(
             width = 6,
             plotOutput("map", click = "clickMap", width = 430, height = 275)
           ),
           column(
             width = 6,
             plotOutput("info", width = 430, height = 275)
           )
           )
  
)



