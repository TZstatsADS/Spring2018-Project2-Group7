
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
             
             
             
             radioButtons(inputId = "crime_climate",
                          label  = "Display Crime/Climate",
                          choices = c('Crime','Climate'),
                          selected ='Crime')
             
             
           )
           
  )
  
)



