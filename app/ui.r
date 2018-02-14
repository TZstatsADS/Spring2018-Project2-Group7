library("varhandle")
library("leaflet")
library("shiny")
library("ggmap")
library("ggplot2")

data <- read.csv("state_M2016.csv")
data.Alabama <- data[(1:1000)[data$STATE=="Alabama"],]
major <- as.character(data.Alabama[(1:nrow(data.Alabama))[data.Alabama$OCC_GROUP=="major"],"OCC_TITLE"])
# major


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
                         
                         selectInput(inputId = "major",
                                     label  = "Select the Occupations",
                                     choices = major,
                                     selected ='Management Occupations'),
                         radioButtons(inputId = "crime_climate",
                                      label  = "Display Crime/Climate",
                                      choices = c('Crime','Climate'),
                                      selected ='Crime')
                                    )
  )
)