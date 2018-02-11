library(shiny)
data <- read.csv("state_M2016.csv")
data.Alabama <- data[(1:1000)[data$STATE=="Alabama"],]
major <- as.character(data.Alabama[(1:nrow(data.Alabama))[data.Alabama$OCC_GROUP=="major"],"OCC_TITLE"])
major
ui<- navbarPage(
  
  ##link to css.file
  theme = "bootstrap2.css",
  
  ##Project Title
  "iJob - Your Job Advisor",
  
  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("TRACE OF AROMA"),
           h4(htmlOutput("text")),
           htmlOutput("teammates")
  ),
  
  ## 3D Globe tab
  tabPanel("3D Globe",
           titlePanel("Coffee ,tea, and others traded between US and the world"),
           absolutePanel(id = "controls", class = "panel panel-default",
                         draggable = TRUE, 
                         top = 180, left = 60, right = "auto", bottom = "auto",
                         width = 350, height = "auto",
                         
                         h2("3D Explorer"),
                         
                         radioButtons(inputId = "type",
                                      label  = "Choose import/export",
                                      choices = c('Export','Import'),
                                      selected ='Import'),
                         sliderInput(inputId = "year_3D",
                                     label = "Select a year",
                                     value = 1996, min =1996, max =2016),
                         sliderInput(inputId = "number_countries",
                                     label = "Top Countries in Trade",
                                     value = 10,min = 1,max = 50),
                         selectInput(inputId = "commodity_3D",
                                     label  = "Select the commodity",
                                     choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                                     selected ='Coffee')
           ),
           
           
           
           globeOutput("Globe",width="100%",height="650px")),
  ## end 3D Globe tab
  
  ## Find Your Location
  tabPanel("Find Your Location",
           titlePanel("Find Your Location"),
           
           leafletOutput("mymap",width = "100%", height = 600),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
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