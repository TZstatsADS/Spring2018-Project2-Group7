
#load the packages
library("varhandle")
library("leaflet")
library("shiny")
library("ggmap")
library("ggplot2")
library("maps")
library("plotly")
library("DT")

data <- read.csv("state_M2016.csv",header = TRUE,stringsAsFactors = FALSE)
data.Alabama <- data[data$STATE=="Alabama",]
major <- as.character(data.Alabama[data.Alabama$OCC_GROUP=="major","OCC_TITLE"])
data.Alabama$first2 <- substr(data.Alabama$OCC_CODE, 1, 2)
data2<-read.csv("state_M2016_2.csv",header = T)

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

preference_list <- c("Salary","Crime Rate","Cleardays","Recreation Level","RPP Rents","RPP Price")

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
  
  tabPanel("Choose Your Major",titlePanel("Choose Your Major"),
           
           sidebarLayout(      
             
             sidebarPanel(choice <<- list("Management Occupations" = 1, 
                                          "Business and Financial Operations Occupations" = 2, 
                                          "Computer and Mathematical Occupations" = 3,
                                          "Architecture and Engineering Occupations" = 4,
                                          "Life, Physical, and Social Science Occupations" = 5,
                                          "Community and Social Service Occupations" = 6,
                                          "Legal Occupations" = 7,
                                          "Education, Training, and Library Occupations" = 8,
                                          "Arts, Design, Entertainment, Sports, and Media Occupations" = 9,
                                          "Healthcare Practitioners and Technical Occupations" = 10,
                                          "Healthcare Support Occupations" = 11,
                                          "Protective Service Occupations" = 12,
                                          "Food Preparation and Serving Related Occupations" = 13,
                                          "Building and Grounds Cleaning and Maintenance Occupations" = 14,
                                          "Personal Care and Service Occupations" = 15,
                                          "Sales and Related Occupations" = 16,
                                          "Office and Administrative Support Occupations" = 17,
                                          "Farming, Fishing, and Forestry Occupations" = 18,
                                          "Construction and Extraction Occupations" = 19,
                                          "Installation, Maintenance, and Repair Occupations" = 20,
                                          "Production Occupations" = 21,
                                          "Transportation and Material Moving Occupations" = 22),
                          selectInput("major1", "Major 1:", 
                                      choices = choice,selected = 1),
                          selectInput("major2", "Major 2:", 
                                      choices = choice,selected = 1),
                          selectInput("major3", "Major 3:", 
                                      choices = choice,selected = 1),
                          hr(),
               
               helpText("Data from Bureau of Labor Statistics: https://www.bls.gov/")
             ),
             
             # Create a spot for the barplot
             mainPanel(
               plotlyOutput("Plot")  
             )
             
           )),
  
  tabPanel("Basic Wage Info",titlePanel("Basic Wage Information"),
           
           
           fluidRow(
             column(4,
                    selectInput("state",
                                "State:",
                                c("All",
                                  unique(as.character(data2$State))))
             ),
             column(4,
                    selectInput("major",
                                "Major:",
                                choices=list("All" = 00,
                                             "Management Occupations" = 11, 
                                             "Business and Financial Operations Occupations" = 13, 
                                             "Computer and Mathematical Occupations" = 15,
                                             "Architecture and Engineering Occupations" = 17,
                                             "Life, Physical, and Social Science Occupations" = 19,
                                             "Community and Social Service Occupations" = 21,
                                             "Legal Occupations" = 23,
                                             "Education, Training, and Library Occupations" = 25,
                                             "Arts, Design, Entertainment, Sports, and Media Occupations" = 27,
                                             "Healthcare Practitioners and Technical Occupations" = 29,
                                             "Healthcare Support Occupations" = 31,
                                             "Protective Service Occupations" = 33,
                                             "Food Preparation and Serving Related Occupations" = 35,
                                             "Building and Grounds Cleaning and Maintenance Occupations" = 37,
                                             "Personal Care and Service Occupations" = 39,
                                             "Sales and Related Occupations" = 41,
                                             "Office and Administrative Support Occupations" = 43,
                                             "Farming, Fishing, and Forestry Occupations" = 45,
                                             "Construction and Extraction Occupations" = 47,
                                             "Installation, Maintenance, and Repair Occupations" = 49,
                                             "Production Occupations" = 51,
                                             "Transportation and Material Moving Occupations" = 53)
                                
                    )
                    
             ),
             
             fluidRow(
               DT::dataTableOutput("table")
             )
           )),
  
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
                            selected ='Management Occupations')
            
             # selectizeInput(inputId = "state_selection",
             #                label  = "Select the State",
             #                choices = state_list,
             #                selected ='New York')
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
  
  
  
  tabPanel("Your Recommandation",
           titlePanel("State Info Detail"),
           column(
             width = 6,
             plotOutput("map", click = "clickMap", width = 500, height = 350)
           ),
           column(
             width = 6,
             plotOutput("info", width = 500, height = 350)
           ),
           
           
           #Give a Recommandation
           titlePanel("Your Recommandation"),
           
           fluidRow(
             #select your preference
             column(3,
                    selectizeInput(inputId = "firstpreference",
                                   label  = "First Preference",
                                   choices = preference_list,
                                   selected ='Salary')),
             column(3,
                    selectizeInput(inputId = "secondpreference",
                                   label  = "Second Preference",
                                   choices = preference_list,
                                   selected ='Crime Rate')),
             column(3,
                    selectizeInput(inputId = "thirdpreference",
                                   label  = "Third Preference",
                                   choices = preference_list,
                                   selected ='Cleardays')),
             #datatable
             column(12,
                    DT::dataTableOutput("recommandationtable")
             )
           )
  )
  
)

 