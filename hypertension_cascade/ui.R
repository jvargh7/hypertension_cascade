
library(shiny)
library(shinydashboard)
library(tmap)
run_manual = FALSE
if(run_manual){
  map2016_v024 <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2016_v024")
  map2018_sdist <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2018_sdist")
  
}

map2016_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2016_v024")
map2018_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2018_sdist")

sidebar_about <- conditionalPanel(condition = "input.selectedpanel == 1",
                                  h3(""))

sidebar_overview <- conditionalPanel(condition="input.selectedpanel==2",
                                     h3(""),
                                     selectInput("zinput1","Age Standardized:",choices = c("Yes","No"),selected = "Yes"),
                                     selectInput("stateinput1","Select State:",unique(map2016_v024$n5_state),selected = "Kerala"),
                                     selectInput("districtinput1","Select District:",c(""),selected = "Kottayam"),
                                     selectInput("varinput1","Select Variable:",c("Screened","Hypertension","Diagnosed","Treated","Controlled"),selected="Diagnosed"),
                                     selectInput("mapinput1","Select Display:",c("Urban","Rural")),
                                     selectInput("stratainput1","Select Strata:",c("Total","Male","Female"),selected = "Female")
)

sidebar_state <- conditionalPanel(condition="input.selectedpanel==3",
                                  h3(""),
                                  selectInput("zinput2","Age Standardized:",choices = c("Yes","No"),selected = "Yes"),
                                  selectInput("stateinput2","Select State:",unique(map2016_v024$n5_state),selected = "Kerala"),
                                  # selectInput("varinput2","Select Variable:",c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
                                  # selectInput("mapinput2","Select Display:",c("Urban","Rural")),
                                  selectInput("stratainput2","Select Strata:",c("Total","Male","Female"),selected = "Female")
)

sidebar_stratified <- conditionalPanel(condition="input.selectedpanel==4",
                                       h3(""),
                                       selectInput("zinput3","Age Standardized:",choices = c("Yes","No"),selected = "Yes"),
                                       selectInput("stateinput3","Select State:",unique(map2016_v024$n5_state),selected = "Kerala")
                                       # selectInput("varinput3","Select Variable:",c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
                                       # selectInput("mapinput3","Select Display:",c("Urban","Rural")),
                                       # selectInput("stratainput3","Select Strata:",c("Total","Male","Female"),selected = "Female")
)

sidebar <- dashboardSidebar(
  sidebar_about,
  sidebar_overview,
  sidebar_state,
  sidebar_stratified
  
)

panel_about <- tabPanel("About",value = 1,
                        # https://stackoverflow.com/questions/65587869/r-shiny-how-to-box-a-simple-text-on-a-shiny-page
                        fluidRow(
                          
                          box(solidHeader=FALSE,status="warning",title = "Definitions",
                              tableOutput("tabledef")),
                          
                          box(solidHeader=FALSE,status="warning",title = "Team",
                              # https://stackoverflow.com/questions/36182535/how-to-place-an-image-in-an-r-shiny-title
                              # tags$img(src = file.path("Logo 1.jpg"),height='100',width='200'),
                              tags$img(src = file.path("gdrc combined.png"),height='200',width='400'),
                              # tags$img(src = file.path("Logo 2.jpg"),height='100',width='200'),
                              # tags$img(src = file.path("Logo 3.jpg"),height='100',width='200'),
                              p("Conceptualization and Development: Jithin Sam Varghese,....,Mohammed K. Ali"))
                          
                        ),
                        fluidRow(
                          
                          box(solidHeader=FALSE,status="warning",title = "Citation",
                              p("Please cite as:"),
                              code(paste0("Varghese JS, Nikhil SV, Jeemon P, Geldsetzer P, Sudharsanan N, Manne-Goehler J, Thirumurthy H, 
                                      Roy A, Narayan KMV, Reddy S, Prabhakaran D, Ali MK.
                                      Diabetes diagnosis, treatment, and control in India: results from a national survey of 1.65 million adults aged 18 years and older in India, 2019-2021.
                                      2022; Version 1.0. Accessed on ",format(Sys.Date(),"%d %B %Y"),"."))
                          
                          ),
                          
                          box(solidHeader=FALSE,status="warning",title = "Reproducibility",
                              p("Data available at:"),
                              tags$a(href="https://www.dhsprogram.com", 
                                     "https://www.dhsprogram.com"),
                              p(""),
                              
                              p("Code available at:"),
                              tags$a(href="https://github.com/jvargh7/nfhs_cascade", 
                                     "https://github.com/jvargh7/nfhs_cascade"))
                          
                          
                        )
)


panel_overview <- tabPanel("Overview",value = 2,
                           fluidRow(
                             h3("Please wait for 15 to 20 seconds for maps to load",align='center'),
                             
                             box(solidHeader=FALSE,status="warning",title = "National Overview (%)",
                                 tmap::tmapOutput("nationalmap"), width = 6),
                             
                             box(solidHeader=FALSE,status="warning",title = "Selected State (%)",
                                 tmap::tmapOutput("statemap"), width = 6)
                           ),
                           
                           
                           box(solidHeader=FALSE,status="warning",width = 12, title = "Hypertension Care Cascade (%)",
                               # background = "light-blue",
                               tableOutput("tableoutput"))
                           
)

panel_state <- tabPanel("District Disparities",value = 3,
                        fluidRow(
                          h3("Please select inputs from panel on left",align='center'),
                          
                          box(solidHeader=FALSE,status="warning",width = 12,
                              title = "Between-district Disparities (%)",
                              plotOutput("unmet_districts2",height=600))
                          # box(plotOutput("cascade_state2",height = 800))
                          
                          
                        )
                        
)

panel_stratified <- tabPanel("Socio-demographic Disparities",value =4,
                             fluidRow(
                               h3("Please select inputs from panel on left",align='center'),
                               box(solidHeader=FALSE,status="warning",width=12,
                                   title = "Socio-demographic Disparities (%)",
                                   plotOutput("cascade_state3",height = 1000))
                               
                               
                             )
)

body <- dashboardBody(
  tabsetPanel(
    panel_about,
    panel_overview,
    panel_state,
    panel_stratified,
    id = "selectedpanel"
  )
  
)




dashboardPage(
  
  dashboardHeader(title = "Hypertension Care Cascade, 2019-21",titleWidth = 400),
  
  sidebar,
  
  body
  
  
)