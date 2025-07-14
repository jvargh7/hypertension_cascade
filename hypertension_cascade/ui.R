
library(shiny)
library(shinydashboard)
library(tmap)
library(waiter)

run_manual = FALSE
if(run_manual){
  map2016_v024 <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2016_v024")
  map2018_sdist <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2018_sdist")
  mapnfhs5_sdist <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="mapnfhs5_sdist")
  mapnfhs5_v024 <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="mapnfhs5_v024")
  
}

map2016_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2016_v024")
map2018_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2018_sdist")
mapnfhs5_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="mapnfhs5_sdist")
mapnfhs5_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="mapnfhs5_v024")


sidebar_about <- conditionalPanel(condition = "input.selectedpanel == 1",
                                  h3(""))

sidebar_overview <- conditionalPanel(condition="input.selectedpanel==2",
                                     h3(""),
                                     selectInput("zinput1","Age Standardized:",choices = c("Yes","No"),selected = "No"),
                                     selectInput("cutoffinput1","Hypertension Cut-point:",choices = c("140/90 mmHg","130/80 mmHg"),selected = "140/90 mmHg"),
                                     selectInput("stateinput1","Select State:",unique(mapnfhs5_v024$n5_state),selected = "Kerala"),
                                     selectInput("districtinput1","Select District:",c(""),selected = "Kottayam"),
                                     selectInput("varinput1","Select Variable:",c("Screened","Hypertension","Diagnosed","Treated","Controlled"),selected="Diagnosed"),
                                     selectInput("mapinput1","Select Region:",c("Total","Urban","Rural"),selected="Total"),
                                     selectInput("stratainput1","Select Sex:",c("Total","Male","Female"),selected = "Female")
)

sidebar_state <- conditionalPanel(condition="input.selectedpanel==3",
                                  h3(""),
                                  selectInput("zinput2","Age Standardized:",choices = c("Yes","No"),selected = "No"),
                                  selectInput("cutoffinput2","Hypertension Cut-point:",choices = c("140/90 mmHg","130/80 mmHg"),selected = "140/90 mmHg"),
                                  selectInput("stateinput2","Select State:",unique(mapnfhs5_v024$n5_state),selected = "Kerala"),
                                  # selectInput("varinput2","Select Variable:",c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
                                  # selectInput("mapinput2","Select Display:",c("Urban","Rural")),
                                  selectInput("stratainput2","Select Sex:",c("Total","Male","Female"),selected = "Female")
)

sidebar_stratified <- conditionalPanel(condition="input.selectedpanel==4",
                                       h3(""),
                                       selectInput("zinput3","Age Standardized:",choices = c("Yes","No"),selected = "No"),
                                       selectInput("cutoffinput3","Hypertension Cut-point:",choices = c("140/90 mmHg","130/80 mmHg"),selected = "140/90 mmHg"),
                                       selectInput("stateinput3","Select State:",unique(mapnfhs5_v024$n5_state),selected = "Kerala")
                                       # selectInput("varinput3","Select Variable:",c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
                                       # selectInput("mapinput3","Select Display:",c("Urban","Rural")),
                                       # selectInput("stratainput3","Select Strata:",c("Total","Male","Female"),selected = "Female")
)


sidebar_contact <- conditionalPanel(condition = "input.selectedpanel == 5",
                                    h3(""))

sidebar <- dashboardSidebar(
  sidebar_about,
  sidebar_overview,
  sidebar_state,
  sidebar_stratified,
  sidebar_contact
  
)



panel_about <- tabPanel("About",value = 1,
                        # https://stackoverflow.com/questions/65587869/r-shiny-how-to-box-a-simple-text-on-a-shiny-page
                        fluidRow(
                          
                          fluidRow(
                            box(solidHeader=FALSE,status="warning",title = "Definitions",
                                tableOutput("tabledef")),
                            
                            box(solidHeader=FALSE,status="warning",title = "Strengths",
                                p("1. Survey enumerators attempted to reach all adults in households of eligible adults"),
                                p("2. Absolute estimates may not be reliable, but relative comparisons should be")
                            ), 
                            box(solidHeader=FALSE,status="warning",title = "Limitations",
                                p("1. Information bias from self-report of high blood pressure, not verified through medical records"),
                                p("2. A single time point blood pressure does not meet confirmatory standards for diagnosis"),
                                p("3. NFHS-5 does not collect information on older adults living by themselves, unlike LASI")
                            )
                            
                          ),
                          
                          
                          
                          fluidRow(
                            box(solidHeader=FALSE,status="warning",title = "",
                                # https://stackoverflow.com/questions/36182535/how-to-place-an-image-in-an-r-shiny-title
                                # tags$img(src = file.path("Logo 1.jpg"),height='100',width='200'),
                                tags$img(src = file.path("gdrc.png"),height='43',width='313'),
                                # tags$img(src = file.path("Logo 2.jpg"),height='100',width='200'),
                                # tags$img(src = file.path("Logo 3.jpg"),height='100',width='200'),
                            ),
                            box(solidHeader=FALSE,status="warning",title = "Citation",
                                p("Please cite as:"),
                                code(paste0("Varghese JS, Nikhil SV, Sudharsanan N, 
                              Jeemon P, Patel SA, Thirumurthy H,
                              Roy A, Tandon N, Narayan KMV, Prabhakaran D, Ali MK.
                              Hypertension diagnosis, treatment, and control in India: results from a national survey of 1.69 million adults aged 18 years and older in India, 2019-2021.
                              2022; Version 1.0. Accessed on ",format(Sys.Date(),"%d %B %Y"),"."))
                                
                            ),
                            
                            box(solidHeader=FALSE,status="warning",title = "Reproducibility",
                                p("Data available at:"),
                                tags$a(href="https://www.dhsprogram.com", 
                                       "https://www.dhsprogram.com"),
                                p(""),
                                
                                p("Code available at:"),
                                tags$a(href="https://github.com/jvargh7/hypertension_cascade", 
                                       "https://github.com/jvargh7/hypertension_cascade"))
                            
                            
                          )
                          
                          
                        )
                        
)



panel_overview <- tabPanel("Overview",value = 2,
                           
                           fluidRow(
                             h4("Please stay on this page for 15 to 20 seconds for dashboard to load.",align='center'),
                             h5("In the meantime, you can easily visualize state-level summary statistics for diabetes at:",align='center'),
                             h5(tags$a("shodha.stopncd.org", href="http://shodha.stopncd.org/",target="_blank"),align='center'),
                             
                             useWaiter(),
                             
                             box(solidHeader=FALSE,status="warning",title = "National Overview (%)",
                                 tmap::tmapOutput("nationalmap"), width = 6),
                             
                             box(solidHeader=FALSE,status="warning",title = "Selected State (%)",
                                 tmap::tmapOutput("statemap"), width = 6)
                           ),
                           fluidRow(box(solidHeader=FALSE,status="warning",width = 12, title = "Hypertension Care Cascade - National, State, District (%)",
                                        # background = "light-blue",
                                        tableOutput("tableoutput1"),
                                        tableOutput("tableoutput2"),
                                        tableOutput("tableoutput3")))
                           
                           
                           
                           
)

panel_state <- tabPanel("District Disparities",value = 3,
                        fluidRow(
                          h3("Please select inputs from panel on left",align='center'),
                          h4("Age standardized to national distribution within district",align='center'),
                          
                          box(solidHeader=FALSE,status="warning",width = 12,
                              title = "Between-district Disparities (%)",
                              plotOutput("unmet_districts2",height=600))
                          # box(plotOutput("cascade_state2",height = 800))
                          
                          
                        )
                        
)

panel_stratified <- tabPanel("Socio-demographic Disparities",value =4,
                             fluidRow(
                               h3("Please select inputs from panel on left",align='center'),
                               h4("Age standardized to national distribution for urban-rural strata within state",align='center'),
                               box(solidHeader=FALSE,status="warning",width=12,
                                   title = "Socio-demographic Disparities (%)",
                                   plotOutput("cascade_state3",height = 1000))
                               
                               
                             )
)


panel_contact <- tabPanel("Contact",value = 5,
                          # https://stackoverflow.com/questions/65587869/r-shiny-how-to-box-a-simple-text-on-a-shiny-page
                          fluidRow(
                            
                            box(solidHeader=FALSE,status="warning",title = "Issues and Request for Features",
                                p("Start an issue at:"),
                                tags$a(href="https://github.com/jvargh7/hypertension_cascade/issues", 
                                       "https://github.com/jvargh7/hypertension_cascade/issues"),
                                p(""),
                                
                                p("Any other questions, email with subject ",strong("Hypertension Dashboard :")),
                                strong("jvargh7@emory.edu")
                            )
                            
                            
                          )
                          
)

body <- dashboardBody(
  tabsetPanel(
    panel_about,
    panel_overview,
    panel_state,
    panel_stratified,
    panel_contact,
    id = "selectedpanel"
  )
  
)




dashboardPage(
  
  dashboardHeader(title = "Hypertension Care Cascade, 2019-21",titleWidth = 400),
  
  sidebar,
  
  body
  
  
)