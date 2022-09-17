
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(tmap)
library(ggpubr)
run_manual = FALSE
if(run_manual){
  map2016_v024 <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2016_v024")
  map2018_sdist <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2018_sdist")
  district_shp <- readRDS(file.path("hypertension_cascade/data","district_shp.RDS"))
  state_shp <- readRDS(file.path("hypertension_cascade/data","state_shp.RDS"))
  
  hca02_national <- readRDS(file.path("hypertension_cascade/data","hca02_national.RDS"))
  national_nested <- readRDS(file.path("hypertension_cascade/data","national_nested.RDS"))
  
  
  hca03_state <- readRDS(file.path("hypertension_cascade/data","hca03_state.RDS"))
  hca05_state_unmet <- readRDS(file.path("hypertension_cascade/data","hca05_state_unmet.RDS"))
  state_nested <- readRDS(file.path("hypertension_cascade/data","state_nested.RDS"))
  statez_nested <- readRDS(file.path("hypertension_cascade/data","statez_nested.RDS"))
  
  hca04_district <- readRDS(file.path("hypertension_cascade/data","hca04_district.RDS"))
  hca08_district_unmet <- readRDS(file.path("hypertension_cascade/data","hca08_district_unmet.RDS"))
  district_nested <- readRDS(file.path("hypertension_cascade/data","district_nested.RDS"))
  districtz_nested <- readRDS(file.path("hypertension_cascade/data","districtz_nested.RDS"))
  
  source("hypertension_cascade/code/cascade_plot.R")
  
  
  input = data.frame(stateinput1 = "Kerala",districtinput1 = "Kottayam",
                     varinput1 = "Diagnosed",mapinput1 = "Rural",stratainput1 = "Total") %>% mutate_all(~as.character(.))
}

map2016_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2016_v024")
map2018_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2018_sdist")

district_shp <- readRDS(file.path("data","district_shp.RDS"))
state_shp <- readRDS(file.path("data","state_shp.RDS"))
hcz01_national <- readRDS(file.path("data","hcz01_national.RDS"))
hca02_national <- readRDS(file.path("data","hca02_national.RDS"))
national_nested <- readRDS(file.path("data","national_nested.RDS"))

hcz02_state <- readRDS(file.path("data","hcz02_state.RDS"))
hca03_state <- readRDS(file.path("data","hca03_state.RDS"))
hca05_state_unmet <- readRDS(file.path("data","hca05_state_unmet.RDS"))
state_nested <- readRDS(file.path("data","state_nested.RDS"))
statez_nested <- readRDS(file.path("data","statez_nested.RDS"))

hca04_district <- readRDS(file.path("data","hca04_district.RDS"))
hca08_district_unmet <- readRDS(file.path("data","hca08_district_unmet.RDS"))
district_nested <- readRDS(file.path("data","district_nested.RDS"))
districtz_nested <- readRDS(file.path("data","districtz_nested.RDS"))

source("code/cascade_plot.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # Panel 1: About -----------
  
  output$tabledef <- renderTable({
    
    data.frame(Indicator = c("Age standardization","Screening","Hypertension","",
                             "Diagnosed","Treated","Controlled"),
               Definition = c("Age standardized to national distribution as per NFHS-5 [18-39: 50.26%, 40-64: 38.78%, 65+: 10.96%]",
                              "Blood pressure ever checked previously",
                              "(a) Self-reported hypertension",
                              "(b) High blood pressure (≥140 mmHg systolic or 90 mmHg diastolic)",
                              "Told had high blood pressure on two or more occasions by a medical provider among those with Hypertension",
                              "Currently taking a prescribed medicine to lower blood pressure among those with Hypertension",
                              "Blood pressure in non-hypertensive range (<140/90 mmHg) among those with Hypertension")
    )
  })
  
  
  
  
  
  # Panel 2: Overview ----------------
  nested_s1 <- reactive({
    if(input$zinput1 == "Yes"){
      return(statez_nested)
      
      
    } else{
      return(state_nested)
    }
  })

  n5_state_input <- reactive({
    input$stateinput1
  })
  
  # https://stackoverflow.com/questions/64796206/dynamically-update-two-selectinput-boxes-based-on-the-others-selection-in-r-shin
  observe({
    d_i = map2018_sdist[map2018_sdist$n5_state == n5_state_input(),]$D_NAME
    updateSelectInput(session, "districtinput1", choices = na.omit(d_i)) 
  })  
  
  
  nm_merge <- reactive({
    
    ss <- state_shp %>% 
      sp::merge(nested_s1() %>%
                  dplyr::filter(variable == input$varinput1,
                                residence == input$mapinput1,
                                strata == input$stratainput1)  %>% 
                  dplyr::select(n5_state,ST_NM,estimate) %>% 
                  rename_at(vars(estimate),~paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1)),
                by.x="ST_NM",by.y="ST_NM",all.x=TRUE)
    
    ss
  })
  
  
  palette_chr <- reactive({
    case_when(input$varinput1 == "Hypertension" ~ "-RdYlGn",
              TRUE ~ "RdYlGn")
  })
  
  breaks <- reactive({
    if(input$varinput1 == "Hypertension"){
      seq(0,60,by=15)
      
    }
    else{seq(0,100,by=20)}
    
  })
  
  # output$nationalmap -----------
  output$nationalmap <- tmap::renderTmap({
    
    nm <- tmap_mode("view") +
      tm_shape(shp = nm_merge(),id = "n5_state") +
      tm_fill(title= "",
              col=paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1),
              palette = palette_chr(),
              style = "fixed",
              breaks= breaks(),
              # midpoint = NA,
              textNA="Data not available",
              colorNA = "white")+ 
      tm_borders(col="black") + 
      tm_text(text="n5_state",col="black",size=0.5,remove.overlap = TRUE)+
      tm_view(view.legend.position = c("right","top")) +
      tm_legend(
        legend.outside=FALSE,
        legend.just=c("left","top"))
    nm
    
    
    
  })
  
  
  
  
  
  # output$statemap -----------
  
  nested_d1 <- reactive({
    if(input$zinput1 == "Yes"){
      return(districtz_nested)
    }
    if(input$zinput1 == "No"){
      return(district_nested)
    }
  })
  
  
  sm_merge <- reactive({
    ds <- district_shp %>% 
      sp::merge(nested_d1() %>%
                  dplyr::filter(variable == input$varinput1,
                                strata == input$stratainput1)  %>% 
                  dplyr::select(D_CODE,n5_state,estimate) %>% 
                  rename_at(vars(estimate),~paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1)),
                by.x="D_CODE",by.y="D_CODE",all.x=TRUE) 
    
    ds@data <- ds@data %>% 
      dplyr::select(D_NAME,D_CODE,everything())
    
    # https://stackoverflow.com/questions/52384937/subsetting-spatial-polygon-dataframe
    subset(ds,n5_state == input$stateinput1)
  })
  
  
  output$statemap <- tmap::renderTmap({
    
    sm <- tmap_mode("view") +
      tm_shape(sm_merge(),ext=1.2,id="D_NAME") + 
      tm_fill(title= "",
              col=paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1),
              palette = palette_chr(),
              style = "fixed",
              breaks= breaks(),
              # midpoint = NA,
              textNA="Data not available",
              colorNA = "white")+ 
      tm_borders(col="black") + 
      tm_text(text="D_NAME",col="black",size=0.5,remove.overlap = TRUE)+
      tm_view(view.legend.position = c("right","top"))
    tm_legend(
      legend.outside=FALSE,
      legend.just=c("left","top"))
    # tm_layout(plot_title,title.size = 2,
    #           legend.text.size = 1,
    #           legend.title.size = 1)
    
    sm
    
  })
  
  # Table --------
  tab1 <- reactive({
    
    st_df <- nested_s1() %>% 
      dplyr::filter(strata %in% c("Total","Male","Female")) %>% 
      dplyr::filter(n5_state == input$stateinput1) %>% 
      dplyr::select(variable,residence,strata,est_ci) %>% 
      mutate(residence = paste0(input$stateinput1," ",residence," ",strata))  %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) 
    
    nt_df <- national_nested %>% 
      dplyr::filter(strata %in% c("Total","Male","Female")) %>% 
      dplyr::select(variable,strata,residence,est_ci) %>% 
      mutate(residence = paste0("India ",residence," ",strata)) %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) 
    
    dt_df <- nested_d1() %>% 
      dplyr::filter(strata == input$stratainput1,
                    D_NAME == input$districtinput1) %>% 
      dplyr::select(variable,strata,D_NAME,est_ci) %>% 
      rename(residence = D_NAME)  %>% 
      mutate(residence = paste0(residence," ",strata)) %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) 
    
    left_join(
      nt_df ,
      st_df,
      by = "variable"
    ) %>% 
      left_join(dt_df,
                by="variable") %>% 
      mutate_all(function(x) str_replace(x," \\(","<br>\\(")) %>% 
      rename(Cascade = variable)
  })
  
  # output$tableoutput -----------
  output$tableoutput <- renderTable({
    
    tab1()
    
  },bordered = TRUE, sanitize.text.function=identity,align = "c")
  
  # Panel 3: State ------------------
  
  
  unmet_d2 <- reactive({
    if(input$zinput2 == "Yes"){
      return(districtz_nested)
    }
    if(input$zinput2 == "No"){
      return(district_nested)
    }
  })
  
  
  observeEvent(input$stateinput2,
               {
                 updateSelectInput(session = session,
                                   inputId = "stateinput3",
                                   selected = input$stateinput2)
               })
  
  
  
  panel2_n5_state <- reactive({
    input$stateinput2
  })
  
  district_cm_merge2 <- reactive({
    dcm2 <- unmet_d2() %>% 
      dplyr::filter(strata == input$stratainput2,
                    n5_state == panel2_n5_state())
    
    
    dcm2
    
  })
  
  # output$unmet_districts -----------
  
  output$unmet_districts2 <- renderPlot({
    
    fig_prevalence <- district_cm_merge2() %>% 
      dplyr::filter(variable == "Hypertension") %>% 
      ggplot(data=.,aes(x = D_NAME,y = estimate,ymin = lci,ymax=uci,
                        group=D_NAME)) +
      geom_col(position=position_dodge(width=0.9),fill="lightblue") +
      geom_errorbar(position = position_dodge(width=0.9),width=0.1) +
      theme_bw() + 
      coord_flip() +
      facet_grid(~variable,scales="free",space="free_y") +
      scale_y_continuous(limits=c(0,40),breaks=seq(0,40,by=10)) +
      facet_grid(~variable,scales="free_y",space="free_y") +
      theme(
        legend.text = element_text(size=12),
        axis.text = element_text(size = 12),
        strip.background.y = element_blank(),
        strip.text.x = element_text(size=12),
        strip.text.y = element_blank(),
        legend.position = "bottom") +
      # scale_y_continuous(limits=c(0,50)) +
      ylab("Prevalence (%)") +
      xlab("") 
    
    fig_uc <- district_cm_merge2() %>% 
      dplyr::filter(!variable %in% c("Hypertension","Screened")) %>% 
      ggplot(data=.,aes(x = D_NAME,y = estimate,ymin = lci,ymax=uci,
                        group=D_NAME),fill="lightblue") +
      geom_col(position=position_dodge(width=0.9)) +
      geom_errorbar(position = position_dodge(width=0.9),width=0.1) +
      theme_bw() + 
      coord_flip() +
      facet_grid(~variable,scales="free",space="free_y") +
      scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100)) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size=12),
        legend.position = "bottom") +
      # scale_y_continuous(limits=c(0,50)) +
      ylab("Prevalence (%)") +
      xlab("") 
    
    ggarrange(fig_prevalence,fig_uc,nrow=1,ncol=2,
              common.legend = TRUE,legend="bottom",
              widths = c(1.5,2))
  })
  
  # Panel 4: Stratified ---------
  
  unmet_d3 <- reactive({
    if(input$zinput3 == "Yes"){
      return(districtz_nested)
    }
    if(input$zinput3 == "No"){
      return(district_nested)
    }
  })
  
  
  unmet_s3 <- reactive({
    if(input$zinput3 == "Yes"){
      return(statez_nested)
    }
    if(input$zinput3 == "No"){
      return(state_nested)
    }
  })
  
  panel3_n5_state <- reactive({
    input$stateinput3
  })
  
  district_cm_merge3 <- reactive({
    dcm3 <- unmet_d3() %>% 
      dplyr::filter(strata == input$stratainput3,
                    n5_state == panel3_n5_state())
    
    dcm3
    
  })
  
  # output$cascade_state3 --------------
  
  state_cs_merge <- reactive({
    # panel2_n5_state = "Kerala"
    scm <- unmet_s3() %>% 
      dplyr::filter(n5_state == panel3_n5_state()) %>% 
      # mutate(cascade = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
      mutate(cascade = factor(variable,levels=c("Screened","Hypertension","Diagnosed","Treated","Controlled"),
                              labels=c("Screened","Hypertension","Diagnosed","Taking Medication","Under Control"))) %>% 
      mutate(group = case_when(is.na(strata) ~ paste0(residence,"\nTotal"),
                               TRUE ~ paste0(residence,"\n",strata)))
    scm
    
  })
  
  output$cascade_state3 <- renderPlot({
    
    figA <- state_cs_merge() %>% 
      dplyr::filter(is.na(stratification)|stratification == "sex") %>% 
      cascade_plot(.,limits_y = c(0,45))
    figB <- state_cs_merge() %>% 
      dplyr::filter(stratification == "age_category") %>% 
      cascade_plot(.,limits_y = c(0,45))
    figC <- state_cs_merge() %>%
      dplyr::filter(stratification == "education") %>%
      mutate(group = factor(group, levels=c("Rural\nNo education","Rural\nPrimary","Rural\nSecondary","Rural\nHigher",
                                            "Urban\nNo education","Urban\nPrimary","Urban\nSecondary","Urban\nHigher"
      ))) %>% 
      cascade_plot(.,limits_y = c(0,45))
    figD <- state_cs_merge() %>%
      dplyr::filter(stratification == "caste") %>%
      cascade_plot(.,limits_y = c(0,45))
    figE <- state_cs_merge() %>%
      dplyr::filter(stratification == "swealthq_ur") %>%
      mutate(group = factor(group,
                            levels = paste0(rep(c("Rural","Urban"),each =5),
                                            "\n",
                                            rep(c("Wealth: Lowest","Wealth: Low",
                                                  "Wealth: Medium","Wealth: High",
                                                  "Wealth: Highest"),times=2)),ordered=TRUE)) %>% 
      cascade_plot(.,limits_y = c(0,45))
    
    ggarrange(figA,
              figB,
              figC,
              figD,
              figE,
              labels = LETTERS[1:5],ncol = 1,nrow=5,common.legend = TRUE,legend="top")
    
    
  })
  
  
})
