
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(tmap)
library(ggpubr)
library(waiter)

run_manual = FALSE
if(run_manual){
  map2016_v024 <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2016_v024")
  map2018_sdist <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="map2018_sdist")
  mapnfhs5_sdist <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="mapnfhs5_sdist")
  mapnfhs5_v024 <- readxl::read_excel(file.path("hypertension_cascade/data","maps.xlsx"),sheet="mapnfhs5_v024")
  
  
  district_shp <- readRDS(file.path("hypertension_cascade/data","district_shp.RDS"))
  state_shp <- readRDS(file.path("hypertension_cascade/data","state_shp.RDS"))
  
  hcz01_national <- bind_rows(
    readRDS(file.path("hypertension_cascade/data","hcz01_national.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
    readRDS(file.path("hypertension_cascade/cutoff130","hcc130z01_national.RDS")) %>% mutate(cutpoint = "130/80 mmHg")
    )
  hca02_national <- bind_rows(
    readRDS(file.path("hypertension_cascade/data","hca02_national.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
    readRDS(file.path("hypertension_cascade/cutoff130","hcc130a02_national.RDS"))%>% mutate(cutpoint = "130/80 mmHg") 
    )
  national_nested <- bind_rows(readRDS(file.path("hypertension_cascade/data","national_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                               readRDS(file.path("hypertension_cascade/cutoff130","national_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg")
                               )
  nationalz_nested <- bind_rows(readRDS(file.path("hypertension_cascade/data","nationalz_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                                readRDS(file.path("hypertension_cascade/cutoff130","nationalz_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg")
                                )
  
  
  hca03_state <- bind_rows(readRDS(file.path("hypertension_cascade/data","hca03_state.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                           readRDS(file.path("hypertension_cascade/cutoff130","hcc130a03_state.RDS")) %>% mutate(cutpoint = "130/80 mmHg")
  )
  
  hca05_state_unmet <- bind_rows(readRDS(file.path("hypertension_cascade/data","hca05_state_unmet.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                                 readRDS(file.path("hypertension_cascade/cutoff130","hcc130a05_state_unmet.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
  
  state_nested <- bind_rows(readRDS(file.path("hypertension_cascade/data","state_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                            readRDS(file.path("hypertension_cascade/cutoff130","state_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
  
  statez_nested <- bind_rows(readRDS(file.path("hypertension_cascade/data","statez_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                             readRDS(file.path("hypertension_cascade/cutoff130","statez_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
  
  hca04_district <- bind_rows(readRDS(file.path("hypertension_cascade/data","hca04_district.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                              readRDS(file.path("hypertension_cascade/cutoff130","hcc130a04_district.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
  
  hca08_district_unmet <- bind_rows(readRDS(file.path("hypertension_cascade/data","hca08_district_unmet.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                                    readRDS(file.path("hypertension_cascade/cutoff130","hcc130a08_district_unmet.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
  
  district_nested <- bind_rows(readRDS(file.path("hypertension_cascade/data","district_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                               readRDS(file.path("hypertension_cascade/cutoff130","district_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
  
  districtz_nested <- bind_rows(readRDS(file.path("hypertension_cascade/data","districtz_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                                readRDS(file.path("hypertension_cascade/cutoff130","districtz_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
  
  source("hypertension_cascade/code/cascade_plot.R")
  
  
  input = data.frame(stateinput1 = "Kerala",districtinput1 = "Kottayam",
                     varinput1 = "Diagnosed",mapinput1 = "Rural",stratainput1 = "Total") %>% mutate_all(~as.character(.))
}

map2016_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2016_v024")
map2018_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2018_sdist")
mapnfhs5_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="mapnfhs5_sdist")
mapnfhs5_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="mapnfhs5_v024")

district_shp <- readRDS(file.path("data","district_shp.RDS"))
state_shp <- readRDS(file.path("data","state_shp.RDS")) 
hcz01_national <- bind_rows(readRDS(file.path("data","hcz01_national.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                            readRDS(file.path("cutoff130","hcc130z01_national.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
hca02_national <- bind_rows(readRDS(file.path("data","hca02_national.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                            readRDS(file.path("cutoff130","hcc130a02_national.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
national_nested <- bind_rows(readRDS(file.path("data","national_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                             readRDS(file.path("cutoff130","national_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
nationalz_nested <- bind_rows(readRDS(file.path("data","nationalz_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                              readRDS(file.path("cutoff130","nationalz_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))

hcz02_state <- bind_rows(readRDS(file.path("data","hcz02_state.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                         readRDS(file.path("cutoff130","hcc130z02_state.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
hca03_state <- bind_rows(readRDS(file.path("data","hca03_state.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                         readRDS(file.path("cutoff130","hcc130a03_state.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
hca05_state_unmet <- bind_rows(readRDS(file.path("data","hca05_state_unmet.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                               readRDS(file.path("cutoff130","hcc130a05_state_unmet.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
state_nested <- bind_rows(readRDS(file.path("data","state_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                          readRDS(file.path("cutoff130","state_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
statez_nested <- bind_rows(readRDS(file.path("data","statez_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                           readRDS(file.path("cutoff130","statez_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))

hca04_district <- bind_rows(readRDS(file.path("data","hca04_district.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                            readRDS(file.path("cutoff130","hcc130a04_district.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
hca08_district_unmet <- bind_rows(readRDS(file.path("data","hca08_district_unmet.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                                  readRDS(file.path("cutoff130","hcc130a08_district_unmet.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
district_nested <- bind_rows(readRDS(file.path("data","district_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                             readRDS(file.path("cutoff130","district_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))
districtz_nested <- bind_rows(readRDS(file.path("data","districtz_nested.RDS")) %>% mutate(cutpoint = "140/90 mmHg"),
                              readRDS(file.path("cutoff130","districtz_nested.RDS")) %>% mutate(cutpoint = "130/80 mmHg"))

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
                              "Currently taking a prescribed medicine to lower blood pressure among those with Diagnosed Hypertension",
                              "Blood pressure in non-hypertensive range (<140/90 mmHg [<80y] and <150/90 mmHg [≥80y]) among those with Treated Hypertension")
    )
  })
  
  
  
  
  
  # Panel 2: Overview ----------------
  # https://waiter.john-coene.com/#/waiter/examples#on-render -----
  w <- Waiter$new(id = c("nationalmap", "statemap"))
  
  nested_n1 <- reactive({
    if(input$zinput1 == "Yes"){
      return(nationalz_nested)
      
      
    } else{
      return(national_nested)
    }
  })
  
  nested_s1 <- reactive({
    if(input$zinput1 == "Yes"){
      return(statez_nested)
      
      
    } else{
      return(state_nested)
    }
  })

  n5_cutpoint_input <- reactive({
    input$cutoffinput1
    
  })
  
  n5_state_input <- reactive({
    input$stateinput1
  })
  
  # https://stackoverflow.com/questions/64796206/dynamically-update-two-selectinput-boxes-based-on-the-others-selection-in-r-shin
  observe({
    d_i = mapnfhs5_sdist[mapnfhs5_sdist$n5_state == n5_state_input(),]$REGNAME
    updateSelectInput(session, "districtinput1", choices = na.omit(d_i)) 
  })  
  
  
  nm_merge <- reactive({
    
    ss <- state_shp %>% 
      sp::merge(nested_s1() %>%
                  dplyr::filter(variable == input$varinput1,
                                cutpoint == input$cutoffinput1,
                                residence == input$mapinput1,
                                strata == input$stratainput1)  %>% 
                  dplyr::select(n5_state,ST_NM,estimate) %>% 
                  rename_at(vars(estimate),~paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1)),
                by.x="ST_NM",by.y="ST_NM",all.x=TRUE)
    
    # https://waiter.john-coene.com/#/waiter/examples#on-render 
    w$show()
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
                                cutpoint == input$cutoffinput1,
                                strata == input$stratainput1)  %>% 
                  dplyr::select(REGCODE,n5_state,estimate) %>% 
                  rename_at(vars(estimate),~paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1)),
                by.x="REGCODE",by.y="REGCODE",all.x=TRUE) 
    
    ds@data <- ds@data %>% 
      dplyr::select(REGNAME,REGCODE,everything())
    
    # https://stackoverflow.com/questions/52384937/subsetting-spatial-polygon-dataframe
    subset(ds,n5_state == input$stateinput1)
  })
  
  
  output$statemap <- tmap::renderTmap({
    
    sm <- tmap_mode("view") +
      tm_shape(sm_merge(),ext=1.2,id="REGNAME") + 
      tm_fill(title= "",
              col=paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1),
              palette = palette_chr(),
              style = "fixed",
              breaks= breaks(),
              # midpoint = NA,
              textNA="Data not available",
              colorNA = "white")+ 
      tm_borders(col="black") + 
      tm_text(text="REGNAME",col="black",size=0.5,remove.overlap = TRUE)+
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
  tab_national <- reactive({
    nested_n1() %>% 
      dplyr::filter(strata %in% c("Total","Male","Female"),cutpoint == input$cutoffinput1) %>% 
      dplyr::select(variable,strata,residence,est_ci) %>% 
      mutate(residence = case_when(is.na(residence) ~ "",
                                   TRUE ~ residence)) %>% 
      mutate(residence = paste0("India ",residence," ",strata)) %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) %>% 
      rename(Variable = variable)
    
  })
    
  tab_state <- reactive({
    
    nested_s1() %>% 
      dplyr::filter(strata %in% c("Total","Male","Female"),cutpoint == input$cutoffinput1) %>% 
      dplyr::filter(n5_state == input$stateinput1) %>% 
      dplyr::select(variable,residence,strata,est_ci) %>% 
      mutate(residence = paste0(input$stateinput1," ",residence," ",strata))  %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci)%>% 
      rename(Variable = variable)
    
  })
  
  tab_district <- reactive({
    nested_d1() %>% 
      dplyr::filter(strata == input$stratainput1,
                    REGNAME == input$districtinput1,
                    cutpoint == input$cutoffinput1) %>% 
      dplyr::select(variable,strata,REGNAME,est_ci) %>% 
      rename(residence = REGNAME)  %>% 
      mutate(residence = paste0(residence," ",strata)) %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) %>% 
      rename(Variable = variable)
    
  })
    
  
  # output$tableoutput -----------
  output$tableoutput1 <- renderTable({
    
    tab_national()
    
  },bordered = TRUE, sanitize.text.function=identity,align = "c")
  
  output$tableoutput2 <- renderTable({
    
    tab_state()
    
  },bordered = TRUE, sanitize.text.function=identity,align = "c")
  
  output$tableoutput3 <- renderTable({
    
    tab_district()
    
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
  
  
  
  panel2_n5_cutpoint <- reactive({
    input$cutoffinput2
    
  })
  
  panel2_n5_state <- reactive({
    input$stateinput2
  })
  
  district_cm_merge2 <- reactive({
    dcm2 <- unmet_d2() %>% 
      dplyr::filter(strata == input$stratainput2,
                    cutpoint == input$cutoffinput2,
                    n5_state == panel2_n5_state())
    
    
    dcm2
    
  })
  
  # output$unmet_districts -----------
  
  output$unmet_districts2 <- renderPlot({
    
    fig_prevalence <- district_cm_merge2() %>% 
      dplyr::filter(variable == "Hypertension") %>% 
      ggplot(data=.,aes(x = REGNAME,y = estimate,ymin = lci,ymax=uci,
                        group=REGNAME)) +
      geom_col(position=position_dodge(width=0.9),fill="lightblue") +
      geom_errorbar(position = position_dodge(width=0.9),width=0.1) +
      theme_bw() + 
      coord_flip() +
      facet_grid(~variable,scales="free",space="free_y") +
      scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100)) +
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
      ggplot(data=.,aes(x = REGNAME,y = estimate,ymin = lci,ymax=uci,
                        group=REGNAME),fill="lightblue") +
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
                    cutpoint == input$cutoffinput3,
                    n5_state == panel3_n5_state())
    
    dcm3
    
  })
  
  # output$cascade_state3 --------------
  
  state_cs_merge <- reactive({
    # panel2_n5_state = "Kerala"
    scm <- unmet_s3() %>% 
      dplyr::filter(n5_state == panel3_n5_state(),cutpoint == input$cutoffinput3) %>% 
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
      cascade_plot(.,limits_y = c(0,100))
    figB <- state_cs_merge() %>% 
      dplyr::filter(stratification == "age_category") %>% 
      cascade_plot(.,limits_y = c(0,100))
    figC <- state_cs_merge() %>%
      dplyr::filter(stratification == "education") %>%
      mutate(group = factor(group, levels=c("Rural\nNo education","Rural\nPrimary","Rural\nSecondary","Rural\nHigher",
                                            "Urban\nNo education","Urban\nPrimary","Urban\nSecondary","Urban\nHigher"
      ))) %>% 
      dplyr::filter(!is.na(group)) %>% 
      cascade_plot(.,limits_y = c(0,100))
    figD <- state_cs_merge() %>%
      dplyr::filter(stratification == "caste") %>%
      cascade_plot(.,limits_y = c(0,100))
    figE <- state_cs_merge() %>%
      dplyr::filter(stratification == "swealthq_ur") %>%
      mutate(group = factor(group,
                            levels = paste0(rep(c("Rural","Urban"),each =5),
                                            "\n",
                                            rep(c("Wealth: Lowest","Wealth: Low",
                                                  "Wealth: Medium","Wealth: High",
                                                  "Wealth: Highest"),times=2)),ordered=TRUE)) %>% 
      dplyr::filter(!is.na(group)) %>% 
      cascade_plot(.,limits_y = c(0,100))
    
    ggarrange(figA,
              figB,
              figC,
              figD,
              figE,
              labels = LETTERS[1:5],ncol = 1,nrow=5,common.legend = TRUE,legend="top")
    
    
  })
  
  
})
