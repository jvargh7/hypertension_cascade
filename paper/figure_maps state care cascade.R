unmet_cascade <- bind_rows(read_csv(file = "analysis/hca05_state unmet need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                           read_csv(file="analysis/hca03_state level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(n >= 50) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled")))

source("C:/code/external/nfhs_cascade/functions/state_map.R")

         

# figA <- state_cascade %>% 
#   dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "htn_screened",plot_title = "A. Rural Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

# figB <- state_cascade %>% 
#   dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "htn_screened",plot_title = "B. Urban Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

figA <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Hypertension",plot_title = "A. Rural Hypertension",breaks = seq(0,40,by=10),palette_chr = "-RdYlGn")

figB <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Undiagnosed",plot_title = "B. Rural Undiagnosed",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figC <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Untreated",plot_title = "C. Rural Unreated",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figD <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Uncontrolled",plot_title = "D. Rural Uncontrolled",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")


figE <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Hypertension",plot_title = "E. Urban Hypertension",breaks = seq(0,40,by=10),palette_chr = "-RdYlGn")

figF <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Undiagnosed",plot_title = "F. Urban Undiagnosed",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figG <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Untreated",plot_title = "G. Urban Untreated",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")



figH <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Uncontrolled",plot_title = "H. Urban Uncontrolled",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

tmap_arrange(
             figA,figB,figC,figD,
             figE,figF,figG,figH,
             ncol = 4,nrow=2) %>% 
  tmap_save(.,filename=paste0(path_cascade_folder,"/figures/figure_state care cascade urban rural.png"),width=28,height=15,dpi=200)

# State

unmet_cascade_total <- bind_rows(read_csv(file = "analysis/hca12_total state unmet need care cascade.csv") %>% 
                                   dplyr::filter(is.na(stratification)) %>% 
                                   mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                                 read_csv(file="analysis/hca11_total state level care cascade.csv") %>% 
                                   dplyr::filter(is.na(stratification)) %>% 
                                   mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                                   dplyr::filter(variable == "Disease") %>% 
                                   mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(n >= 25) %>%
  dplyr::filter(!is.na(variable)) %>% 
  dplyr::filter(variable !="Unscreened") 

figAT <- unmet_cascade_total %>% 
  dplyr::filter(is.na(stratification)) %>% 
  state_map(.,plot_variable = "Hypertension",plot_title = "A. Hypertension",breaks = seq(0,40,by=10),palette_chr = "-RdYlGn",include_state_names=FALSE)

figBT <- unmet_cascade_total %>% 
  dplyr::filter(is.na(stratification)) %>% 
  state_map(.,plot_variable = "Undiagnosed",plot_title = "B. Undiagnosed",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn",include_state_names=FALSE)

figCT <- unmet_cascade_total %>% 
  dplyr::filter(is.na(stratification)) %>% 
  state_map(.,plot_variable = "Untreated",plot_title = "C. Untreated",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn",include_state_names=FALSE)

figDT <- unmet_cascade_total %>% 
  dplyr::filter(is.na(stratification)) %>% 
  state_map(.,plot_variable = "Uncontrolled",plot_title = "D. Uncontrolled",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn",include_state_names=FALSE)

tmap_arrange(
  figAT,figBT,figCT,figDT,
  ncol = 2,nrow=2) %>% 
  tmap_save(.,filename=paste0(path_cascade_folder,"/figures/figure_state care cascade.png"),height=14,width=14)

tmap_arrange(
  figAT,figBT,figCT,figDT,
  ncol = 2,nrow=2) %>% 
  tmap_save(.,filename=paste0(path_cascade_folder,"/writing/JAMA Network Open R1/Figure 2.pdf"),height=14,width=14)
