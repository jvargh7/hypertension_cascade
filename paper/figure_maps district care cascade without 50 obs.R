unmet_cascade <- bind_rows(read_csv(file = "analysis/hca08_district unmet need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                           read_csv(file="analysis/hca04_district2018 level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(n >= 50) %>%
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled")))

source(paste0(path_dmcascade_repo,"/functions/district_map.R"))


figA <- unmet_cascade %>% 
  dplyr::filter(is.na(stratification)) %>% 
  district_map(.,plot_variable = "Hypertension",plot_title = "A. Hypertension",breaks = seq(0,40,by=10),palette_chr = "-RdYlGn")

figB <- unmet_cascade %>% 
  dplyr::filter(is.na(stratification)) %>% 
  district_map(.,plot_variable = "Undiagnosed",plot_title = "B. Undiagnosed",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figC <- unmet_cascade %>% 
  dplyr::filter(is.na(stratification)) %>% 
  district_map(.,plot_variable = "Untreated",plot_title = "C. Unreated",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figD <- unmet_cascade %>% 
  dplyr::filter(is.na(stratification)) %>% 
  district_map(.,plot_variable = "Uncontrolled",plot_title = "D. Uncontrolled",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")


tmap_arrange(
  figA,figB,figC,figD,
  ncol = 2,nrow=2) %>% 
  tmap_save(.,filename=paste0(path_cascade_folder,"/figures/figure_district care cascade without 50 obs.png"),height=14,width=14)