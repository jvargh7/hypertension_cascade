
nfhs5htntreat_df <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                               mutate(sex = "Female"),
                             readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                               mutate(sex = "Male")) %>%
  dplyr::filter(htn_treated == 1) %>% 
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         htn_uncontrolled = 1 - htn_controlled) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE)

nfhs5htntreat_svydesign <- nfhs5htntreat_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer")
