
nfhs5htndiag_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women pre2.RDS")) %>% 
                               mutate(sex = "Female"),
                             readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men pre2.RDS")) %>% 
                               mutate(sex = "Male")) %>%
  dplyr::filter(htn_diagnosed == 1) %>% 
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         htn_untreated = 1 - htn_treated,
         htn_uncontrolled = 1 - htn_controlled) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) %>% 
  mutate(htn_diagnosis_type = case_when(htn_treated == 1 & htn_controlled == 1 ~ 1,
                                        htn_treated == 1 & htn_controlled == 0 ~ 2,
                                        htn_treated == 0 & htn_controlled == 1 ~ 3,
                                        htn_treated == 0 & htn_controlled == 0 ~ 3,
                                        TRUE ~ NA_real_)) %>% 
  mutate(htn_diagnosis_type = factor(htn_diagnosis_type,levels=c(1:3),
                                     labels=c("Treated & Controlled",
                                              "Treated & Uncontrolled",
                                              # "Untreated & Controlled",
                                              # "Untreated & Uncontrolled",
                                              "Untreated"),ordered=TRUE))

nfhs5htndiag_svydesign <- nfhs5htndiag_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")
