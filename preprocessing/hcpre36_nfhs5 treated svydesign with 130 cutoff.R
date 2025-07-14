
nfhs5htntreat_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women pre3.RDS")) %>% 
                                mutate(sex = "Female"),
                              readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men pre3.RDS")) %>% 
                                mutate(sex = "Male")) %>%
  dplyr::filter(htn_treated == 1) %>% 
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         htn_uncontrolled = 1 - htn_controlled,
         age_category2 = case_when(age %in% c(18:39) ~ 1,
                                   age %in% c(40:64) ~ 2,
                                   age >= 65 ~ 2,
                                   TRUE ~ NA_real_)) %>% 
  mutate(age_category2 = factor(age_category2,levels=c(1:2),labels=c("18-39","40 plus"))) %>%  
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

nfhs5htntreat_svydesign <- nfhs5htntreat_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer")
