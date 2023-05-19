
nfhs5htntreat_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                               mutate(sex = "Female"),
                             readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
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
  rename(district_df = REGCODE)

nfhs5htntreat_svydesign <- nfhs5htntreat_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer")
