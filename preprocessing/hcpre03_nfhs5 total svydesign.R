
nfhs5_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                               mutate(sex = "Female"),
                             readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                               mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(htn_free)) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) %>% 
  mutate(dm_disease_cat = case_when(is.na(dm_disease) ~ "Missing",
                                    dm_disease == 1 ~ "Yes",
                                    dm_disease == 0 ~ "No")) %>% 
  mutate(bp_group = case_when(sbp>= 160 | dbp >= 100 ~ 4,
                              sbp >= 140 | dbp >= 90 ~ 3,
                              sbp >= 120 | dbp >= 80 ~ 2,
                              sbp < 120 | dbp < 80 ~ 1,
                              TRUE ~ NA_real_)) %>% 
  mutate(bp_group = factor(bp_group,levels=c(1:4),labels=c("<120/80","<140/90","<160/100",">=160/100")))



nfhs5_svydesign <- nfhs5_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

