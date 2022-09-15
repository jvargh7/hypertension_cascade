
nfhs5_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                               mutate(sex = "Female"),
                             readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                               mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(htn_free)) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE) %>% 
  mutate(dm_disease_cat = case_when(is.na(dm_disease) ~ "Missing",
                                    dm_disease == 1 ~ "Yes",
                                    dm_disease == 0 ~ "No"))



nfhs5_svydesign <- nfhs5_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

