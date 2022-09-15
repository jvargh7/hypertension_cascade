
nfhs5pop_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                        mutate(sex = "Female"),
                      readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                        mutate(sex = "Male")) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         status = case_when(is.na(htn_free) ~ "Excluded",
                            !is.na(htn_free) ~ "Analytic")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE)

nfhs5pop_svydesign <- nfhs5pop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")
