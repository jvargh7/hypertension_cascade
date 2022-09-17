lasipop_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                          mutate(sex = "Female"),
                        readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                          mutate(sex = "Male")) %>%
  dplyr::filter(age >= 45) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         status = case_when(is.na(htn_free) ~ "Excluded",
                            age < 45 ~ "Excluded",
                            TRUE ~ "Analytic")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE)  %>% 
  left_join(readRDS(paste0(path_cascade_folder,"/working/ipw_df.RDS")) %>% 
              dplyr::select(psu,hhid,linenumber,sampleweight_ipw))



lasipop_design <- lasipop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# IPW -------------
lasipopipw_design <- lasipop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight_ipw,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# Analytic only --------
lasi_df <- lasipop_df  %>% 
  dplyr::filter(status == "Analytic") 

lasi_design <- lasi_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

lasi_htn_design <- lasi_df %>% 
  dplyr::filter(htn_disease == 1) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")