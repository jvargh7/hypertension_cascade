nfhs4pop_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                          mutate(sex = "Female"),
                        readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                          mutate(sex = "Male")) %>%
  dplyr::filter(age %in% c(18:49)) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         status = case_when(is.na(htn_free) ~ "Excluded",
                            age <18 | age>49 ~ "Excluded",
                            TRUE ~ "Analytic")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE) %>% 
  mutate(htn_disease_cat = case_when(is.na(htn_disease) ~ "Missing",
                                     htn_disease == 1 ~ "Yes",
                                     htn_disease == 0 ~ "No")) %>% 
  left_join(readRDS(paste0(path_cascade_folder,"/working/ipw_df.RDS")) %>% 
              dplyr::select(psu,hhid,linenumber,sampleweight_ipw))




nfhs4pop_design <- nfhs4pop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# IPW -------------
nfhs4popipw_design <- nfhs4pop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight_ipw,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# Analytic only --------
nfhs4_df <- nfhs4pop_df  %>% 
  dplyr::filter(status == "Analytic") 

nfhs4_design <- nfhs4_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

nfhs4_htn_design <- nfhs4_df %>% 
  dplyr::filter(htn_disease == 1) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")