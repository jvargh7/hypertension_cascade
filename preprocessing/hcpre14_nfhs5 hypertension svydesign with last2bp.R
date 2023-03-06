
nfhs5htn_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women last2bp.RDS")) %>% 
                           mutate(sex = "Female"),
                         readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men last2bp.RDS")) %>% 
                           mutate(sex = "Male")) %>%
  dplyr::filter(htn_disease == 1) %>% 
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         htn_unscreened = 1 - htn_screened,
         htn_undiagnosed = 1 - htn_diagnosed) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE)

nfhs5htn_svydesign <- nfhs5htn_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")
