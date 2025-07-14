rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/functions/survey/svysummary.R")

nfhs5_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                        mutate(sex = "Female"),
                      readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                        mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(htn_free),age >=40) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) %>% 
  mutate(htn_disease_cat = case_when(is.na(htn_disease) ~ "Missing",
                                     htn_disease == 1 ~ "Yes",
                                     htn_disease == 0 ~ "No"))

# with(nfhs5_df,table(age <= 49, !is.na(bmi)))

proportion_vars <- c("htn_disease")


nfhs5_svydesign <- nfhs5_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

n5_sy <- svysummary(nfhs5_svydesign,
                    # c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    # g_vars = grouped_vars,
                    # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))




