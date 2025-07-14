source(".Rprofile")

absolute_df <- bind_rows(
  read_csv(file = "analysis/hca02_national total.csv"),
  read_csv(file = "analysis/hca02_national level care cascade.csv")) %>% 
  dplyr::mutate(residence = case_when(is.na(residence) ~ "Total",
                                      TRUE ~ residence)) %>% 
  dplyr::filter(is.na(strata),variable != "dm_screened") %>% 
  mutate(variable = factor(variable,
                           levels=c("htn_disease","htn_diagnosed","htn_treated","htn_controlled"),
                           labels=c("Hypertension","Diagnosed","Treated","Controlled")),
         residence = factor(residence,
                            levels=c("Total","Urban","Rural"))) %>% 
  arrange(residence,variable) %>% 
  group_by(residence) %>% 
  mutate(denominator = case_when(variable == "Hypertension" ~ estimate,
                                 TRUE ~ NA_real_)) %>% 
  mutate(denominator = zoo::na.locf(denominator)) %>% 
  mutate(drop = (1-(estimate*100/denominator)))
