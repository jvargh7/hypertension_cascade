# nfhs5_svysummary <- read_csv("analysis/nca02_national level care cascade.csv")
population <- read_csv("age_standardized/hcz01_age standardized national care cascade.csv") %>% 
  dplyr::filter(variable %in% c("htn_disease"))

nested <- read_csv(file = "age_standardized/hcz03_national met need care cascade.csv") %>% 
  dplyr::filter(variable %in% c("htn_diagnosed","htn_treated"))

conditional <- bind_rows(
  read_csv(file = "age_standardized/hcz08_total age standardized national conditional cascade.csv"),
  read_csv(file = "age_standardized/hcz08_national conditional cascade.csv")) %>% 
  dplyr::filter(variable %in% c("htn_controlled"))

bind_rows(population,
          nested,
          conditional) %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence)) %>% 
  arrange(residence) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_conditional care cascade.csv")
