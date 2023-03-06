# nfhs5_svysummary <- read_csv("analysis/hca02_national level care cascade.csv")
population <- read_csv("age_standardized/hcz11_age standardized national care cascade with last2bp.csv") %>% 
  dplyr::filter(variable %in% c("htn_screened","htn_disease"))

nested <- read_csv(file = "age_standardized/hcz13_national met need care cascade with last2bp.csv") %>% 
  dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled"))


bind_rows(population,
          nested) %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence)) %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  arrange(residence) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_sociodemographic population care cascade with last2bp.csv")
