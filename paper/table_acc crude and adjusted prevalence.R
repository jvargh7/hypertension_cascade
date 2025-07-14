# nfhs5_svysummary <- read_csv("analysis/nca02_national level care cascade.csv")
population <- read_csv("cutoff 130 and 80/hcc130z01_age standardized national care cascade.csv") %>% 
  dplyr::filter(variable %in% c("htn_disease"))

pop_total = read_csv("cutoff 130 and 80/hcc130z01_total age standardized national care cascade.csv") %>% 
  dplyr::filter(variable %in% c("htn_disease")) %>% 
  mutate(stratification = case_when(sex %in% c("Female","Male") ~ "sex",
                                    TRUE ~ NA_character_)) %>% 
  rename(strata = sex) %>% 
  dplyr::filter(is.na(strata))

# nested <- read_csv(file = "cutoff 130 and 80/hcc130z03_national met need care cascade.csv") %>% 
#   dplyr::filter(variable %in% c("htn_diagnosed","htn_treated"))
# 
# conditional <- bind_rows(
#   read_csv(file = "cutoff 130 and 80/hcc130z08_total age standardized national conditional cascade.csv"),
#   read_csv(file = "cutoff 130 and 80/hcc130z08_national conditional cascade.csv")) %>% 
#   dplyr::filter(variable %in% c("htn_controlled"))

bind_rows(population,
          pop_total
          # nested,
          # conditional
          ) %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence)) %>% 
  arrange(residence) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_hcc130 age adjusted prevalence.csv")


# CRUDE --------------

# nfhs5_svysummary <- read_csv("analysis/nca02_national level care cascade.csv")
population_crude <- read_csv("cutoff 130 and 80/hcc130a02_national level care cascade.csv") %>% 
  dplyr::filter(variable %in% c("htn_disease"))

pop_total_crude = read_csv("cutoff 130 and 80/hcc130a02_national total.csv") %>% 
  dplyr::filter(variable %in% c("htn_disease")) 

# nested <- read_csv(file = "cutoff 130 and 80/hcc130z03_national met need care cascade.csv") %>% 
#   dplyr::filter(variable %in% c("htn_diagnosed","htn_treated"))
# 
# conditional <- bind_rows(
#   read_csv(file = "cutoff 130 and 80/hcc130z08_total age standardized national conditional cascade.csv"),
#   read_csv(file = "cutoff 130 and 80/hcc130z08_national conditional cascade.csv")) %>% 
#   dplyr::filter(variable %in% c("htn_controlled"))

bind_rows(population_crude,
          pop_total_crude
          # nested,
          # conditional
) %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence)) %>% 
  arrange(residence) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_hcc130 crude prevalence.csv")

