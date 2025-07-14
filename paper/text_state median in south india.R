state_htn <- read_csv(file="analysis/hca03_state level care cascade.csv") %>% 
  dplyr::filter(is.na(stratification)) %>% 
  mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
  dplyr::filter(variable == "Disease") %>% 
  mutate(variable = "Hypertension") 

state_htn %>% 
  group_by(zone=="South"|n5_state == "Goa",residence) %>% 
  summarize(median = median(estimate))

state_htn <- read_csv(file="analysis/hca11_total state level care cascade.csv") %>% 
  dplyr::filter(is.na(stratification)) %>% 
  mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
  dplyr::filter(variable == "Disease") %>% 
  mutate(variable = "Hypertension") 

state_htn %>% 
  group_by(zone=="South"|n5_state == "Goa") %>% 
  summarize(median = median(estimate),
            q25 = quantile(estimate,0.25),
            q75 = quantile(estimate,0.75))
