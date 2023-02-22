population = read_csv(paste0(path_cascade_folder,"/working/eaa34925-403e-4757-9cd4-a4826203e763_Data.csv")) %>% 
  dplyr::select('Series Name','Series Code','2021 [YR2021]') %>% 
  rename(series_code = 'Series Code',
         series_name = 'Series Name',
         yr2021 = '2021 [YR2021]') %>% 
  dplyr::filter(!is.na(yr2021)) %>% 
  as.data.frame()

pop_18plus = population %>% 
  dplyr::select(-series_name) %>% 
  pivot_wider(names_from=series_code,values_from=yr2021) %>% 
  mutate_all(~as.numeric(.))

# Total - (0-14) - (15-17)
est_18plus = pop_18plus$SP.POP.TOTL - (pop_18plus$SP.POP.0014.FE.IN + pop_18plus$SP.POP.0014.MA.IN) - (3/500)*(pop_18plus$SP.POP.1519.FE.5Y*pop_18plus$SP.POP.TOTL.FE.IN +
                                                                                                  pop_18plus$SP.POP.1519.MA.5Y*pop_18plus$SP.POP.TOTL.MA.IN)

hypertension <- read_csv("age_standardized/hcz01_total age standardized national care cascade.csv") %>% 
  dplyr::filter(variable %in% c("htn_disease"),is.na(sex))

diagnosed <- read_csv(file = "paper/abstract_national cascade.csv") %>% 
  dplyr::filter(variable %in% c("htn_diagnosed"),is.na(strata))

count_hypertension = est_18plus*(hypertension$estimate/100)
count_undiagnosed = count_hypertension*(1-(diagnosed$estimate/100))
