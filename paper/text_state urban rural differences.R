unmet_cascade <- bind_rows(read_csv(file = "analysis/hca05_state unmet need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                           read_csv(file="analysis/hca03_state level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(n >= 50) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled")))

# Differences

unmet_cascade %>% 
  dplyr::filter(is.na(stratification), n >= 50) %>%
  dplyr::select(residence,variable,zone,state,estimate) %>% 
  pivot_wider(names_from=residence,values_from=estimate) %>% 
  mutate(urb_min_rur = Urban - Rural,
         urb_gt_rur = case_when(Urban > Rural ~ 1,
                                Urban <= Rural ~ 0,
                                TRUE ~ NA_real_)) %>% 
  group_by(variable,zone) %>% 
  summarize(diff = median(urb_min_rur,na.rm=TRUE),
            sum = sum(urb_gt_rur,na.rm=TRUE)) %>% View()

unmet_cascade %>% 
  dplyr::filter(is.na(stratification), n >= 50) %>%
  dplyr::select(residence,variable,zone,state,estimate) %>% 
  pivot_wider(names_from=residence,values_from=estimate) %>% 
  mutate(urb_min_rur = Urban - Rural,
         urb_gt_rur = case_when(Urban > Rural ~ 1,
                                Urban <= Rural ~ 0,
                                TRUE ~ NA_real_)) %>% 
  group_by(variable) %>% 
  summarize(diff = median(urb_min_rur,na.rm=TRUE),
            sum = sum(urb_gt_rur,na.rm=TRUE),
            count_diff = sum(!is.na(urb_min_rur)))



# Results: State-level care cascade ---------

unmet_cascade %>% 
  dplyr::filter(variable == "Unscreened",is.na(stratification),n>100) %>% 
  dplyr::select(state,residence,estimate) %>% 
  # Count of states with unscreened >= 20
  mutate(cutoff = case_when(estimate < 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))

unmet_cascade %>% 
  dplyr::filter(variable == "Undiagnosed",is.na(stratification),n>100) %>% 
  dplyr::select(state,residence,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))


unmet_cascade %>% 
  dplyr::filter(variable == "Untreated",is.na(stratification),n>100) %>% 
  dplyr::select(state,residence,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))


unmet_cascade %>% 
  dplyr::filter(variable == "Uncontrolled",is.na(stratification),n>100) %>% 
  dplyr::select(state,residence,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))



unmet_cascade %>% 
  dplyr::filter(variable == "Untreated",is.na(stratification),n>100) %>% 
  dplyr::select(state,residence,zone,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence,zone) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)),
            median = median(estimate)) %>% 
  View()
