
# hca02_national -------
hca02_national <- read_csv(file = "cutoff 130 and 80/hcc130a02_national level care cascade.csv")  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(hca02_national,file="hypertension_cascade/cutoff130/hcc130a02_national.RDS")

hcz01_national <- read_csv(file = "cutoff 130 and 80/hcc130z01_age standardized national care cascade.csv")  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(hcz01_national,file="hypertension_cascade/cutoff130/hcc130z01_national.RDS")


national_nested <- bind_rows(
  read_csv("cutoff 130 and 80/hcc130a02_national level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "cutoff 130 and 80/hcc130a09_national met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(national_nested,file="hypertension_cascade/cutoff130/national_nested.RDS")

nationalz_nested <- bind_rows(
  read_csv("cutoff 130 and 80/hcc130z01_age standardized national care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "cutoff 130 and 80/hcc130z03_national met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(nationalz_nested,file="hypertension_cascade/cutoff130/nationalz_nested.RDS")



# hca03_state ----------
hca03_state <- read_csv("cutoff 130 and 80/hcc130a03_state level care cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 
saveRDS(hca03_state,file="hypertension_cascade/cutoff130/hcc130a03_state.RDS")


hcz02_state <- read_csv("cutoff 130 and 80/hcc130z02_age standardized state cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 
saveRDS(hcz02_state,file="hypertension_cascade/cutoff130/hcc130z02_state.RDS")

# hca05_state - Unmet need ----------
hca05_state <- bind_rows(read_csv(file = "cutoff 130 and 80/hcc130a05_state unmet need care cascade.csv") %>% 
                           dplyr::filter(is.na(stratification)) %>% 
                           mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                         read_csv(file="cutoff 130 and 80/hcc130a03_state level care cascade.csv") %>% 
                           dplyr::filter(is.na(stratification)) %>% 
                           mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                           dplyr::filter(variable == "Disease") %>% 
                           mutate(variable = "Hypertension")
) %>% 
  # dplyr::filter(n > 100) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"))) 
saveRDS(hca05_state,file="hypertension_cascade/cutoff130/hcc130a05_state_unmet.RDS")

# state_nested --------
state_nested <- bind_rows(
  read_csv("cutoff 130 and 80/hcc130a03_state level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv("cutoff 130 and 80/hcc130a11_total state level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "cutoff 130 and 80/hcc130a05_state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")),
  read_csv(file = "cutoff 130 and 80/hcc130a12_total state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 


saveRDS(state_nested,file="hypertension_cascade/cutoff130/state_nested.RDS")

# statez_nested: Age standardized --------
statez_nested <- bind_rows(
  read_csv("cutoff 130 and 80/hcc130z02_age standardized state cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv("cutoff 130 and 80/hcc130z15_age standardized total state cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "cutoff 130 and 80/hcc130z04_state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")),
  read_csv(file = "cutoff 130 and 80/hcc130z16_total state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 


saveRDS(statez_nested,file="hypertension_cascade/cutoff130/statez_nested.RDS")

# hca04_district --------
hca04_district <- read_csv("cutoff 130 and 80/hcc130a04_district2018 level care cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            TRUE ~ strata))  %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)
saveRDS(hca04_district,file="hypertension_cascade/cutoff130/hcc130a04_district.RDS")

# hca08_district - Unmet need --------

hca08_district <- bind_rows(read_csv(file = "cutoff 130 and 80/hcc130a08_district unmet need care cascade.csv",guess_max = 6000) %>% 
                              mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                            read_csv(file="cutoff 130 and 80/hcc130a04_district2018 level care cascade.csv",guess_max = 6000) %>% 
                              mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                              dplyr::filter(variable == "Disease") %>% 
                              mutate(variable = "Hypertension")
) %>% 
  mutate(strata = case_when(is.na(strata) ~ "Total",
                            TRUE ~ strata)) %>% 
  # dplyr::filter(n > 100) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"))) %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)

saveRDS(hca08_district,file="hypertension_cascade/cutoff130/hcc130a08_district_unmet.RDS")

# district_nested ---------

district_nested <- bind_rows(
  read_csv("cutoff 130 and 80/hcc130a04_district2018 level care cascade.csv",guess_max = 3000) %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "cutoff 130 and 80/hcc130a08_district met need care cascade.csv",guess_max = 3000) %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))  %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)


saveRDS(district_nested,file="hypertension_cascade/cutoff130/district_nested.RDS")


# statez_nested: Age standardized --------
districtz_nested <- bind_rows(
  read_csv("cutoff 130 and 80/hcc130z06_age standardized district cascade.csv",guess_max = 3000) %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "cutoff 130 and 80/hcc130z05_district met need care cascade.csv",guess_max = 3000) %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)


saveRDS(districtz_nested,file="hypertension_cascade/cutoff130/districtz_nested.RDS")

