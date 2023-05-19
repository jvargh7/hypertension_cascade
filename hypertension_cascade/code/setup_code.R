state_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/smapsmaster_sp.RDS"))
saveRDS(state_shp,file="hypertension_cascade/data/state_shp.RDS")


district_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/dnfhs5_sp.RDS"))
saveRDS(district_shp,file="hypertension_cascade/data/district_shp.RDS")

# hca02_national -------
hca02_national <- read_csv(file = "analysis/hca02_national level care cascade.csv")  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(hca02_national,file="hypertension_cascade/data/hca02_national.RDS")

hcz01_national <- read_csv(file = "age_standardized/hcz01_age standardized national care cascade.csv")  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(hcz01_national,file="hypertension_cascade/data/hcz01_national.RDS")


national_nested <- bind_rows(
  read_csv("analysis/hca02_national level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "analysis/hca09_national met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(national_nested,file="hypertension_cascade/data/national_nested.RDS")

nationalz_nested <- bind_rows(
  read_csv("age_standardized/hcz01_age standardized national care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "age_standardized/hcz03_national met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")))  %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(nationalz_nested,file="hypertension_cascade/data/nationalz_nested.RDS")



# hca03_state ----------
hca03_state <- read_csv("analysis/hca03_state level care cascade.csv",guess_max = 6000) %>% 
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
saveRDS(hca03_state,file="hypertension_cascade/data/hca03_state.RDS")


hcz02_state <- read_csv("age_standardized/hcz02_age standardized state cascade.csv",guess_max = 6000) %>% 
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
saveRDS(hcz02_state,file="hypertension_cascade/data/hcz02_state.RDS")

# hca05_state - Unmet need ----------
hca05_state <- bind_rows(read_csv(file = "analysis/hca05_state unmet need care cascade.csv") %>% 
                           dplyr::filter(is.na(stratification)) %>% 
                           mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                         read_csv(file="analysis/hca03_state level care cascade.csv") %>% 
                           dplyr::filter(is.na(stratification)) %>% 
                           mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                           dplyr::filter(variable == "Disease") %>% 
                           mutate(variable = "Hypertension")
) %>% 
  # dplyr::filter(n > 100) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"))) 
saveRDS(hca05_state,file="hypertension_cascade/data/hca05_state_unmet.RDS")

# state_nested --------
state_nested <- bind_rows(
  read_csv("analysis/hca03_state level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv("analysis/hca11_total state level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "analysis/hca05_state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")),
  read_csv(file = "analysis/hca12_total state met need care cascade.csv") %>% 
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


saveRDS(state_nested,file="hypertension_cascade/data/state_nested.RDS")

# statez_nested: Age standardized --------
statez_nested <- bind_rows(
  read_csv("age_standardized/hcz02_age standardized state cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv("age_standardized/hcz15_age standardized total state cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "age_standardized/hcz04_state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("htn_diagnosed","htn_treated","htn_controlled")),
  read_csv(file = "age_standardized/hcz16_total state met need care cascade.csv") %>% 
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


saveRDS(statez_nested,file="hypertension_cascade/data/statez_nested.RDS")

# hca04_district --------
hca04_district <- read_csv("analysis/hca04_district2018 level care cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"htn_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Hypertension","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            TRUE ~ strata))  %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)
saveRDS(hca04_district,file="hypertension_cascade/data/hca04_district.RDS")

# hca08_district - Unmet need --------

hca08_district <- bind_rows(read_csv(file = "analysis/hca08_district unmet need care cascade.csv",guess_max = 6000) %>% 
                              mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                            read_csv(file="analysis/hca04_district2018 level care cascade.csv",guess_max = 6000) %>% 
                              mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                              dplyr::filter(variable == "Disease") %>% 
                              mutate(variable = "Hypertension")
) %>% 
  mutate(strata = case_when(is.na(strata) ~ "Total",
                            TRUE ~ strata)) %>% 
  # dplyr::filter(n > 100) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"))) %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)

saveRDS(hca08_district,file="hypertension_cascade/data/hca08_district_unmet.RDS")

# district_nested ---------

district_nested <- bind_rows(
  read_csv("analysis/hca04_district2018 level care cascade.csv",guess_max = 3000) %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "analysis/hca08_district met need care cascade.csv",guess_max = 3000) %>% 
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


saveRDS(district_nested,file="hypertension_cascade/data/district_nested.RDS")


# statez_nested: Age standardized --------
districtz_nested <- bind_rows(
  read_csv("age_standardized/hcz06_age standardized district cascade.csv",guess_max = 3000) %>% 
    dplyr::filter(variable %in% c("htn_screened","htn_disease")),
  read_csv(file = "age_standardized/hcz05_district met need care cascade.csv",guess_max = 3000) %>% 
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


saveRDS(districtz_nested,file="hypertension_cascade/data/districtz_nested.RDS")

# Copying functions --------
# if(Sys.info["user"]=="JVARGH7"){
#   file.copy("C:/code/external/nfhs_cascade/functions/cascade_plot.R",to = "hypertension_cascade/code/cascade_plot.R")
#   file.copy("C:/code/external/nfhs_cascade/diabetes_cascade/code/tmap plots.R",to = "hypertension_cascade/code/tmap plots.R")
#   
#   # Copying data -----------
#   file.copy("C:/code/external/nfhs_cascade/diabetes_cascade/data/maps.xlsx",to = "hypertension_cascade/data/maps.xlsx")
#   
#   
#   # Copying logos -------------
#   file.copy("C:/code/external/nfhs_cascade/diabetes_cascade/www/gdrc.png",to = "hypertension_cascade/www/gdrc.png")
#   file.copy("C:/code/external/nfhs_cascade/diabetes_cascade/www/Logo 1.jpg",to = "hypertension_cascade/www/Logo 1.jpg")
#   file.copy("C:/code/external/nfhs_cascade/diabetes_cascade/www/Logo 2.jpg",to = "hypertension_cascade/www/Logo 2.jpg")
#   file.copy("C:/code/external/nfhs_cascade/diabetes_cascade/www/Logo 3.jpg",to = "hypertension_cascade/www/Logo 3.jpg")
#   
#   
# }
