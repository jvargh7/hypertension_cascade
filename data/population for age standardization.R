source("preprocessing/hcpre03_nfhs5 total svydesign.R")

pop_age <- nfhs5_svydesign %>% 
  group_by(age_category) %>% 
  survey_tally()

write_csv(pop_age,"data/population for age standardization.csv")
