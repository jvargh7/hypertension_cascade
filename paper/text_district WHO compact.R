district_met <- read_csv("analysis/hca08_district met need care cascade.csv",guess_max = 4000) %>% 
  dplyr::filter(is.na(strata))

district_met %>% 
  mutate(n_category = case_when(n >= 50 ~ 1,
                                n %in% c(25:49) ~ 2,
                                n < 25 ~ 3)) %>% 
  mutate(n_category = factor(n_category,labels=c(">=50","25-49","<25"))) %>% 
  group_by(variable,n_category) %>% 
  tally() %>% 
  ggplot(data=.,aes(x = variable,y=n,group=n_category,fill=n_category,label=n)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_text(position = position_dodge(width=0.9)) +
  theme_bw() +
  xlab("cascade level")



district_met %>% 
  dplyr::filter(is.na(stratification), n >= 50) %>%
  group_by(variable) %>% 
  summarize(count = sum(estimate > 80),
            prop = sum(estimate > 80)/n(),
            n = n()) 

district_met %>% 
  dplyr::filter(is.na(stratification)) %>%
  group_by(variable) %>% 
  summarize(count = sum(estimate > 80),
            prop = sum(estimate > 80)/n(),
            n = n()) 

# Between district variation --------

require(lme4)
district_met %>% 
  dplyr::filter(variable == "htn_diagnosed") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) %>% 
  performance::icc(.)

district_met %>% 
  dplyr::filter(variable == "htn_treated") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) %>% 
  performance::icc(.)

district_met %>% 
  dplyr::filter(variable == "htn_controlled") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) %>% 
  performance::icc(.)