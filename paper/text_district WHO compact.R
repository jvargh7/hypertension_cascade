district_met <- read_csv("analysis/hca08_district met need care cascade.csv",guess_max = 4000) %>% 
  dplyr::filter(is.na(strata))

district_met %>% 
  mutate(n_category = case_when(n >= 50 ~ 1,
                                n %in% c(25:49) ~ 2,
                                n < 25 ~ 3)) %>% 
  mutate(n_category = factor(n_category,levels=c(1:3),labels=c(">=50","25-49","<25"))) %>% 
  group_by(variable,n_category) %>% 
  tally() %>% 
  ggplot(data=.,aes(x = variable,y=n,group=n_category,fill=n_category,label=n)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_text(position = position_dodge(width=0.9)) +
  theme_bw() +
  xlab("cascade level")



district_met %>% 
  # dplyr::filter(is.na(stratification), n >= 50) %>%
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

district_met %>% 
  dplyr::filter(is.na(stratification)) %>% 
  group_by(variable) %>% 
  summarize(min = min(estimate),
            max = max(estimate))

library(lme4)
m_diag = district_met %>% 
  dplyr::filter(variable == "htn_diagnosed") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) 

icc_diag = m_diag %>% 
  performance::icc(.)

1- icc_diag$ICC_adjusted

m_treat = district_met %>% 
  dplyr::filter(variable == "htn_treated") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) 

icc_treat = m_treat %>% 
  performance::icc(.)

1-icc_treat

m_contr = district_met %>% 
  dplyr::filter(variable == "htn_controlled") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) 

icc_contr = m_contr %>% 
  performance::icc(.)
1-icc_contr

