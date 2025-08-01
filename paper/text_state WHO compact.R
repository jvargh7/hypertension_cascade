state_met <- read_csv("analysis/hca05_state met need care cascade.csv",guess_max = 4000) %>% 
  dplyr::filter(is.na(strata))

state_met %>% 
  mutate(n_category = case_when(n >= 50 ~ 1,
                                n %in% c(25:49) ~ 2,
                                n < 25 ~ 3)) %>% 
  mutate(n_category = factor(n_category,labels=c(">=50","25-49"))) %>% 
  group_by(variable,n_category) %>% 
  tally() %>% 
  ggplot(data=.,aes(x = variable,y=n,group=n_category,fill=n_category,label=n)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_text(position = position_dodge(width=0.9)) +
  theme_bw() +
  xlab("cascade level")



state_met %>% 
  # dplyr::filter(is.na(stratification), n >= 50) %>%
  group_by(residence,variable) %>% 
  summarize(count = sum(estimate > 80),
            prop = sum(estimate > 80)/n(),
            n = n()) 


state_met %>% 
  dplyr::filter(is.na(stratification), n >= 50) %>%
  group_by(residence,zone, variable) %>% 
  summarize(count = sum(estimate > 80),
            prop = sum(estimate > 80)/n(),
            median = median(estimate),
            n = n()) %>% 
  dplyr::filter(variable == "htn_treated")


state_met %>% 
  dplyr::filter(is.na(stratification), n >= 50) %>%
  group_by(residence,zone, variable) %>% 
  summarize(count = sum(estimate > 80),
            prop = sum(estimate > 80)/n(),
            median = median(estimate),
            n = n()) %>% 
  dplyr::select(residence,zone,variable,median) %>% 
  pivot_wider(names_from=residence,values_from=median) %>% 
  mutate(urb_min_rur = Urban - Rural) %>% View()



