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




fig_prevalence <- unmet_cascade %>% 
  dplyr::filter(variable == "Hypertension") %>% 
  ggplot(data=.,aes(x = n5_state,y = estimate,ymin = lci,ymax=uci,
                    group=interaction(residence,n5_state),
                    fill=residence)) +
  geom_col(position=position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width=0.1) +
  theme_bw() + 
  coord_flip() +
  facet_grid(zone~variable,scales="free",space="free_y") +
  scale_y_continuous(limits=c(0,80),breaks=seq(0,80,by=20)) +
  facet_grid(zone~variable,scales="free_y",space="free_y") +
  scale_fill_manual(name="",values=c("lightblue","orange")) +
  scale_shape_discrete(name="") +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_blank(),
    legend.position = "bottom") +
  # scale_y_continuous(limits=c(0,50)) +
  ylab("Prevalence (%)") +
  xlab("") 

fig_uc <- unmet_cascade %>% 
  dplyr::filter(variable != "Hypertension") %>% 
  ggplot(data=.,aes(x = n5_state,y = estimate,ymin = lci,ymax=uci,
                    group=interaction(residence,n5_state),
                    fill=residence)) +
  geom_col(position=position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width=0.1) +
  theme_bw() + 
  coord_flip() +
  facet_grid(zone~variable,scales="free",space="free_y") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20)) +
  scale_fill_manual(name="",values=c("lightblue","orange")) +
  scale_shape_discrete(name="") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size=12),
    legend.position = "bottom") +
  # scale_y_continuous(limits=c(0,50)) +
  ylab("Prevalence (%)") +
  xlab("") 

require(ggpubr)
ggarrange(fig_prevalence,fig_uc,nrow=1,ncol=2,
          common.legend = TRUE,legend="bottom",
          widths = c(1.5,2)) %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_column cascade.png"),width=15,height=8)

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
