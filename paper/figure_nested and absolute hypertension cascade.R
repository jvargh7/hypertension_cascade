source(".Rprofile")

absolute_df <- bind_rows(
  read_csv(file = "age_standardized/hcz01_total age standardized national care cascade.csv"),
  read_csv(file = "age_standardized/hcz01_age standardized national care cascade.csv")) %>% 
  dplyr::mutate(residence = case_when(is.na(residence) ~ "Total",
                                      TRUE ~ residence)) %>% 
  dplyr::filter(is.na(strata),variable != "htn_screened",is.na(sex)) %>% 
  mutate(variable = factor(variable,
                           levels=c("htn_disease","htn_diagnosed","htn_treated","htn_controlled"),
                           labels=c("Hypertension","Diagnosed","Treated","Controlled")),
         residence = factor(residence,
                            levels=c("Total","Urban","Rural"))) %>% 
  arrange(residence,variable) %>% 
  group_by(residence) %>% 
  mutate(denominator = case_when(variable == "Hypertension" ~ estimate,
                                 TRUE ~ NA_real_)) %>% 
  mutate(denominator = zoo::na.locf(denominator)) %>% 
  mutate(drop = (1-(estimate*100/denominator)))

fig_absolute <- absolute_df %>% 
  ggplot(data=.,aes(x=residence,y=estimate,ymin=lci,ymax=uci,fill=variable,group=variable)) +
  geom_col(position=position_dodge(width=0.9),width=0.6,col="black") +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=c("#375a66","#698994","#cad8de","#eff3f5")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(2,"picas"),
        legend.text = element_text(size=16),
        panel.grid.major.y = element_line(color="grey80"),
        axis.text = element_text(size=16)
  ) +
  scale_y_continuous(breaks=seq(0,35,by=5),limits=c(0,35))

fig_absolute
ggsave(fig_absolute,filename = paste0(path_cascade_folder,"/figures/nested and absolute hypertension cascade.png"),width=17.53,height=6.27)  


change_from_total = bind_rows(
  read_csv(file = "age_standardized/hcz14_total age standardized estimates among hypertension.csv"),
  read_csv(file = "age_standardized/hcz14_age standardized estimates among hypertension.csv")
  
) %>% 
  dplyr::mutate(residence = case_when(is.na(residence) ~ "Total",
                                      TRUE ~ residence)) %>% 
  dplyr::filter(is.na(strata),variable != "htn_screened",is.na(sex)) %>% 
  mutate(variable = factor(variable,
                           levels=c("htn_diagnosed","htn_treated","htn_controlled"),
                           labels=c("Diagnosed","Treated","Controlled")),
         residence = factor(residence,
                            levels=c("Total","Urban","Rural"))) %>% 
  dplyr::select(variable,estimate,residence) %>% 
  mutate(estimate = 100-estimate) %>% 
  pivot_wider(names_from=residence,values_from=estimate)
write_csv(change_from_total,"paper/table_drops for nested and absolute hypertension.csv")
