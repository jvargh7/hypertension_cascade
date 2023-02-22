district_met <- bind_rows(read_csv(file = "analysis/hca08_district met need care cascade.csv") %>% 
                            dplyr::filter(is.na(stratification)) %>% 
                            mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                          read_csv(file="analysis/hca04_district2018 level care cascade.csv") %>% 
                            dplyr::filter(is.na(stratification)) %>% 
                            mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                            dplyr::filter(variable == "Disease") %>% 
                            mutate(variable = "Hypertension")
) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Diagnosed","Treated","Controlled"))) %>% 
  mutate(ml_comparison = case_when(n5_state == "Meghalaya" & str_detect(REGNAME,"Garo") ~ "Garo Hills",
                                   n5_state == "Meghalaya" & str_detect(REGNAME,"(Jaintia|Jantia|Khasi)") ~ "Jaintia and Khasi Hills",
                                   TRUE ~ "Other"),
         
         ka_comparison = case_when(n5_state == "Karnataka" & REGNAME %in% c("Chikmagalur","Udupi") ~ "Chikmagalur-Udupi",
                                   n5_state == "Karnataka" & REGNAME %in% c("Chitradurga","Shimoga") ~ "Chitradurga-Shimoga",
                                   TRUE ~ "Other")) %>% 
  left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),sheet="map2020_v024") %>% 
              dplyr::select(n5_state,zone) %>% 
              distinct(n5_state,.keep_all=TRUE),
            by=c("n5_state"))

district_met %>% 
  mutate(ml_comparison2 = case_when(n5_state == "Meghalaya" & str_detect(REGNAME,"Garo") ~ "Garo Hills",
                                   n5_state == "Meghalaya" & str_detect(REGNAME,"(Jaintia|Jantia)") ~ "Jaintia Hills",
                                   n5_state == "Meghalaya" & str_detect(REGNAME,"(Khasi)") ~ "Khasi Hills",
                                   TRUE ~ "Other")) %>% 
  dplyr::filter(variable %in% c("Hypertension","Diagnosed"),ml_comparison != "Other") %>% 
  dplyr::select(n5_state,REGNAME,ml_comparison2,variable,estimate) %>% 
  group_by(ml_comparison2,variable) %>% 
  summarize(m = median(estimate),
            n = n())
  

district_met %>% 
  mutate(ka_comparison = case_when(n5_state == "Karnataka" & REGNAME %in% c("Chikmagalur","Udupi") ~ "Chikmagalur-Udupi",
                                   n5_state == "Karnataka" & REGNAME %in% c("Chitradurga","Shimoga") ~ "Chitradurga-Shimoga",
                                   TRUE ~ "Other")) %>% 
  dplyr::filter(variable %in% c("Hypertension","Treated","Controlled"),ka_comparison != "Other") %>% 
  dplyr::select(n5_state,REGNAME,variable,est_ci) %>% 
  pivot_wider(names_from=variable,values_from=est_ci) %>% View()


figA = district_met %>%
  dplyr::filter(variable %in% c("Hypertension","Diagnosed")) %>% 
  dplyr::select(n5_state,REGNAME,ml_comparison,estimate,variable) %>% 
  pivot_wider(names_from=variable,values_from=estimate) %>% 
  ggplot(data=,aes(x=Hypertension,y=Diagnosed,col=ml_comparison))  +
  geom_point(alpha=0.5) +
  # geom_text() +
  # facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_color_manual("Estimate (%)",values=c("Garo Hills"="red","Jaintia and Khasi Hills"="darkgreen","Other"="grey80")) +
  theme_bw() +
  xlab("Hypertension (%)") +
  ylab("Diagnosed (%)")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20)) +
  geom_hline(yintercept=80,col="orange",linetype=2)


figB = district_met %>%
  dplyr::filter(variable %in% c("Hypertension","Treated")) %>% 
  dplyr::select(n5_state,REGNAME,ka_comparison,estimate,variable) %>% 
  pivot_wider(names_from=variable,values_from=estimate) %>% 
  ggplot(data=,aes(x=Hypertension,y=Treated,col=ka_comparison))  +
  geom_point(alpha=0.5) +
  # geom_text() +
  # facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_color_manual("Estimate (%)",values=c("Chikmagalur-Udupi"="red","Chitradurga-Shimoga"="darkgreen","Other"="grey80")) +
  theme_bw() +
  xlab("Hypertension (%)") +
  ylab("Treated (%)")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))  +
  geom_hline(yintercept=80,col="orange",linetype=2)

figC = district_met %>%
  dplyr::filter(variable %in% c("Hypertension","Controlled")) %>% 
  dplyr::select(n5_state,REGNAME,ka_comparison,estimate,variable) %>% 
  pivot_wider(names_from=variable,values_from=estimate) %>% 
  ggplot(data=,aes(x=Hypertension,y=Controlled,col=ka_comparison))  +
  geom_point(alpha=0.5) +
  # geom_text() +
  # facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_color_manual("Estimate (%)",values=c("Chikmagalur-Udupi"="red","Chitradurga-Shimoga"="darkgreen","Other"="grey80")) +
  theme_bw() +
  xlab("Hypertension (%)") +
  ylab("Controlled (%)")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))  +
  geom_hline(yintercept=80,col="orange",linetype=2)

require(ggpubr)
ggarrange(figA,figB,figC,nrow=3,ncol=1,
          labels = c("A","B","C"),
          common.legend = FALSE) %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_scatterplot of district estimates.png"),width=12,height=8)

