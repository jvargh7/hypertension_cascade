district_met <- bind_rows(read_csv(file = "age_standardized/hcz05_district met need care cascade.csv") %>% 
                            dplyr::filter(is.na(stratification)) %>% 
                            mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                          read_csv(file="age_standardized/hcz06_age standardized district cascade.csv") %>% 
                            dplyr::filter(is.na(stratification)) %>% 
                            mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                            dplyr::filter(variable == "Disease") %>% 
                            mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(variable !="Screened") %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Diagnosed","Treated","Controlled"))) %>% 
  left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),sheet="map2020_v024") %>% 
              dplyr::select(v024,zone) %>% 
              distinct(v024,.keep_all=TRUE),
            by=c("v024")) %>% 
  dplyr::filter(n5_state %in% c("Uttar Pradesh", "Madhya Pradesh",
                                "Bihar","West Bengal",
                                "Punjab", "Rajasthan",
                                "Maharashtra", "Gujarat",
                                "Tamil Nadu","Andhra Pradesh",
                                "Nagaland","Assam"))  %>% 
  arrange(zone,n5_state) %>% 
  mutate(n5_state = fct_inorder(factor(n5_state)))


state_met <- bind_rows(read_csv(file = "age_standardized/hcz04_state met need care cascade.csv") %>% 
                         dplyr::filter(is.na(stratification)) %>% 
                         mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                       read_csv(file="age_standardized/hcz02_age standardized state cascade.csv") %>% 
                         dplyr::filter(is.na(stratification)) %>% 
                         mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                         dplyr::filter(variable == "Disease") %>% 
                         mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(variable !="Screened") %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Diagnosed","Treated","Controlled")))  %>% 
  dplyr::filter(n5_state %in% c("Uttar Pradesh", "Madhya Pradesh",
                                "Bihar","West Bengal",
                                "Punjab", "Rajasthan",
                                "Maharashtra", "Gujarat",
                                "Tamil Nadu","Andhra Pradesh",
                                "Nagaland","Assam")) %>% 
  arrange(zone,n5_state) %>% 
  mutate(n5_state = fct_inorder(factor(n5_state)))


figA = ggplot() +
  geom_violin(data=district_met %>% 
                dplyr::filter(variable == "Diagnosed"),aes(x=n5_state,y=estimate,col=zone,fill=zone)) +
  geom_point(data = state_met %>% 
               dplyr::filter(variable == "Diagnosed"),aes(x=n5_state,y=estimate,shape=residence),col="black") +
  xlab("") +
  ylab("Diagnosed (%)") +
  theme_bw() +
  scale_fill_discrete(name = "Zone") +
  scale_color_discrete(name = "Zone") +
  scale_shape_discrete(name = "") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))

figB = ggplot() +
  geom_violin(data=district_met %>% 
                dplyr::filter(variable == "Treated"),aes(x=n5_state,y=estimate,col=zone,fill=zone)) +
  geom_point(data = state_met %>% 
               dplyr::filter(variable == "Treated"),aes(x=n5_state,y=estimate,shape=residence),col="black") +
  xlab("") +
  ylab("Treated (%)") +
  theme_bw() +
  scale_fill_discrete(name = "Zone") +
  scale_color_discrete(name = "Zone") +
  scale_shape_discrete(name = "") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))

figC = ggplot() +
  geom_violin(data=district_met %>% 
                dplyr::filter(variable == "Controlled"),aes(x=n5_state,y=estimate,col=zone,fill=zone)) +
  geom_point(data = state_met %>% 
               dplyr::filter(variable == "Controlled"),aes(x=n5_state,y=estimate,shape=residence),col="black") +
  xlab("") +
  ylab("Controlled (%)") +
  theme_bw() +
  scale_fill_discrete(name = "Zone") +
  scale_color_discrete(name = "Zone") +
  scale_shape_discrete(name = "") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))

require(ggpubr)
ggarrange(figA,figB,figC,nrow=3,ncol=1,
          labels = c("A","B","C"),
          common.legend = TRUE) %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_violinplot for example states age standardized.png"),width=12,height=6)

