


# Urban - Rural ----------
unmet_cascade_ur <- bind_rows(read_csv(file = "analysis/hca05_state unmet need care cascade.csv") %>% 
                                dplyr::filter(is.na(stratification)) %>% 
                                mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                              read_csv(file="analysis/hca03_state level care cascade.csv") %>% 
                                dplyr::filter(is.na(stratification)) %>% 
                                mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                                dplyr::filter(variable == "Disease") %>% 
                                mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(n >= 25) %>%
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"),
                           labels=c("Hypertension","Unscreened","Undiagnosed","Untreated \nAmong Diagnosed",
                                    "Uncontrolled \nAmong Treated"))) %>% 
  dplyr::filter(!is.na(variable)) %>% 
  dplyr::filter(variable !="Unscreened") %>% 
  mutate(estimate_label = sprintf("%.01f",round(estimate,1)),
         estimate_fig = case_when(variable == "Hypertension" ~ 120,
                                  TRUE ~ estimate))

figure_urban <- unmet_cascade_ur %>% 
  dplyr::filter(residence == "Urban") %>% 
  ggplot(data=.,aes(x=variable,y=n5_state,fill=estimate_fig,label=estimate_label)) +
  geom_tile() +
  geom_text() +
  facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_fill_gradient2("Estimate (%)",low="white",mid="lightblue",high="darkblue",midpoint = 50,limits=c(0,100),na.value = "white") +
  theme_bw() +
  xlab("") +
  ylab("")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 10),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")

  
figure_rural <- unmet_cascade_ur %>% 
  dplyr::filter(residence == "Rural") %>% 
  ggplot(data=.,aes(x=variable,y=n5_state,fill=estimate_fig,label=estimate_label)) +
  geom_tile() +
  geom_text() +
  facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_fill_gradient2("Estimate (%)",low="white",mid="lightblue",high="darkblue",midpoint = 50,limits=c(0,100),na.value = "white") +
  theme_bw() +
  xlab("") +
  ylab("")  +
  theme(
    legend.text = element_text(size=12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")

require(ggpubr)
  ggarrange(figure_urban,figure_rural,nrow=1,ncol=2,
            labels = c("A","B"),
            common.legend = TRUE,legend="bottom",
            widths = c(2.2,1.5)) %>% 
    ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_heatmap cascade urban rural.png"),width=15,height=8)

  # Total ----------
  unmet_cascade_total <- bind_rows(read_csv(file = "analysis/hca12_total state unmet need care cascade.csv") %>% 
                                     dplyr::filter(is.na(stratification)) %>% 
                                     mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                                   read_csv(file="analysis/hca11_total state level care cascade.csv") %>% 
                                     dplyr::filter(is.na(stratification)) %>% 
                                     mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                                     dplyr::filter(variable == "Disease") %>% 
                                     mutate(variable = "Hypertension")
  ) %>% 
    dplyr::filter(n >= 25) %>%
    mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"),
                             labels=c("Hypertension","Unscreened","Undiagnosed","Untreated \nAmong Diagnosed",
                                      "Uncontrolled \nAmong Treated"))) %>% 
    dplyr::filter(!is.na(variable)) %>% 
    dplyr::filter(variable !="Unscreened") %>% 
    mutate(estimate_label = sprintf("%.01f",round(estimate,1)),
           estimate_fig = case_when(variable == "Hypertension" ~ 120,
                                    TRUE ~ estimate))
  
  figure_total <- unmet_cascade_total %>% 
    ggplot(data=.,aes(x=variable,y=n5_state,fill=estimate_fig,label=estimate_label)) +
    geom_tile() +
    geom_text() +
    facet_grid(zone~.,scales="free_y",space="free_y") +
    scale_fill_gradient2("Estimate (%)",low="white",mid="lightblue",high="darkblue",midpoint = 50,limits=c(0,100),na.value = "white") +
    theme_bw() +
    xlab("") +
    ylab("")  +
    theme(
      legend.text = element_text(size=12),
      axis.text = element_text(size = 10),
      strip.background.y = element_blank(),
      strip.text.x = element_text(size=12),
      strip.text.y = element_text(size=12),
      legend.position = "bottom")
  
  
  require(ggpubr)
  figure_total %>% 
    ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_heatmap cascade total.png"),width=8,height=8)
  
  