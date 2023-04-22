source("functions/equiplot_states.R")


state_unmet_cascade <- bind_rows(read_csv(file = "analysis/hca05_state unmet need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                           read_csv(file="analysis/hca03_state level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Hypertension")
) %>% 
  dplyr::filter(n >= 25) %>%
  dplyr::filter(!is.na(variable)) %>%
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"),
                           labels=c("Hypertension","Unscreened","Undiagnosed","Untreated \namong Diagnosed",
                                    "Uncontrolled \namong Treated"))) %>% 
  dplyr::filter(variable !="Unscreened") %>% 
  mutate(se = (estimate - lci)/1.96)


national_unmet_cascade <- bind_rows(read_csv(file = "analysis/hca09_national unmet need care cascade.csv") %>% 
                                      dplyr::filter(is.na(stratification)) %>% 
                                      mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()),
                                    read_csv(file="analysis/hca02_national level care cascade.csv") %>% 
                                      dplyr::filter(is.na(stratification)) %>% 
                                      mutate(variable = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
                                      dplyr::filter(variable == "Disease") %>% 
                                      mutate(variable = "Hypertension")) %>%
  mutate(variable = factor(variable,levels=c("Hypertension","Unscreened","Undiagnosed","Untreated","Uncontrolled"),
                           labels=c("Hypertension","Unscreened","Undiagnosed","Untreated \namong Diagnosed",
                                    "Uncontrolled \namong Treated"))) %>% 
  dplyr::filter(variable !="Unscreened") %>% 
  mutate(se = (estimate - lci)/1.96) %>% 
  dplyr::select(residence,variable,estimate,se) %>% 
  pivot_wider(names_from=residence,values_from=c(estimate,se)) %>% 
  mutate(diff = estimate_Urban - estimate_Rural,
         se_diff = sqrt(se_Urban^2 + se_Rural^2)) %>% 
  mutate(lci_diff = diff - 1.96*se_diff,
         uci_diff = diff + 1.96*se_diff)

equiplot_df <- unmet_cascade %>% 
  dplyr::select(n5_state,residence,variable,zone,estimate,se) %>% 
  pivot_wider(names_from=residence,values_from=c(estimate,se)) %>% 
  mutate(diff = estimate_Urban - estimate_Rural,
         se_diff = sqrt(se_Urban^2 + se_Rural^2)) %>% 
  mutate(lci_diff = diff - 1.96*se_diff,
         uci_diff = diff + 1.96*se_diff)

library(lme4)
vpcA = state_unmet_cascade %>% 
  dplyr::filter(variable == "Hypertension") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) %>% 
  performance::icc()

figA = equiplot_df %>% 
  dplyr::filter(variable == "Hypertension") %>% 
  equiplot_states(state_diff=.,national_diff = national_unmet_cascade[national_unmet_cascade$variable=="Hypertension",]$diff
                  )


ggplot(data=equiplot_df,aes(x=diff,y=n5_state,xmin=lci_diff,xmax=uci_diff)) +
  geom_point() +
  geom_errorbarh() +
  facet_grid(zone~variable,scales="free_y",space="free_y") +
  # https://stackoverflow.com/questions/44196384/how-to-produce-different-geom-vline-in-different-facets-in-r
  geom_vline(data=dplyr::filter(national_unmet_cascade,variable=="Hypertension"),aes(xintercept=diff),col="red",linetype=2) +
  geom_vline(data=dplyr::filter(national_unmet_cascade,variable=="Undiagnosed"),aes(xintercept=diff),col="red",linetype=2) +
  geom_vline(data=dplyr::filter(national_unmet_cascade,variable=="Untreated \namong Diagnosed"),aes(xintercept=diff),col="red",linetype=2) +
  geom_vline(data=dplyr::filter(national_unmet_cascade,variable=="Uncontrolled \namong Treated"),aes(xintercept=diff),col="red",linetype=2) +
  theme_bw() 

