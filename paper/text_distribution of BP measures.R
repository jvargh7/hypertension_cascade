

source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/hcpre03_nfhs5 total svydesign.R")

continuous_vars <- c("sbp1","sbp2","sbp3","dbp1","dbp2","dbp3")

pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

nfhs5_svystdz <- svystandardize(nfhs5_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                population = pop_age)
rm(nfhs5_svydesign);gc();


id_vars = list(c(""),
               c("sex"),
               c("residence"))

analytic_sample_summary <- map_dfr(id_vars,
                                   function(i_v){
                                     
                                     if(i_v == ""){
                                       n5_sy <- svysummary(nfhs5_svystdz,
                                                           c_vars = continuous_vars,
                                                           # p_vars = proportion_vars,
                                                           # g_vars = grouped_vars,
                                                           # id_vars = i_v
                                       ) %>% 
                                         mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                         mutate(est_ci = paste0(estimate," (",
                                                                lci,", ",uci,")"));
                                     }
                                     if(i_v != ""){
                                       n5_sy <- svysummary(nfhs5_svystdz,
                                                           c_vars = continuous_vars,
                                                           # p_vars = proportion_vars,
                                                           # g_vars = grouped_vars,
                                                           id_vars = i_v
                                       ) %>% 
                                         mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                         mutate(est_ci = paste0(estimate," (",
                                                                lci,", ",uci,")"));
                                     }
                                     
                                     # # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                     # n5_ct <- nfhs5_df %>% 
                                     #   group_by_at(vars(one_of(i_v))) %>% 
                                     #   summarize_at(vars(one_of(c(
                                     #     continuous_vars,
                                     #     # proportion_vars,
                                     #     # grouped_vars
                                     #   ))),
                                     #   list(n = ~sum(!is.na(.)))) %>% 
                                     #   pivot_longer(names_to="variable",values_to="n",cols=-one_of(i_v)) %>% 
                                     #   mutate(variable = str_replace(variable,"_n$",""));
                                     # 
                                     n5_out <- n5_sy %>%

                                       # Restrict to those cells with more than 100 observations
                                       mutate(stratification = i_v) %>%
                                       rename_at(vars(one_of(i_v)),~c("strata")) %>%
                                       mutate_at(vars(one_of("strata")),~as.character(.))
                                     
                                     return(n5_out)
                                     
                                   })



analytic_sample_summary %>% 
  mutate(strata = case_when(is.na(strata) ~ "Total",
                               TRUE ~ strata)) %>% 
  dplyr::select(variable,stratification,strata,est_ci) %>% 
  pivot_wider(names_from=variable,values_from=est_ci) %>% 
  
  write_csv(.,file = "paper/text_distribution of bp measuers.csv")


library(lme4)
m_sbp = (nfhs5_df %>% 
           mutate(lineid = paste0(caseid,sprintf("%02d",linenumber))) %>% 
           dplyr::select(lineid,sbp1,sbp2,sbp3) %>% 
           pivot_longer(cols=contains("sbp"),names_to="measure",values_to="sbp") %>% 
           lmer(sbp ~ 1 + (1|lineid),data=.)) 

m_dbp = (nfhs5_df %>% 
           mutate(lineid = paste0(caseid,sprintf("%02d",linenumber))) %>% 
           dplyr::select(lineid,dbp1,dbp2,dbp3) %>% 
           pivot_longer(cols=contains("dbp"),names_to="measure",values_to="dbp") %>% 
           dplyr::filter(!is.na(dbp)) %>% 
           mutate(dbp = scale(dbp)) %>% 
           lmer(dbp ~ 1 + (1|lineid),data=.,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))) 

# performance::icc(m_sbp)
# performance::icc(d_sbp)
summary(m_sbp)
summary(m_dbp)
