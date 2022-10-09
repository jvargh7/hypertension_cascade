group_vars <- c("","sex","age_category","education",
                "caste","religion","wealthq_ur")


source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/hcpre03_nfhs5 total svydesign.R")

proportion_vars <- c("htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")


national_summary <- svysummary(nfhs5_svydesign,
                               # c_vars = continuous_vars,
                               p_vars = proportion_vars,
                               # g_vars = grouped_vars,
                               # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

write_csv(national_summary,"analysis/hca02_national total.csv")



# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize


  nfhs5_svysummary <- map_dfr(group_vars,
                                     function(g_v){
                                       id_vars = c("residence",g_v);
                                       p();
                                       n5_sy <- svysummary(nfhs5_svydesign,
                                                           # c_vars = continuous_vars,
                                                           p_vars = proportion_vars,
                                                           # g_vars = grouped_vars,
                                                           id_vars = id_vars
                                       ) %>% 
                                         mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                         mutate(est_ci = paste0(estimate," (",
                                                                lci,", ",uci,")"));
                                       
                                       # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                       n5_ct <- nfhs5_df %>% 
                                         group_by_at(vars(one_of(id_vars))) %>% 
                                         summarize_at(vars(one_of(c(
                                           # continuous_vars,
                                           proportion_vars
                                           # grouped_vars
                                         ))),
                                         list(n = ~sum(!is.na(.)))) %>% 
                                         pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                         mutate(variable = str_replace(variable,"_n$",""));
                                       
                                       n5_out <- left_join(n5_sy,
                                                           n5_ct,
                                                           by=c(id_vars[id_vars!=""],"variable")) %>% 
                                         
                                         # Restrict to those cells with more than 100 observations
                                         dplyr::filter(n > 100) %>% 
                                         mutate(stratification = g_v) %>% 
                                         rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                         mutate_at(vars(one_of("strata")),~as.character(.));
                                       
                                       return(n5_out)
                                       
                                     })
  



write_csv(nfhs5_svysummary,file = "analysis/hca02_national level care cascade.csv")


