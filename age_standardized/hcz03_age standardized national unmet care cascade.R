
group_vars = c("","sex","age_category","education",
               "caste","religion","wealthq_ur")

source("C:/code/external/functions/survey/svysummary.R")


source("preprocessing/hcpre04_nfhs5 hypertension svydesign.R")
source("preprocessing/hcpre05_nfhs5 diagnosed svydesign.R")

proportion_vars <- c("htn_screened","htn_diagnosed","htn_unscreened","htn_undiagnosed",
                     "htn_treated","htn_controlled","htn_untreated","htn_uncontrolled")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

nfhs5htnz_svydesign = svystandardize(nfhs5htn_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                     population = pop_age)
nfhs5htndiagz_svydesign = svystandardize(nfhs5htndiag_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                     population = pop_age)

source("preprocessing/hcp_parallelize.R")
# NATIONAL ----------
unmet_svysummary_htn_national <- future_map_dfr(group_vars[-1],
                                               function(g_v){
                                                 print(g_v);
                                                 id_vars = c(g_v);
                                                 n5_sy_htn <- svysummary(nfhs5htnz_svydesign,
                                                                        # c_vars = continuous_vars,
                                                                        p_vars = proportion_vars[1:4],
                                                                        # g_vars = grouped_vars,
                                                                        id_vars = id_vars
                                                 ) %>% 
                                                   mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                                   mutate(est_ci = paste0(estimate," (",
                                                                          lci,", ",uci,")"));
                                                 
                                                 # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                                 n5_ct_htn <- nfhs5htn_df %>% 
                                                   group_by_at(vars(one_of(id_vars))) %>% 
                                                   summarize_at(vars(one_of(c(
                                                     # continuous_vars,
                                                     proportion_vars[1:4]
                                                     # grouped_vars
                                                   ))),
                                                   list(n = ~sum(!is.na(.)))) %>% 
                                                   pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                                   mutate(variable = str_replace(variable,"_n$",""));
                                                 
                                                 n5_out_htn <- left_join(n5_sy_htn,
                                                                        n5_ct_htn,
                                                                        by=c(id_vars[id_vars!=""],"variable")) %>% 
                                                   
                                                   # Restrict to those cells with more than 100 observations
                                                   # dplyr::filter(n > 100) %>%
                                                   mutate(stratification = g_v) %>% 
                                                   rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                                   mutate_at(vars(one_of("strata")),~as.character(.));
                                                 gc();
                                                 return(n5_out_htn)
                                                 
                                               })


unmet_svysummary_htndiag_national <- future_map_dfr(group_vars[-1],
                                                   function(g_v){
                                                     id_vars = c(g_v);
                                                     print(g_v);
                                                     n5_sy_htndiag <- svysummary(nfhs5htndiagz_svydesign,
                                                                                # c_vars = continuous_vars,
                                                                                p_vars = proportion_vars[5:8],
                                                                                # g_vars = grouped_vars,
                                                                                id_vars = id_vars
                                                     ) %>% 
                                                       mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                                       mutate(est_ci = paste0(estimate," (",
                                                                              lci,", ",uci,")"));
                                                     
                                                     # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                                     n5_ct_htndiag <- nfhs5htndiag_df %>% 
                                                       group_by_at(vars(one_of(id_vars))) %>% 
                                                       summarize_at(vars(one_of(c(
                                                         # continuous_vars,
                                                         proportion_vars[5:8]
                                                         # grouped_vars
                                                       ))),
                                                       list(n = ~sum(!is.na(.)))) %>% 
                                                       pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                                       mutate(variable = str_replace(variable,"_n$",""));
                                                     
                                                     n5_out_htndiag <- left_join(n5_sy_htndiag,
                                                                                n5_ct_htndiag,
                                                                                by=c(id_vars[id_vars!=""],"variable")) %>% 
                                                       
                                                       # Restrict to those cells with more than 100 observations
                                                       # dplyr::filter(n > 100) %>%
                                                       mutate(stratification = g_v) %>% 
                                                       rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                                       mutate_at(vars(one_of("strata")),~as.character(.));
                                                     gc();
                                                     return(n5_out_htndiag)
                                                     
                                                   })

# REGIONAL ----------
unmet_svysummary_htn <- future_map_dfr(group_vars,
                                      function(g_v){
                                        print(g_v);
                                        id_vars = c("residence",g_v);
                                        n5_sy_htn <- svysummary(nfhs5htnz_svydesign,
                                                               # c_vars = continuous_vars,
                                                               p_vars = proportion_vars[1:4],
                                                               # g_vars = grouped_vars,
                                                               id_vars = id_vars
                                        ) %>% 
                                          mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                          mutate(est_ci = paste0(estimate," (",
                                                                 lci,", ",uci,")"));
                                        
                                        # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                        n5_ct_htn <- nfhs5htn_df %>% 
                                          group_by_at(vars(one_of(id_vars))) %>% 
                                          summarize_at(vars(one_of(c(
                                            # continuous_vars,
                                            proportion_vars[1:4]
                                            # grouped_vars
                                          ))),
                                          list(n = ~sum(!is.na(.)))) %>% 
                                          pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                          mutate(variable = str_replace(variable,"_n$",""));
                                        
                                        n5_out_htn <- left_join(n5_sy_htn,
                                                               n5_ct_htn,
                                                               by=c(id_vars[id_vars!=""],"variable")) %>% 
                                          
                                          # Restrict to those cells with more than 100 observations
                                          # dplyr::filter(n > 100) %>%
                                          mutate(stratification = g_v) %>% 
                                          rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                          mutate_at(vars(one_of("strata")),~as.character(.));
                                        gc();
                                        return(n5_out_htn)
                                        
                                      })


unmet_svysummary_htndiag <- future_map_dfr(group_vars,
                                          function(g_v){
                                            id_vars = c("residence",g_v);
                                            print(g_v);
                                            n5_sy_htndiag <- svysummary(nfhs5htndiagz_svydesign,
                                                                       # c_vars = continuous_vars,
                                                                       p_vars = proportion_vars[5:8],
                                                                       # g_vars = grouped_vars,
                                                                       id_vars = id_vars
                                            ) %>% 
                                              mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                              mutate(est_ci = paste0(estimate," (",
                                                                     lci,", ",uci,")"));
                                            
                                            # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                            n5_ct_htndiag <- nfhs5htndiag_df %>% 
                                              group_by_at(vars(one_of(id_vars))) %>% 
                                              summarize_at(vars(one_of(c(
                                                # continuous_vars,
                                                proportion_vars[5:8]
                                                # grouped_vars
                                              ))),
                                              list(n = ~sum(!is.na(.)))) %>% 
                                              pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                              mutate(variable = str_replace(variable,"_n$",""));
                                            
                                            n5_out_htndiag <- left_join(n5_sy_htndiag,
                                                                       n5_ct_htndiag,
                                                                       by=c(id_vars[id_vars!=""],"variable")) %>% 
                                              
                                              # Restrict to those cells with more than 100 observations
                                              # dplyr::filter(n > 100) %>%
                                              mutate(stratification = g_v) %>% 
                                              rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                              mutate_at(vars(one_of("strata")),~as.character(.));
                                            gc();
                                            return(n5_out_htndiag)
                                            
                                          })



bind_rows(
  unmet_svysummary_htn,
  unmet_svysummary_htndiag,
  unmet_svysummary_htn_national,
  unmet_svysummary_htndiag_national) %>% 
  dplyr::filter(str_detect(variable,"htn_un")) %>% 
  write_csv(.,file = "age_standardized/hcz03_national unmet need care cascade.csv")

bind_rows(unmet_svysummary_htn,
          unmet_svysummary_htndiag,
          unmet_svysummary_htn_national,
          unmet_svysummary_htndiag_national) %>% 
  dplyr::filter(!str_detect(variable,"htn_un")) %>% 
  write_csv(.,file = "age_standardized/hcz03_national met need care cascade.csv")
