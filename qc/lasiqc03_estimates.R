
group_vars = c("","sex")

source("C:/code/external/functions/survey/svysummary.R")
proportion_vars <- c("htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")


source("qc/lasiqc01_total svydesign.R")


lasi_national <- map_dfr(group_vars,
                         function(g_v){
                           id_vars = c(g_v);
                           n5_sy <- svysummary(lasi_design,
                                               # c_vars = continuous_vars,
                                               p_vars = proportion_vars,
                                               # g_vars = grouped_vars,
                                               id_vars = id_vars
                           ) %>% 
                             mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                             mutate(est_ci = paste0(estimate," (",
                                                    lci,", ",uci,")"));
                           
                           # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                           n5_ct <- lasi_df %>% 
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

lasi_regional <- map_dfr(group_vars,
                         function(g_v){
                           id_vars = c("residence",g_v);
                           n5_sy <- svysummary(lasi_design,
                                               # c_vars = continuous_vars,
                                               p_vars = proportion_vars,
                                               # g_vars = grouped_vars,
                                               id_vars = id_vars
                           ) %>% 
                             mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                             mutate(est_ci = paste0(estimate," (",
                                                    lci,", ",uci,")"));
                           
                           # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                           n5_ct <- lasi_df %>% 
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

lasi_htn_national <- map_dfr(group_vars,
                            function(g_v){
                              id_vars = c(g_v);
                              n5_sy <- svysummary(lasi_htn_design,
                                                  # c_vars = continuous_vars,
                                                  p_vars = proportion_vars[3:5],
                                                  # g_vars = grouped_vars,
                                                  id_vars = id_vars
                              ) %>% 
                                mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                mutate(est_ci = paste0(estimate," (",
                                                       lci,", ",uci,")"));
                              
                              # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                              n5_ct <- lasi_df %>% 
                                dplyr::filter(htn_disease == 1) %>% 
                                group_by_at(vars(one_of(id_vars))) %>% 
                                summarize_at(vars(one_of(c(
                                  # continuous_vars,
                                  proportion_vars[3:5]
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

lasi_htn_regional <- map_dfr(group_vars,
                            function(g_v){
                              id_vars = c("residence",g_v);
                              n5_sy <- svysummary(lasi_htn_design,
                                                  # c_vars = continuous_vars,
                                                  p_vars = proportion_vars[3:5],
                                                  # g_vars = grouped_vars,
                                                  id_vars = id_vars
                              ) %>% 
                                mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                mutate(est_ci = paste0(estimate," (",
                                                       lci,", ",uci,")"));
                              
                              # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                              n5_ct <- lasi_df %>% 
                                dplyr::filter(htn_disease == 1) %>% 
                                group_by_at(vars(one_of(id_vars))) %>% 
                                summarize_at(vars(one_of(c(
                                  # continuous_vars,
                                  proportion_vars[3:5]
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


write_csv(lasi_national,"qc/lasiqc03_national equivalent estimates.csv")
write_csv(lasi_regional,"qc/lasiqc03_regional equivalent estimates.csv")
write_csv(lasi_htn_national,"qc/lasiqc03_national equivalent nested estimates.csv")
write_csv(lasi_htn_regional,"qc/lasiqc03_regional equivalent nested estimates.csv")
