
group_vars = c("","sex")

source("C:/code/external/functions/survey/svysummary.R")

# source("preprocessing/hcpre03_nfhs5 total svydesign.R")
source("preprocessing/hcpre14_nfhs5 hypertension svydesign with last2bp.R")
source("preprocessing/hcpre15_nfhs5 diagnosed svydesign with last2bp.R")
source("preprocessing/hcpre16_nfhs5 treated svydesign with last2bp.R")

proportion_vars <- c("htn_screened","htn_diagnosed","htn_unscreened","htn_undiagnosed",
                     "htn_treated","htn_untreated","htn_controlled","htn_uncontrolled")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

nfhs5htnz_svydesign = svystandardize(nfhs5htn_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                     population = pop_age)
nfhs5htndiagz_svydesign = svystandardize(nfhs5htndiag_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                         population = pop_age)
nfhs5htntreatz_svydesign = svystandardize(nfhs5htntreat_svydesign,by=~age_category,over = ~caste + religion + wealthq_ur,
                                         population = pop_age)

unmet_svysummary_htn <- map_dfr(group_vars,
                                function(g_v){
                                  id_vars = c(g_v);
                                  print(g_v);
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
                                    dplyr::filter(n > 100) %>%
                                    mutate(stratification = g_v) %>% 
                                    rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                    mutate_at(vars(one_of("strata")),~as.character(.));
                                  gc();
                                  return(n5_out_htn)
                                  
                                })


unmet_svysummary_htndiag <- map_dfr(group_vars,
                                    function(g_v){
                                      id_vars = c(g_v);
                                      print(g_v);
                                      n5_sy_htndiag <- svysummary(nfhs5htndiagz_svydesign,
                                                                  # c_vars = continuous_vars,
                                                                  p_vars = proportion_vars[5:6],
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
                                          proportion_vars[5:6]
                                          # grouped_vars
                                        ))),
                                        list(n = ~sum(!is.na(.)))) %>% 
                                        pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                        mutate(variable = str_replace(variable,"_n$",""));
                                      
                                      n5_out_htndiag <- left_join(n5_sy_htndiag,
                                                                  n5_ct_htndiag,
                                                                  by=c(id_vars[id_vars!=""],"variable")) %>% 
                                        
                                        # Restrict to those cells with more than 100 observations
                                        dplyr::filter(n > 100) %>%
                                        mutate(stratification = g_v) %>% 
                                        rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                        mutate_at(vars(one_of("strata")),~as.character(.));
                                      gc();
                                      return(n5_out_htndiag)
                                      
                                    })


unmet_svysummary_htntreat <- map_dfr(group_vars,
                                    function(g_v){
                                      id_vars = c(g_v);
                                      print(g_v);
                                      n5_sy_htntreat <- svysummary(nfhs5htntreatz_svydesign,
                                                                  # c_vars = continuous_vars,
                                                                  p_vars = proportion_vars[7:8],
                                                                  # g_vars = grouped_vars,
                                                                  id_vars = id_vars
                                      ) %>% 
                                        mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                        mutate(est_ci = paste0(estimate," (",
                                                               lci,", ",uci,")"));
                                      
                                      # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                      n5_ct_htntreat <- nfhs5htntreat_df %>% 
                                        group_by_at(vars(one_of(id_vars))) %>% 
                                        summarize_at(vars(one_of(c(
                                          # continuous_vars,
                                          proportion_vars[7:8]
                                          # grouped_vars
                                        ))),
                                        list(n = ~sum(!is.na(.)))) %>% 
                                        pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                        mutate(variable = str_replace(variable,"_n$",""));
                                      
                                      n5_out_htntreat <- left_join(n5_sy_htntreat,
                                                                  n5_ct_htntreat,
                                                                  by=c(id_vars[id_vars!=""],"variable")) %>% 
                                        
                                        # Restrict to those cells with more than 100 observations
                                        dplyr::filter(n > 100) %>%
                                        mutate(stratification = g_v) %>% 
                                        rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                        mutate_at(vars(one_of("strata")),~as.character(.));
                                      gc();
                                      return(n5_out_htntreat)
                                      
                                    })


bind_rows(unmet_svysummary_htn,
          unmet_svysummary_htndiag,
          unmet_svysummary_htntreat
          ) %>% 
  write_csv(.,file = "paper/abstract_national cascade with last2bp.csv")

