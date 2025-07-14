
group_vars = c("","sex","age_category","education",
               "caste","religion","wealthq_ur")

source("C:/code/external/functions/survey/svysummary.R")


source("preprocessing/hcpre33_nfhs5 total svydesign with 130 cutoff.R")

proportion_vars <- c("htn_controlled","htn_uncontrolled")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

# Dropped education in age standardization 
nfhs5htntreatz_svydesign = svystandardize(nfhs5htntreat_svydesign,by=~age_category,over = ~caste + religion + wealthq_ur,
                                         population = pop_age)
# National ---------

n5_sy <- svysummary(nfhs5htntreatz_svydesign,
                    # c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    # g_vars = grouped_vars,
                    # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

n5_sy %>% 
  write_csv(.,file = "cutoff 130 and 80/hcc130z08_total age standardized national conditional cascade.csv")

# NATIONAL ---------

unmet_svysummary_htntreat_national <- map_dfr(group_vars[-1],
                                     function(g_v){
                                       id_vars = c(g_v);
                                       n5_sy <- svysummary(nfhs5htntreatz_svydesign,
                                                           # c_vars = continuous_vars,
                                                           p_vars = proportion_vars,
                                                           # g_vars = grouped_vars,
                                                           id_vars = id_vars
                                       ) %>% 
                                         mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                         mutate(est_ci = paste0(estimate," (",
                                                                lci,", ",uci,")"));
                                       
                                       # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                       n5_ct <- nfhs5htntreat_df %>% 
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
                                         # dplyr::filter(n > 100) %>% 
                                         mutate(stratification = g_v) %>% 
                                         rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                         mutate_at(vars(one_of("strata")),~as.character(.));
                                       
                                       return(n5_out)
                                       
                                     })


# REGIONAL --------
unmet_svysummary_htntreat_regional <- map_dfr(group_vars,
                                    function(g_v){
                                      id_vars = c("residence",g_v);
                                      print(g_v);
                                      n5_sy_htntreat <- svysummary(nfhs5htntreatz_svydesign,
                                                                  # c_vars = continuous_vars,
                                                                  p_vars = proportion_vars,
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
                                          proportion_vars
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


bind_rows(unmet_svysummary_htntreat_national,
          unmet_svysummary_htntreat_regional) %>% 
  dplyr::filter(str_detect(variable,"htn_un")) %>% 
  write_csv(.,file = "cutoff 130 and 80/hcc130z08_national conditional cascade.csv")

bind_rows(unmet_svysummary_htntreat_national,
          unmet_svysummary_htntreat_regional) %>% 
  dplyr::filter(!str_detect(variable,"htn_un")) %>% 
  write_csv(.,file = "cutoff 130 and 80/hcc130z08_national conditional cascade.csv")

