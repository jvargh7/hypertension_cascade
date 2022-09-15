
group_vars = c("","sex")

source("C:/code/external/functions/survey/svysummary.R")
proportion_vars <- c("htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")


nfhs4pop_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                          mutate(sex = "Female"),
                        readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                          mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(htn_free),age %in% c(15:49)) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE)

nfhs4pop_design <- nfhs4pop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

nfhs4pop_htn_design <- nfhs4pop_df %>% 
  dplyr::filter(htn_disease == 1) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


nfhs4_national <- map_dfr(group_vars,
                         function(g_v){
                           id_vars = c(g_v);
                           
                           n5_sy <- svysummary(nfhs4pop_design,
                                               # c_vars = continuous_vars,
                                               p_vars = proportion_vars,
                                               # g_vars = grouped_vars,
                                               id_vars = id_vars
                           ) %>% 
                             mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                             mutate(est_ci = paste0(estimate," (",
                                                    lci,", ",uci,")"));
                           
                           # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                           n5_ct <- nfhs4pop_df %>% 
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

nfhs4_regional <- map_dfr(group_vars,
                         function(g_v){
                           id_vars = c("residence",g_v);
                           
                           n5_sy <- svysummary(nfhs4pop_design,
                                               # c_vars = continuous_vars,
                                               p_vars = proportion_vars,
                                               # g_vars = grouped_vars,
                                               id_vars = id_vars
                           ) %>% 
                             mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                             mutate(est_ci = paste0(estimate," (",
                                                    lci,", ",uci,")"));
                           
                           # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                           n5_ct <- nfhs4pop_df %>% 
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

nfhs4_htn_national <- map_dfr(group_vars,
                             function(g_v){
                               id_vars = c(g_v);
                               
                               n5_sy <- svysummary(nfhs4pop_htn_design,
                                                   # c_vars = continuous_vars,
                                                   p_vars = proportion_vars[3:5],
                                                   # g_vars = grouped_vars,
                                                   id_vars = id_vars
                               ) %>% 
                                 mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                 mutate(est_ci = paste0(estimate," (",
                                                        lci,", ",uci,")"));
                               
                               # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                               n5_ct <- nfhs4pop_df %>% 
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

nfhs4_htn_regional <- map_dfr(group_vars,
                             function(g_v){
                               id_vars = c("residence",g_v);
                               
                               n5_sy <- svysummary(nfhs4pop_htn_design,
                                                   # c_vars = continuous_vars,
                                                   p_vars = proportion_vars[3:5],
                                                   # g_vars = grouped_vars,
                                                   id_vars = id_vars
                               ) %>% 
                                 mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                 mutate(est_ci = paste0(estimate," (",
                                                        lci,", ",uci,")"));
                               
                               # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                               n5_ct <- nfhs4pop_df %>% 
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


write_csv(nfhs4_national,"qc/nfhs4 national equivalent estimates.csv")
write_csv(nfhs4_regional,"qc/nfhs4 regional equivalent estimates.csv")
write_csv(nfhs4_htn_national,"qc/nfhs4 national equivalent nested estimates.csv")
write_csv(nfhs4_htn_regional,"qc/nfhs4 regional equivalent nested estimates.csv")
