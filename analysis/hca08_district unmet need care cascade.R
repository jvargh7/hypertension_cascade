
group_vars = c("","sex")

source("C:/code/external/functions/survey/svysummary.R")


source("preprocessing/hcpre04_nfhs5 hypertension svydesign.R")
source("preprocessing/hcpre05_nfhs5 diagnosed svydesign.R")

proportion_vars <- c("htn_screened","htn_diagnosed","htn_unscreened","htn_undiagnosed",
                     "htn_treated","htn_controlled","htn_untreated","htn_uncontrolled")

require(furrr)
options(future.globals.maxSize= (6*1024*1024)^2) #4GB
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
plan(multisession, workers = 3)
unmet_svysummary_htn <- future_map_dfr(group_vars,
                                      function(g_v){
                                        id_vars = c("district_df",g_v);
                                        print(g_v);
                                        n5_sy_htn <- svysummary(nfhs5htn_svydesign,
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
                                            id_vars = c("district_df",g_v);
                                            print(g_v);
                                            n5_sy_htndiag <- svysummary(nfhs5htndiag_svydesign,
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


bind_rows(unmet_svysummary_htn,
          unmet_svysummary_htndiag) %>% 
  dplyr::filter(str_detect(variable,"htn_un")) %>% 
  rename(D_CODE = district_df) %>% 
  # There are missing values in D_CODE from subsetting on map
  dplyr::filter(!is.na(D_CODE)) %>% 
  left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),"map2018_sdist") %>% 
              dplyr::select(D_CODE,n5_state,v024,D_NAME) %>% 
              mutate(D_CODE = sprintf("%03d",as.numeric(D_CODE))),
            by=c("D_CODE")) %>% 
  write_csv(.,file = "analysis/hca08_district unmet need care cascade.csv")


bind_rows(unmet_svysummary_htn,
          unmet_svysummary_htndiag) %>% 
  dplyr::filter(!str_detect(variable,"htn_un")) %>% 
  rename(D_CODE = district_df) %>% 
  # There are missing values in D_CODE from subsetting on map
  dplyr::filter(!is.na(D_CODE)) %>% 
  left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),"map2018_sdist") %>% 
              dplyr::select(D_CODE,n5_state,v024,D_NAME) %>% 
              mutate(D_CODE = sprintf("%03d",as.numeric(D_CODE))),
            by=c("D_CODE")) %>% 
  write_csv(.,file = "analysis/hca08_district met need care cascade.csv")

# df <- read_csv("analysis/nca08_district unmet need care cascade.csv") %>%
#   dplyr::select(-n5_state,-v024,-D_NAME) %>%
#   left_join(readxl::read_excel("data/NFHS Cascade Variable List.xlsx","map2018_sdist") %>%
#               dplyr::select(D_CODE,n5_state,v024,D_NAME) %>%
#               mutate(D_CODE = sprintf("%03d",as.numeric(D_CODE))),
#             by=c("D_CODE"))
# write_csv(df,file = "analysis/hca08_district unmet need care cascade.csv")
# 
