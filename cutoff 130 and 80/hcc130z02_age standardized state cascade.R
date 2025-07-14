group_vars <- c("","sex","age_category","education",
                "caste","religion","swealthq_ur")


source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/hcpre33_nfhs5 total svydesign with 130 cutoff.R")

proportion_vars <- c("htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

# id_vars = c("residence",group_vars[4]);

nfhs5_svystdz <- svystandardize(nfhs5_svydesign,by=~age_category,over = ~state + residence,
                                population = pop_age)
rm(nfhs5_svydesign);gc();


source("preprocessing/hcp_parallelize.R")


state_svysummary <- future_map_dfr(group_vars,
                            function(g_v){
                              print(g_v)
                              id_vars = c("state","residence",g_v);
                              n5_sy <- svysummary(nfhs5_svystdz,
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
                                # dplyr::filter(n > 100) %>% 
                                mutate(stratification = g_v) %>% 
                                rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                mutate_at(vars(one_of("strata")),~as.character(.));
                              gc();
                              return(n5_out)
                              
                            })


state_svysummary %>% 
  left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),sheet="map2020_v024") %>% 
              dplyr::select(v024,n5_state,zone) %>% 
              distinct(v024,n5_state,.keep_all=TRUE),
            by=c("state"="v024")) %>% 
write_csv(.,path = "cutoff 130 and 80/hcc130z02_age standardized state cascade.csv")


# df <- read_csv(file="cutoff 130 and 80/hcc130z02_age standardized state cascade.csv") %>%
#   left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),sheet="map2020_v024") %>%
#               dplyr::select(v024,n5_state,zone) %>%
#               distinct(v024,n5_state,.keep_all=TRUE),
#             by=c("state"="v024"))
# write_csv(df,file = "cutoff 130 and 80/hcc130z02_age standardized state cascade.csv")
