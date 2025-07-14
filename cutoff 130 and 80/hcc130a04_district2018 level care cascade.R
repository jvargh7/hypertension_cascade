group_vars <- c("","sex")


source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/hcpre33_nfhs5 total svydesign with 130 cutoff.R")

proportion_vars <- c("htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")

source("preprocessing/hcp_parallelize.R")


district_svysummary <- future_map_dfr(group_vars,
                                   function(g_v){
                                     id_vars = c("district_df",g_v);
                                     print(g_v);
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
                                       # dplyr::filter(n > 100) %>% 
                                       mutate(stratification = g_v) %>% 
                                       rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                       mutate_at(vars(one_of("strata")),~as.character(.));
                                     
                                     return(n5_out)
                                     
                                   })

district_svysummary <- district_svysummary %>% 
  rename(REGCODE = district_df) %>% 
  # There are missing values in D_CODE from subsetting on map
  dplyr::filter(!is.na(REGCODE)) %>% 
  left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),"mapnfhs5_sdist") %>% 
              dplyr::select(REGCODE,n5_state,v024,REGNAME),
            by=c("REGCODE")) 

write_csv(district_svysummary,file = "cutoff 130 and 80/hcc130a04_district2018 level care cascade.csv")

# df <- read_csv("cutoff 130 and 80/hcc130a04_district2018 level care cascade.csv") %>% 
#   dplyr::select(-n5_state,-v024,-D_NAME) %>% 
#   left_join(readxl::read_excel(paste0(path_dmcascade_repo,"/data/NFHS Cascade Variable List.xlsx"),"map2018_sdist") %>%
#               dplyr::select(D_CODE,n5_state,v024,D_NAME) %>%
#               mutate(D_CODE = sprintf("%03d",as.numeric(D_CODE))),
#             by=c("D_CODE"))
# write_csv(df,file = "cutoff 130 and 80/hcc130a04_district2018 level care cascade.csv")
# 
  