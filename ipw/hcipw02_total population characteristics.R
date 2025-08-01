group_vars <- c("status")


source("C:/code/external/functions/survey/svysummary.R")
# Output: id_vars, variable, group, estimate, lci, uci


source("preprocessing/hcpre08_ipw population characteristics.R")

continuous_vars <- c("bmi","age")
proportion_vars <- c("highwc","htn","highbp","diaghtn",
                     "htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")
grouped_vars <- c("residence","sex","age_category","age_category10","age_category5","education",
                  "caste","religion","wealthq_ur","bmi_category")

id_vars = c(group_vars);
n5_sy <- svysummary(nfhs5pop_svydesign,
                    c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    g_vars = grouped_vars,
                    id_vars = c(group_vars)
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

# Count of non-NA values at intersection of id_vars and each variable in proportion_vars
n5_ct <- nfhs5pop_df %>% 
  group_by_at(vars(one_of(id_vars))) %>% 
  summarize_at(vars(one_of(c(
    continuous_vars,
    proportion_vars,
    grouped_vars
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
  mutate(variable = str_replace(variable,"_n$",""));

n5_out <- left_join(n5_sy,
                    n5_ct,
                    by=c(id_vars[id_vars!=""],"variable")) %>% 
  
  # Restrict to those cells with more than 100 observations
  dplyr::filter(n > 100) %>% 
  mutate(stratification = group_vars) %>% 
  rename_at(vars(one_of(group_vars)),~c("strata")) %>% 
  mutate_at(vars(one_of("strata")),~as.character(.))



n5_out %>% 
  write_csv(.,file = "ipw/hcipw02_total population characteristics.csv")


n5_out %>% 
  dplyr::select(strata,variable,group,est_ci) %>% 
  pivot_wider(names_from=c("strata"),values_from="est_ci") %>% 
  write_csv(.,"ipw/hcipw_total population characteristics.csv")
