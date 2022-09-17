population_characteristics <- read_csv("analysis/hca10_total population characteristics.csv") %>% 
  dplyr::select(strata,variable,group,est_ci)

population_characteristics %>% 
  pivot_wider(names_from=c("strata"),values_from="est_ci") %>% 
  write_csv(.,"paper/table_total population characteristics.csv")
