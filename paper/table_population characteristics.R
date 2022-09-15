population_characteristics <- read_csv("analysis/hca06_population characteristics.csv") %>% 
  dplyr::select(residence,strata,variable,group,est_ci)

population_characteristics %>% 
  pivot_wider(names_from=c("residence","strata"),values_from="est_ci") %>% 
  write_csv(.,"paper/table_population characteristics.csv")
