analytic_characteristics <- read_csv("analysis/hca07_analytic sample characteristics.csv") %>% 
  dplyr::select(residence,strata,variable,group,est_ci)

analytic_characteristics %>% 
  pivot_wider(names_from=c("residence","strata"),values_from="est_ci") %>% 
  write_csv(.,"paper/table_analytic sample characteristics.csv")
