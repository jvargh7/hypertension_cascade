
group_vars = c("","residence","sex")

source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/hcpre15_nfhs5 diagnosed svydesign with last2bp.R")

pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

nfhs5htndiagz_svydesign <- svystandardize(nfhs5htndiag_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                          population = pop_age)


figure_diagnosed_df <- map_dfr(group_vars,
                               function(g_v){
                                 print(g_v);
                                 
                                 if(g_v == ""){
                                   n5_sy_htn <- svysummary(nfhs5htndiagz_svydesign,
                                                           # c_vars = continuous_vars,
                                                           # p_vars = proportion_vars[1:4],
                                                           g_vars = "htn_diagnosis_type"
                                   ) %>% 
                                     mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                     mutate(est_ci = paste0(estimate," (",
                                                            lci,", ",uci,")"))  %>% 
                                     mutate(g_v = g_v);
                                 } 
                                 if(g_v != ""){
                                   n5_sy_htn <- svysummary(nfhs5htndiagz_svydesign,
                                                           # c_vars = continuous_vars,
                                                           # p_vars = proportion_vars[1:4],
                                                           g_vars = "htn_diagnosis_type",
                                                           id_vars = g_v
                                   ) %>% 
                                     mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                     mutate(est_ci = paste0(estimate," (",
                                                            lci,", ",uci,")")) %>% 
                                     mutate(g_v = g_v);
                                 }
                                 
                                 
                                 
                                 return(n5_sy_htn)
                                 
                               })

write_csv(figure_diagnosed_df,"paper/figure_type of diagnosed hypertension with last2bp.csv")

figure_diagnosed = figure_diagnosed_df %>% 
  dplyr::select(residence,sex,group,estimate,lci,uci) %>% 
  mutate(strata = case_when(is.na(residence) & is.na(sex) ~ "Total",
                            !is.na(residence) ~ residence,
                            !is.na(sex) ~ sex)) %>% 
  mutate(strata = factor(strata,levels=c("Total","Rural","Urban","Male","Female"))) %>% 
  ggplot(data=.,aes(x=strata,ymin=lci,ymax=uci,y=estimate,fill=group,label=paste0(estimate," [",lci,",",uci,"]"))) +
  geom_col() +
  geom_text(position = position_stack(vjust=0.5)) +
  theme_bw() +
  xlab("") +
  ylab("Percentage (%)") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="",values=c("darkgreen","orange","lightgreen","red"))

figure_diagnosed %>% 
  ggsave(.,filename=paste0(path_cascade_folder,"/figures/figure_type of diagnosed hypertension with last2bp.png"),width=8,height=6)
