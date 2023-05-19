rm(list=ls());gc();source(".Rprofile")

x_locations = c(0,20,40,60,80)
names(x_locations) = c("Total","Urban","Rural","Male","Female")

df_single_panel <- function(df,group_title){
  
 
  df2 = df  %>% 
    group_by(variable == "htn_disease") %>% 
    mutate(lci_x = max(lci),
           uci_x = max(uci)) %>% 
    ungroup() %>% 
    dplyr::select(variable,estimate,lci_x,uci_x)
  
  df3 = df2 %>% 
    dplyr::filter(variable == "htn_diagnosed")
  
  df_untreated = data.frame(
    variable = "htn_untreated",
    estimate = df3$estimate - df2[df2$variable == "htn_treated",]$estimate,
    lci_x = df2[df2$variable == "htn_treated",]$lci_x,
    uci_x = df2[df2$variable == "htn_treated",]$uci_x
  )
  
  df_uncontrolled = data.frame(
    variable = "htn_uncontrolled",
    estimate = df2[df2$variable == "htn_treated",]$estimate - df2[df2$variable == "htn_controlled",]$estimate,
    lci_x = df2[df2$variable == "htn_treated",]$lci_x,
    uci_x = df2[df2$variable == "htn_treated",]$uci_x
  )
  
  df_out = bind_rows(
    df2 %>% dplyr::filter(variable %in% c("htn_disease","htn_controlled")),
    df_untreated,
    df_uncontrolled
  ) %>% 
    mutate(group = group_title) %>% 
    mutate(group_x = case_when(variable == "htn_disease" ~ 0 + x_locations[group_title],
                               TRUE ~ 5 + x_locations[group_title])) %>% 
    mutate(variable = factor(variable,
                             levels=c("htn_disease","htn_untreated","htn_uncontrolled","htn_controlled"),
                             labels=c("Hypertension","Untreated","Treated and Uncontrolled","Treated and Controlled")))
  
  
  return(df_out)
  
  
  
  
}


total <- read_csv("age_standardized/hcz01_total age standardized national care cascade.csv") %>% 
  dplyr::filter(is.na(sex),variable !="htn_screened") %>% 
  df_single_panel(.,group_title = "Total")

rural <- read_csv(file = "age_standardized/hcz01_age standardized national care cascade.csv") %>% 
  mutate(cascade = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
  dplyr::filter(residence == "Rural",is.na(stratification),variable !="htn_screened") %>% 
  df_single_panel(.,group_title = "Rural")

urban <- read_csv(file = "age_standardized/hcz01_age standardized national care cascade.csv") %>% 
  mutate(cascade = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
  dplyr::filter(residence == "Urban",is.na(stratification),variable !="htn_screened") %>% 
  df_single_panel(.,group_title = "Urban")

male <- read_csv(file = "age_standardized/hcz01_age standardized national care cascade.csv") %>% 
  mutate(cascade = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
  dplyr::filter(is.na(residence),strata == "Male",variable !="htn_screened") %>% 
  df_single_panel(.,group_title = "Male")

female <- read_csv(file = "age_standardized/hcz01_age standardized national care cascade.csv") %>% 
  mutate(cascade = str_replace(variable,"htn_","") %>% str_to_title()) %>% 
  dplyr::filter(is.na(residence),strata == "Female",variable !="htn_screened") %>% 
  df_single_panel(.,group_title = "Female")

# https://stackoverflow.com/questions/43281303/combine-stack-and-dodge-with-bar-plot-in-ggplot2 --> trick to do both
# https://stackoverflow.com/questions/12592041/plotting-a-stacked-bar-plot/12592235#12592235 --> can do one or the other
axis_breaks = x_locations
fig_cascade <- bind_rows(total,
                           urban,
                           rural,
                           male,
                           female) %>% 
  ggplot(data=.,aes(x=group_x,y=estimate,ymin=lci_x,ymax=uci_x,fill=variable,label=round(estimate,1))) +
  geom_col(position = "stack") +
  geom_text(position = position_stack(vjust=0.5)) +
  geom_errorbar(width=0.1) +
  theme_bw() +
  xlab("") +
  ylab("Prevalence (%)") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="",values=c("#00A5E3","#FF5768",
                                                "#FFBF65",
                                                "#8DD7BF")) +
  scale_x_continuous(breaks=x_locations)
  


require(ggpubr)
fig_cascade %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/national care cascade single panel.png"),width=10,height=6)
