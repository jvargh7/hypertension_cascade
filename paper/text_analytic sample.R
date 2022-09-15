bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
            mutate(sex = "Female"),
          readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
            mutate(sex = "Male")) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  group_by(sex,residence,is.na(htn_free),is.na(diagnosed_htn),is.na(sbp)) %>% 
  tally() %>% 
  write_csv(.,paste0("paper/text_analytic sample exclusions.csv"))
