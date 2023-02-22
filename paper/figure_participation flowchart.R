nfhs5_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                        mutate(sex = "Female"),
                      readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                        mutate(sex = "Male"))  %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         
         missing_bp = case_when(is.na(sbp)|is.na(dbp) ~ "Missing BP",
                                TRUE ~ "Available BP"),
         
         self_reported_highbp = case_when(diagnosed_bp == 1 ~ "Self-reported High BP",
                                          TRUE ~ "No Self-reported High BP")
         )


step1 = nfhs5_df %>% 
  dplyr::filter(self_reported_highbp == "Self-reported High BP",missing_bp == "Missing BP") %>% 
  group_by(sex,residence) %>% 
  tally()

step2 = nfhs5_df %>% 
  dplyr::filter(missing_bp == "Available BP") %>% 
  group_by(sex,residence) %>% 
  tally()
