
# All adults who are not eligible but for whom glucose and BP are collected come from the sampled hh if anti_join() of nfhs5_hh is null

nfhs5_df <- bind_rows(readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                        mutate(sex = "Female"),
                      readRDS(paste0(path_dmcascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                        mutate(sex = "Male")) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE) %>% 
  mutate(dm_disease_cat = case_when(is.na(dm_disease) ~ "Missing",
                                    dm_disease == 1 ~ "Yes",
                                    dm_disease == 0 ~ "No"))


# NFHS-5 Household Recode
iahr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iahr7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))
nfhs5_hh <- haven::read_dta(paste0(path_dhs_data,"/IA/IAHR7ADT/IAHR7AFL.dta"),col_select = iahr7a_variables$selected)

# HV027: 
# Household selected for male interview  
# 0  Not selected
# 1  Men's survey
# 2  Husband's survey
# HV020:
# Ever-married sample             
# 0  All woman sample
# 1  Ever married sample


table(nfhs5_hh$hv020,nfhs5_hh$hv027,useNA="always")

# 0      1   <NA>
#   0    540706  95993      0
# <NA>      0      0      0
other_hh <- nfhs5_df %>% 
  anti_join(nfhs5_hh,
            by=c("cluster"="hv001","hhid" = "hv002"))

nrow(other_hh)
# [1] 0