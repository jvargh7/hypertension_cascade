source("preprocessing/hcpre03_nfhs5 total svydesign.R")

rm(nfhs5pop_svydesign)

nfhs5_df <- nfhs5_df %>% 
  left_join(readRDS(paste0(path_cascade_folder,"/working/ipw_df.RDS")) %>% 
              dplyr::select(psu,hhid,linenumber,sampleweight_ipw))

nfhs5popipw_svydesign <- nfhs5popipw_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight_ipw,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")