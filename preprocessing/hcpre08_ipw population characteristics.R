source("preprocessing/hcpre06_population characteristics.R")

rm(nfhs5pop_svydesign)

nfhs5pop_df <- nfhs5pop_df %>% 
  left_join(readRDS(paste0(path_cascade_folder,"/working/ipw_df.RDS")) %>% 
              dplyr::select(psu,hhid,linenumber,sampleweight_ipw),
            by = c("psu","hhid","linenumber"))



nfhs5pop_svydesign <- nfhs5pop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight_ipw,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")