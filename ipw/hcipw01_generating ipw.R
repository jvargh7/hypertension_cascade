source("preprocessing/hcpre06_population characteristics.R")

require(survey)

ipw_model <- svyglm(status_binary ~ residence + sex + age_category5 + education + caste + religion + 
                      swealthq_ur + factor(state),
              design = nfhs5pop_svydesign,family = quasipoisson())

ipw_weights <- predict(ipw_model,type="response")

nfhs5pop_df %>% 
  mutate(ipw = ipw_weights,
         sampleweight_ipw = ipw*sampleweight) %>% 
  dplyr::select(state,cluster,psu,hhid,linenumber,ipw,sampleweight,sampleweight_ipw) %>% 
  saveRDS(.,paste0(path_cascade_folder,"/working/ipw_df.RDS"))
