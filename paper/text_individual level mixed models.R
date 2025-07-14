source("preprocessing/hcpre04_nfhs5 hypertension svydesign.R")
source("preprocessing/hcpre05_nfhs5 diagnosed svydesign.R")
source("preprocessing/hcpre10_nfhs5 treated svydesign.R")

library(lme4)
library(nloptr)
library(nlme)
library(optimx)
# control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
# glmerControl(optimizer ="nloptwrap")  --> default
# glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))

mod_diagnosed <- glmer(htn_diagnosed ~ age_category + education + sex + residence + wealthq_ur + (1|state/district_df),
                       data=nfhs5htn_df,
                       control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                       family = binomial())
summary(mod_diagnosed)
1 - performance::icc(mod_diagnosed,by_group = TRUE)$ICC[2]

mod_treated <- glmer(htn_treated ~ age_category + education + sex + residence + wealthq_ur  + (1|state/district_df),
                     data=nfhs5htndiag_df,
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                     family = binomial())
summary(mod_treated)
1 - performance::icc(mod_treated,by_group = TRUE)$ICC[2]


mod_controlled <- glmer(htn_controlled ~ age_category + education + sex + residence + wealthq_ur  + (1|state/district_df),
                        data=nfhs5htntreat_df,
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                        family = binomial())
summary(mod_controlled)
1 - performance::icc(mod_controlled,by_group = TRUE)$ICC[2]


data.frame(
           vpc_state_diagnosed = performance::icc(mod_diagnosed,by_group = TRUE)$ICC[1],
           vpc_district_diagnosed = performance::icc(mod_diagnosed,by_group = TRUE)$ICC[2],
           
           
           vpc_state_treated = performance::icc(mod_treated,by_group = TRUE)$ICC[1],
           vpc_district_treated = performance::icc(mod_treated,by_group = TRUE)$ICC[2],
           
           vpc_state_controlled = performance::icc(mod_controlled,by_group = TRUE)$ICC[1],
           vpc_district_controlled = performance::icc(mod_controlled,by_group = TRUE)$ICC[2]
           
           ) %>% 
  mutate(vpc_below_state_diagnosed = 1 - performance::icc(mod_diagnosed,by_group = TRUE)$ICC[1],
         vpc_below_state_treated = 1 - performance::icc(mod_treated,by_group = TRUE)$ICC[1],
         vpc_below_state_controlled = 1 - performance::icc(mod_controlled,by_group = TRUE)$ICC[1]
         ) %>% 
  
  write_csv(.,"paper/text_individual level mixed models.csv")
