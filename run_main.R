# ANALYSIS ------

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca02_national level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca03_state level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca05_state unmet need care cascade.R")



rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca06_population characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca07_analytic sample characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca09_national unmet need care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca10_total population characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca11_total state level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca12_total state unmet need care cascade.R")

# AGE STANDARDIZED --------
rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz01_age standardized national care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz02_age standardized state cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz03_age standardized national unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz04_age standardized state unmet care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz08_age standardized national conditional cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz09_age standardized screening among undiagnosed.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz10_age standardized analytic sample summary.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz14_age standardized estimates among hypertension.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz15_age standardized total state cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz16_age standardized total unmet state cascade.R")


# PAPER -------
rm(list=ls()); gc(); source(".Rprofile")
source("paper/abstract_national cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/abstract_crude national cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_heatmap cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_maps state care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_national care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_type of diagnosed hypertension.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_column plot cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_analytic sample characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_population characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_sociodemographic population care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_crude sociodemographic population care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_sociodemographic population care cascade total htn.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/text_analytic sample.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/text_state WHO compact.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_conditional care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_violin plots for example states.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_type of diagnosed hypertension.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_scatterplot of district estimates.R")

# NEW SENSITIVITY ANALYSIS ----------

rm(list=ls()); gc(); source(".Rprofile")
source("sensitivity last2bp/hcz11_age standardized national care cascade with last2bp.R")

rm(list=ls()); gc(); source(".Rprofile")
source("sensitivity last2bp/hcz13_age standardized national unmet care cascade with last2bp.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_sociodemographic population care cascade with last2bp.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/abstract_national cascade with last2bp.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_type of diagnosed hypertension with last2bp.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/text_distribution of BP measures.R")


# ANALYSIS: district ------

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca04_district2018 level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca08_district unmet need care cascade.R")

# AGE STANDARDIZED: district --------

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz05_age standardized district unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz06_age standardized district cascade.R")



rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_maps district care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_maps district care cascade without 50 obs.R")




# SHINY --------

rm(list=ls()); gc(); source(".Rprofile")
source("hypertension_cascade/code/setup_code.R")
































