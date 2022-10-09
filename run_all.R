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



# PAPER -------
rm(list=ls()); gc(); source(".Rprofile")
source("paper/abstract_national cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_heatmap cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_maps state care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_national care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_analytic sample characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_population characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_sociodemographic population care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/text_analytic sample.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/text_state WHO compact.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_conditional care cascade.R")

# DISTRICT -------

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca04_district2018 level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/hca08_district unmet need care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz05_age standardized district unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz06_age standardized district cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/hcz07_age standardized missing caste stratified.R")


































