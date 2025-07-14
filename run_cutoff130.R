
# Crude --

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a02_national level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a03_state level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a05_state unmet need care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a09_national unmet need care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a09_national unmet need care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a11_total state level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a12_total state unmet need care cascade.R")


# Age-standardized --
rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z01_age standardized national care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z02_age standardized state cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z03_age standardized national unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z04_age standardized state unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z15_age standardized total state cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z16_age standardized total unmet state cascade.R")


# District - Crude
rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a04_district2018 level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130a08_district unmet need care cascade.R")


# District - age standardized
rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z05_age standardized district unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff 130 and 80/hcc130z06_age standardized district cascade.R")