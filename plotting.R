# ploting figures

# load data ---------------------------------------------------------------

source("DataPrep.R")


# source functions --------------------------------------------------------

library(ggplot2)
library(purrr)
# overall cohort
source("func_line_groups.R")
purrr::map(c("CKD"),linegroups_age)
purrr::map(comos,linegroups_age)
purrr::map(utils,linegroups_age)
purrr::map(comos,linegroups_age_CKD)
purrr::map(utils,linegroups_age_CKD)
# peds cohort
source("Datapeds.R")
purrr::map(comos,peds_linegroups_byage)
purrr::map(utils,peds_linegroups_byage)
purrr::map(c("CKD"),peds_linegroups_byage)

# maps --------------------------------------------------------------------

source("CKDmap_bycty.R")
list <- c("anemia", "dm", "htn", "ed",  "ckd")



  for (co in list) {
    for (yr in seq(2013,2013)) {
    #map_by_state(state = "michigan", como = co,yr)
      map_by_cty(state = "michigan", como = co,yr)
  }

}

# back to back bar plots --------------------------------------------------

source("func_b2b_plot.R")


# b2b_age_ckd(2013,comos,"comos")
years <- seq(2007,2013)

# years %>% purrr::map(b2b_age_ckd(thisyear= .,vars = comos,name = "comos"))
for (year in years) {
  b2b_age_ckd(year,vars = comos,name = "comos")
  b2b_age_ckd(year,vars = utils,name = "utilization")
}


# grouped bar plots -------------------------------------------------------

source("func_grp_bar_plot.R")

years <- seq(2007,2013)

# years %>% purrr::map(b2b_age_ckd(thisyear= .,vars = comos,name = "comos"))
for (year in years) {
  grpbar_age_ckd(year, vars = comos, name = "comos")
  grpbar_age_ckd(year, vars = utils, name = "utilization")
}
