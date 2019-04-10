# ploting figures

# load data ---------------------------------------------------------------

source("DataPrep.R")


# source functions --------------------------------------------------------

library(ggplot2)
library(purrr)
source("func_line_groups.R")
purrr::map(comos,linegroups)
purrr::map(utils,linegroups)

# maps --------------------------------------------------------------------

source("CKDmap.R")
list <- c("anemia", "dm" ,      "htn" ,     "ed" ,      "cvd_hosp", "inf_hosp","ckd")



  for (co in list) {
    for (yr in seq(2007,2013)) {
    map_by_state(state = "michigan", como = co,yr)
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
  grpbar_age_ckd(year,vars = comos,name = "comos")
  grpbar_age_ckd(year,vars = utils,name = "utilization")
}
