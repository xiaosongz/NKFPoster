# extract table one from result table
# load data ---------------------------------------------------------------

source("DataPrep.R")
demo_vars <- c("n",
               "EL_SEX_CD",
               "race (%)",
               "White",
               "Black",
               "Native American",
               "Asian or PI",
               "Hispanic",
               "NH PI",
               "NA.",
               "age (mean (sd))")
 # as.character((data %>% filter(year== 2013))$var)[c(1:10,60)]

tab1 <- data %>% filter(year == 2013) %>%
  select(var,Overall,peds,pedsCKD,adult,adultCKD) %>%
    filter(var %in% demo_vars) %>% write_csv(path = "data/results/tab1.csv")



source("Datapeds.R")
demo_vars <- c("n",
               "EL_SEX_CD = M (%)",
               "race (%)",
               "White",
               "Black",
               "Native American",
               "Asian or PI",
               "Hispanic",
               "NH PI",
               "NA.",
               "age (mean (sd))")
# as.character((data %>% filter(year== 2013))$var)[c(1:10,60)]

tab1 <- data %>% filter(year == 2013) %>%
  select(var,Overall,PEDCKD,No_PEDCKD) %>%
  filter(var %in% demo_vars) %>% write_csv(path = "data/results/tab1_peds.csv")


