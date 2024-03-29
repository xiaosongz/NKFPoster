library(tidyverse)
library(stringr)

# static vars -------------------------------------------------------------

# define comos and ultilization------------------------------------------------------------

comos <- c("ANEMIA",
           "CHF",
           "DM",
           "HTN"
)

utils <- c("ED",
           "CVD_HOSP",
           "INF_HOSP")


# function to prepare data ------------------------------------------------


dataprep <- function(year) {
  data <- read_csv(paste0("data/results/",year,"/MainByAgeCKD.csv"))
  data <- data %>% rename(var = "X1",
                            peds = "[0,22)",
                            adult = "[22,120)",
                            pedsNoCKD = "[0,22):0",
                            pedsCKD = "[0,22):1",
                            adultNoCKD = "[22,120):0",
                            adultCKD = "[22,120):1"
  ) %>%
    mutate(year = year)
  return(data)
}
for (year in seq(2007,2013)) {
  print(year)
  data <- dataprep(year = year)
 # names(data)
  dim(data) %>% print()

  if (year == 2007) {

    out = data
    str(out) %>% print()
    print(paste0(year,"added"))

  }
  else{
    out = bind_rows(out,data)
    print(paste0(year,"added"))
  }

}

# get clean variable names ------------------------------------------------


var_c <- out %>% mutate(var_a = sub("\\ =.*","",x = var)) %>% select(var_a)

data <-  out %>%
  mutate(var_a = sub("\\ =.*","",x = var)) %>%
  rename(var_old = var,
         var = var_a)

# define function to extract n and prop from the combined string ----------

extract_prop <- function(combined) {
  prop_text = gsub(".*\\((.*?)\\)","\\1", x = combined)
  prop = as.numeric(prop_text)
  return(prop)
}
