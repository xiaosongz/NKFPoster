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
  data <- read_csv(paste0("data/results/",year,"/PedsMainByAgePEDCKD.csv"))
  data <- data %>% rename(var  = "X1",
                          PEDCKD = "1",
                          No_PEDCKD = "0",
                          age0_5 = "[0,5)",
                          age5_10 = "[5,10)",
                          age10_14 = "[10,14)",
                          age14_18 = "[14,18)",
                          age18_22 = "[18,22)"
                          # age0_5_nockd = "[0,5):0",
                          # age5_11_nockd = "[5,10):0",
                          # age11_14_nockd = "[10,14):0",
                          # age14_19_nockd = "[14,18):0",
                          # age19_22_nockd = "[18,22):0",
                          # age0_5ckd = "[0,5):1",
                          # age5_11ckd = "[5,10):1",
                          # age11_14ckd = "[10,14):1",
                          # age14_19ckd = "[14,18):1",
                          # age19_22ckd = "[18,22):1"
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

ext_n <- function(combined) {
  n_text = gsub("(\\d +)\\((.*?)\\)","\\1", x = combined)
  n = as.numeric(n_text)
  return(n)
}

