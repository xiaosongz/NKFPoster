library(tidyverse)
library(stringr)
dataprep <- function(year) {
  data <- read_csv(paste0("data/results/",year,"/MainByAgeCKD.csv"))
  data <- data %>% rename(var = "X1",
                            peds = "(0,21]",
                            adult = "(21,120]",
                            pedsNoCKD = "(0,21]:0",
                            pedsCKD = "(0,21]:1",
                            adultNoCKD = "(21,120]:0",
                            adultCKD = "(21,120]:1"
  ) %>%
    mutate(year = year)
  return(data)
}

# rm(data)
# rm(out)
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
