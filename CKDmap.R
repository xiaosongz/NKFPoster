library(tidyverse)
library(zipcode)
library(ggplot2)
library(maps)
library(viridis)

prevbyzip <- read_csv("data/results/prevbyzip.csv")
prevbyzip$zip <- clean.zipcodes(prevbyzip$Zipcode)

# merge with zipcode geoinfo
data(zipcode)
ff_dist_geo <- ff_dist %>%
  inner_join(zipcode,by = "zip")

us <- map_data('state')

#devtools::install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)
library(choroplethr)
data(df_pop_zip)
data("zip.map")
yr = 2013
state = "michigan"


df_zip <- prevbyzip %>%
  mutate(region = zip) %>%
  right_join(df_pop_zip, by = "region") %>%
  rename("pop" = value) %>%
  mutate(value = CKD*1000/pop)


 zip_choropleth(df_zip,
               state_zoom = "michigan",
               #county_zoom = "26161",
               title      = paste0(yr," Michigan State prev CKD map"),
               legend     = "CKD prev per 1000 residents",num_colors = 9)
ggsave("maps/michigan.png")


state.name.lower <-  state.name %>% tolower()
map_by_state <- function(state,yr) {
  tomap <- df_zip %>% filter(year == yr)
  zip_choropleth(tomap,
                 state_zoom = state,
                 #county_zoom = "26161",
                 title      = paste0(yr," ",
                                     state,
                                     " Michigan State prev CKD by zipcode"),
                 legend     = "CKD prev per 1000 residents",
                 num_colors = 9)
  ggsave(paste0("plots/maps/",yr,state,".png"))
}

for (yr in seq(2007,2013)) {
  map_by_state(state = "michigan", yr)
}
