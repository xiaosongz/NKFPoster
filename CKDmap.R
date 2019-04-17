library(tidyverse)
library(zipcode)
library(ggplot2)
library(maps)
library(viridis)

prevbyzip <- read_csv("data/results/prevby.csv") %>% select(-ZIP)
prevbyzip$zip <- clean.zipcodes(prevbyzip$Zipcode)
names(prevbyzip) <- names(prevbyzip) %>% tolower()


#devtools::install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)
library(choroplethr)
data(df_pop_zip)
data("county.map")
data(df_pop_county)
data(zipcode)

state.name.lower <-  state.name %>% tolower()
map_by_state <- function(state,yr,como) {

  df_zip <- prevbyzip %>%filter(statecode == "MI" & year == yr) %>%
    mutate(region = zip) %>%
    left_join(df_pop_zip, by = "region") %>%
    rename("pop" = value,
           "target" = !!como) %>%
    mutate(value = target*1000/pop)


  zip_choropleth(df_zip,
                 state_zoom = state,
                 #county_zoom = "26161",
                 title      = paste0(yr," ",
                                     # state,
                                     paste0(" Michigan State prev ", como," by zipcode")),
                 legend     = paste0(como, " per 1000 residents"),
                 num_colors = 9)

  file_name <- paste0("plots/maps/",como,"/",yr,state,"_",como)
  ggsave(paste0(file_name,".png"),height = 12, width = 14)
  ggsave(paste0(file_name,".pdf"),height = 12, width = 14)
}

# map by county


map_by_cty <- function(state,yr,como) {

  df_cty <- prevbyzip %>%
    filter(statecode == "MI" & county != "" & year == yr) %>%
    mutate(region = state*1000+county) %>%
    rename("target" = !!como) %>%
    group_by(region) %>%
    summarise(target= sum(target)) %>%
    left_join(df_pop_county, by = "region") %>%
    rename("pop" = value,
           "target" = target) %>%
    filter(target >=10) %>%
    mutate(value = target*1000/pop)

county_choropleth(df_cty,
                 state_zoom = state,
                 #county_zoom = "26161",
                 title      = paste0(yr," ",
                                     # state,
                                     paste0(" Michigan State prev ", como," by county")),
                 legend     = paste0(como, " per 1000 residents"),
                 num_colors = 8)

  file_name <- paste0("plots/maps/",como,"/",yr,state,"_",como)
  ggsave(paste0(file_name,".png"),height = 12, width = 14)
  ggsave(paste0(file_name,".pdf"),height = 12, width = 14)
}
