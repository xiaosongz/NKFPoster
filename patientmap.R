prevbycty <- read_csv("data/results/prevbycty.csv")

names(prevbycty) <- names(prevbycty) %>% tolower()


library(choroplethr)
data("county.map")
data(df_pop_county)

state.name.lower <-  state.name %>% tolower()


# map by county


map_by_cty <- function(state ="michigan",yr,como) {

  df_cty <- prevbycty %>%
    filter( county != "" & year == yr) %>%
    mutate(region = 26*1000+county) %>%
    rename("target" = n)  %>%
    left_join(df_pop_county, by = "region") %>%
    rename("pop" = value,
           "target" = target) %>%
    filter(target >=10) %>%
    mutate(value = target*1000/pop)

  county_choropleth(df_cty,
                    state_zoom = "michigan",
                    #county_zoom = "26161",
                    title      = paste0(yr," ",
                                        # state,
                                        paste0(" Michigan State prev ", como," by county")),
                    legend     = paste0(como, " per 1000 residents"),
                    num_colors = 8)

  file_name <- paste0("plots/maps/",como,"/",yr,state,"_",como,"byres")
  ggsave(paste0(file_name,".png"),height = 12, width = 14)
  ggsave(paste0(file_name,".pdf"),height = 12, width = 14)



}

prevbycty_peds <- read_csv("data/results/pedsbycty.csv")

names(prevbycty_peds) <- names(prevbycty_peds) %>% tolower()


library(choroplethr)
data("county.map")
data(df_pop_county)

state.name.lower <-  state.name %>% tolower()

map_by_cty_peds <- function(state ="michigan",yr,como) {

  df_cty <- prevbycty_peds %>%
    filter( county != "" & year == yr) %>%
    mutate(region = 26*1000+county) %>%
    rename("target" = n)  %>%
    left_join(df_pop_county, by = "region") %>%
    rename("pop" = value,
           "target" = target) %>%
    filter(target >=10) %>%
    mutate(value = target*1000/pop)

  county_choropleth(df_cty,
                    state_zoom = "michigan",
                    #county_zoom = "26161",
                    title      = paste0(yr," ",
                                        # state,
                                        paste0(" Michigan State prev ", como," by county")),
                    legend     = paste0(como, " per 1000 residents"),
                    num_colors = 8)

  file_name <- paste0("plots/maps_peds/",como,"/",yr,state,"_",como,"byres")
  ggsave(paste0(file_name,".png"),height = 12, width = 14)
  ggsave(paste0(file_name,".pdf"),height = 12, width = 14)

}
