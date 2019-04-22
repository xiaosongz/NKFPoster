data(county.fips)

prevbycty_peds <- read_csv("data/results/pedsbycty.csv")

names(prevbycty_peds) <- names(prevbycty_peds) %>% tolower()


library(choroplethr)
data("county.map")
data(df_pop_county)

state_name_lower <-  state.name %>% tolower()



  df_cty <- prevbycty_peds %>%
    filter( county != "" & year == yr) %>%
    mutate(region = 26*1000+county) %>%
    rename("target" = !!como,
           "pts"  = n)  %>%
    left_join(df_pop_county, by = "region") %>%
    rename("pop" = value,
           "target" = target) %>%
    filter(target >=10) %>%
    mutate(value = target*1000/pts)

df_cty %>%
  rename("fips" = region )%>%
  left_join(county.fips, by = "fips") %>%
  mutate(county = stringr::str_split(polyname,pattern = ",",simplify = T)[,2]) %>%
  rename("pedckd" = target,
         "pedckd_per_1000_pts" = value) %>%
  arrange(desc(pedckd_per_1000_pts)) %>% write_csv("data/results/top_peds_CKD.csv")
df_cty$target %>% sum


data(county.fips)

prevbycty <- read_csv("data/results/prevbycty.csv")

names(prevbycty) <- names(prevbycty) %>% tolower()


library(choroplethr)
data("county.map")
data(df_pop_county)

state_name_lower <-  state.name %>% tolower()



df_cty <- prevbycty %>%
  filter( county != "" & year == yr) %>%
  mutate(region = 26*1000+county) %>%
  rename("target" = "ckd",
         "pts"  = n)  %>%
  left_join(df_pop_county, by = "region") %>%
  rename("pop" = value,
         "target" = target) %>%
  filter(target >=10) %>%
  mutate(value = target*1000/pts)

df_cty %>%
  rename("fips" = region )%>%
  left_join(county.fips, by = "fips") %>%
  mutate(county = stringr::str_split(polyname,pattern = ",",simplify = T)[,2]) %>%
  rename("pedckd" = target,
         "pedckd_per_1000_pts" = value) %>%
  arrange(desc(pedckd_per_1000_pts)) %>% write_csv("data/results/top_CKD.csv")
df_cty$target %>% sum

