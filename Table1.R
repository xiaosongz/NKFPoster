# extract table one from result table

demo_vars <- as.character(tab1$var)[c(1:10,60)]

tab1 <- data %>% filter(year == 2013) %>%
  select(var,Overall,peds,adult) %>%
    filter(var %in% demo_vars) %>% write_csv(path = "data/results/tab1.csv")

