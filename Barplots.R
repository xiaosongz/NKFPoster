# COMO comparision by CKD and Age

# source data prep code ---------------------------------------------------

source("DataPrep.R")

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggsci)

# define comos ------------------------------------------------------------

comos <- c("DM",
           "HTN",
           "CHF",
           "CVD",
           "AKI",
           "CVD_HOSP",
           "GI",
           "ANEMIA")
# data reshape for ggplot -------------------------------------------------


plot_data <- data %>% filter(year == 2013) %>%
  select(var,Overall,starts_with("peds"),starts_with("adult")) %>%
  mutate(overall_prop = extract_prop(Overall),
       peds_prop = extract_prop(peds),
       adult_prop = extract_prop(adult),
       adultNoCKD_prop = extract_prop(adultNoCKD),
       adultCKD_prop = extract_prop(adultCKD),
       pedsNoCKD_prop = extract_prop(pedsNoCKD),
       pedsCKD_prop = extract_prop(pedsCKD)) %>%
  select(var,ends_with("_prop")) %>%
  filter(var %in% comos) %>%
  write_csv(path = "data/results/comos2013.csv")


# make barplot adult ------------------------------------------------------------

# prepare data for plotting

plotting_df <-
  plot_data %>%
  select(var,adultNoCKD_prop,adultCKD_prop) %>%
  gather(key = "group", value = "prop",
         adultNoCKD_prop,adultCKD_prop) %>%
  # a trick!
  mutate(prop = if_else(group == "adultNoCKD_prop", -prop, prop))
## find the order
temp_df <-
  plotting_df %>%
  filter(group == "adultNoCKD_prop") %>%
  arrange(prop)
the_order <- temp_df$var
# plot
p <-
  plotting_df %>%
  ggplot(aes(x = var, y = prop, group = group, fill = group)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  # another trick!
  scale_y_continuous(breaks = seq(-100, 100, 10),
                     labels = abs(seq(-100, 100, 10))) +
  labs(x = "Comobidity", y = "% Diagnosis", title = "Back-to-back bar chart")+
  ggthemes::theme_economist()+
  ggsci::scale_fill_jama()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #panel.background = element_rect(fill =  "grey90")
  ) +
  # reverse the order of items in legend
  guides(fill = guide_legend(reverse = TRUE))
# change the default colors of bars
# scale_fill_manual(
#                   #values=c("red", "blue"),
#                   name="",
#                   breaks=c("adultCKD_prop", "adultNoCKD_prop"),
#                   labels=c("adultCKD_prop", "adultNoCKD_prop"))


print(p)

ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/adultCOMObyCKD.pdf")


# make barplot peds ------------------------------------------------------------
plotting_df <-
  plot_data %>%
  select(var,pedsNoCKD_prop,pedsCKD_prop) %>%
  gather(key = "group", value = "prop",
         pedsNoCKD_prop,pedsCKD_prop) %>%
  # a trick!
  mutate(prop = if_else(group == "pedsNoCKD_prop", -prop, prop))
## find the order
temp_df <-
  plotting_df %>%
  filter(group == "pedsNoCKD_prop") %>%
  arrange(prop)
the_order <- temp_df$var
# plot
p <-
  plotting_df %>%
  ggplot(aes(x = var, y = prop, group = group, fill = group)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  # another trick!
  scale_y_continuous(breaks = seq(-100, 100, 10),
                     labels = abs(seq(-100, 100, 10))) +
  labs(x = "Comobidity", y = "% Diagnosis", title = "Back-to-back bar chart")+
  ggthemes::theme_economist()+
  ggsci::scale_fill_jama()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #panel.background = element_rect(fill =  "grey90")
  ) +
  # reverse the order of items in legend
  guides(fill = guide_legend(reverse = TRUE))
# change the default colors of bars
# scale_fill_manual(
#                   #values=c("red", "blue"),
#                   name="",
#                   breaks=c("pedsCKD_prop", "pedsNoCKD_prop"),
#                   labels=c("pedsCKD_prop", "pedsNoCKD_prop"))


print(p)

ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/pedsCOMObyCKD.pdf")
