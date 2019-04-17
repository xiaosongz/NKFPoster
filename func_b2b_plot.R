# This is a function to map sets of back to back bar plots for PAS/NKF abstract/poster
# COMO comparision by CKD and Age

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggsci)


b2b_age_ckd <- function(thisyear,vars,name) {
  # data reshape for ggplot -------------------------------------------------

  plot_data <- data %>% filter(year == thisyear) %>%
    select(var,Overall,starts_with("peds"),starts_with("adult")) %>%
    mutate(overall_prop = extract_prop(Overall),
           peds_prop = extract_prop(peds),
           adult_prop = extract_prop(adult),
           adultNoCKD_prop = extract_prop(adultNoCKD),
           adultCKD_prop = extract_prop(adultCKD),
           pedsNoCKD_prop = extract_prop(pedsNoCKD),
           pedsCKD_prop = extract_prop(pedsCKD)) %>%
    select(var,ends_with("_prop")) %>%
    filter(var %in% vars)

  # prepare data for plotting

  plotting_df <-
    plot_data %>%
    select(var,adultNoCKD_prop,adultCKD_prop) %>%
    gather(key = "group", value = "prop",
           adultNoCKD_prop,adultCKD_prop) %>%
    # a trick!
    mutate(prop = if_else(group == "adultNoCKD_prop", -prop, prop))
  # create dynamic lower and upper limit based on ploting data
  axis_limit <- max(plotting_df$prop)*1.1;axis_limit
  ## find the order
  temp_df <-
    plotting_df %>%
    filter(group == "adultNoCKD_prop") %>%
    arrange(prop)
  # plot
  p <-
    plotting_df %>%
    ggplot(aes(x = var, y = prop, group = group, fill = group)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    scale_x_discrete(limits = rev(vars)) +
    # another trick!
    scale_y_continuous(limits = c(-axis_limit,axis_limit),#set the lower and upper limit for x axis(flipped)
                       breaks = seq(-100, 100, 10),
                       labels = abs(seq(-100, 100, 10))) +
    labs(x = "Comobidity", y = "% Diagnosis", title = paste0(thisyear,"Adult ",name, " diagnosis by CKD status"))+
    ggthemes::theme_tufte()+
    ggsci::scale_fill_nejm(labels = c("CKD","No-CKD"))+
    #scale_fill_discrete(labels = c("A","B"))+
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(),
          plot.title = element_text(hjust = 0.5),
          #panel.background = element_rect(fill =  "grey90")
    ) +
    # reverse the order of items in legend
    guides(fill = guide_legend(reverse = TRUE))


  print(p)

  ggsave(height = 6,
         width = 6*1.6,
         filename = paste0("plots/b2b_bars/",thisyear,"adult",name,"byCKD.pdf"))

  ggsave(height = 6,
         width = 6*1.6,
         filename = paste0("plots/b2b_bars/",thisyear,"adult",name,"byCKD.png"))

  # make barplot peds ------------------------------------------------------------
  plotting_df <-
    plot_data %>%
    select(var,pedsNoCKD_prop,pedsCKD_prop) %>%
    gather(key = "group", value = "prop",
           pedsNoCKD_prop,pedsCKD_prop) %>%
    # a trick!
    mutate(prop = if_else(group == "pedsNoCKD_prop", -prop, prop))
  # create dynamic lower and upper limit based on ploting data
  axis_limit <- max(plotting_df$prop)*1.1;axis_limit
  ## find the order
  temp_df <-
    plotting_df %>%
    filter(group == "pedsNoCKD_prop") %>%
    arrange(prop)
  # plot
  p <-
    plotting_df %>%
    ggplot(aes(x = var, y = prop, group = group, fill = group)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    scale_x_discrete(limits = rev(vars)) +
    # another trick!
    scale_y_continuous(limits = c(-axis_limit,axis_limit),#set the lower and upper limit for x axis(flipped)
                       breaks = seq(-100, 100, 10),
                       labels = abs(seq(-100, 100, 10))) +
    labs(x = "Comobidity", y = "% Diagnosis", title = paste0(thisyear," Pediatric ",name, " diagnosis by CKD status")) +
    ggthemes::theme_tufte() +
    ggsci::scale_fill_nejm(labels = c("CKD","No-CKD")) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          #legend.text = element_blank(),
          plot.title = element_text(hjust = 0.5),
          #panel.background = element_rect(fill =  "grey90")
    ) +
    # reverse the order of items in legend
    guides(fill = guide_legend(reverse = TRUE))

  print(p)

  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/b2b_bars/",thisyear,"peds",name,"byCKD.pdf"))
  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/b2b_bars/",thisyear,"peds",name,"byCKD.png"))
}



