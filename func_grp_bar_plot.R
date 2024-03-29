# This is a function to map sets of back to back bar plots for PAS/NKF abstract/poster
# COMO comparision by CKD and Age

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggsci)


grpbar_age_ckd <- function(thisyear,vars,name) {
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
           adultNoCKD_prop,adultCKD_prop)
    # a trick!
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
    geom_bar(stat = "identity",
             position = position_dodge(),# make it dodged bar plot
             width = 0.7) +
    coord_flip() +
    scale_x_discrete(limits = rev(vars)) +
    # another trick!
    scale_y_continuous(limits = c(0,axis_limit),#set the lower and upper limit for x axis(flipped)
                       breaks = seq(-100, 100, 10),
                       labels = abs(seq(-100, 100, 10))) +
    labs(x = "Comobidity", y = "% Diagnosis", title = paste0(thisyear,"Adult ",name, " diagnosis by CKD status"))+
    ggthemes::theme_tufte()+
    ggsci::scale_fill_nejm(labels = c("CKD","No-CKD"))+
    theme(
      #plot.title = element_blank(),
      legend.title=element_blank(),
      legend.position = "top",
      axis.title = element_text(family = "sans",size = 14),
      text = element_text(family = "sans", size = 14),
      legend.spacing.x = unit(1.0, 'cm')) +
    # reverse the order of items in legend
    guides(fill = guide_legend(reverse = TRUE))


  print(p)

  ggsave(height = 6,
         width = 6*1.6,
         filename = paste0("plots/grp_bars/pdf/",thisyear,"adult",name,"byCKD.pdf"))

  ggsave(height = 6,
         width = 6*1.6,
         filename = paste0("plots/grp_bars/png/",thisyear,"adult",name,"byCKD.png"))
  create_pptx(p,path = "plots/ppt/test.pptx")

  # make barplot peds ------------------------------------------------------------
  plotting_df <-
    plot_data %>%
    select(var,pedsNoCKD_prop,pedsCKD_prop) %>%
    gather(key = "group", value = "prop",
           pedsNoCKD_prop,pedsCKD_prop)
  # create dynamic lower and upper limit based on ploting data
  axis_limit <- max(plotting_df$prop) *1.1;axis_limit
  ## find the order
  temp_df <-
    plotting_df %>%
    filter(group == "pedsNoCKD_prop") %>%
    arrange(prop)
  # plot
  p <-
    plotting_df %>%
    ggplot(aes(x = var, y = prop, group = group, fill = group)) +
    geom_bar(stat = "identity",
             position = position_dodge(),
             width = 0.7) +
    coord_flip() +
    scale_x_discrete(limits = rev(vars)) +
    # another trick!
    scale_y_continuous(limits = c(0,axis_limit),#set the lower and upper limit for x axis(flipped)
                       breaks = seq(-100, 100, 10),
                       labels = abs(seq(-100, 100, 10))) +
    labs(x = "Comobidity", y = "% Diagnosis", title = paste0(thisyear," Pediatric ",name, " diagnosis by CKD status")) +
    ggthemes::theme_tufte() +
    ggsci::scale_fill_nejm(labels = c("CKD","No-CKD")) +
    theme(
          #plot.title = element_blank(),
          legend.title=element_blank(),
          legend.position = "top",
          axis.title = element_text(family = "sans",size = 14),
          text = element_text(family = "sans", size = 14),
          legend.spacing.x = unit(1.0, 'cm')) +
    # reverse the order of items in legend
    guides(fill = guide_legend(reverse = TRUE))

  print(p)

  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/grp_bars/pdf/",thisyear,"peds",name,"byCKD.pdf"))
  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/grp_bars/png/",thisyear,"peds",name,"byCKD.png"))
  create_pptx(p,path = "plots/ppt/test.pptx")
}



