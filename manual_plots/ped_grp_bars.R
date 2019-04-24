thisyear = 2013
vars = comos

# make barplot peds ------------------------------------------------------------

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

plotting_df <-
  plot_data %>%
  select(var,pedsNoCKD_prop,pedsCKD_prop) %>%
  gather(key = "group", value = "prop",
         pedsNoCKD_prop,pedsCKD_prop)
# create dynamic lower and upper limit based on ploting data
axis_limit <- max(plotting_df$prop) *1.15;axis_limit
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
  scale_y_continuous(limits = c(0,20),#set the lower and upper limit for x axis(flipped)
                     breaks = seq(-100, 100, 5),
                     labels = abs(seq(-100, 100, 5))) +
  labs(x = "Comobidity", y = "Proportion (%) of Children", title = paste0(thisyear," Pediatric ","Comos", " diagnosis by CKD status")) +
  ggthemes::theme_tufte() +
  ggsci::scale_fill_nejm(labels = c("CKD","No-CKD")) +
  theme(plot.title = element_blank(),
        legend.title=element_blank(),
        legend.position = "top",
        #axis.title = element_text(family = "sans",size = 14),
        text = element_text(family = "sans", size = 14)) +
  # reverse the order of items in legend
  guides(fill = guide_legend(reverse = TRUE))

print(p)

create_pptx(p,path = "plots/ppt/test.pptx")

thisyear = 2013
vars = utils

# make barplot peds ------------------------------------------------------------

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

plotting_df <-
  plot_data %>%
  select(var,pedsNoCKD_prop,pedsCKD_prop) %>%
  gather(key = "group", value = "prop",
         pedsNoCKD_prop,pedsCKD_prop)
# create dynamic lower and upper limit based on ploting data
axis_limit <- max(plotting_df$prop) *1.15;axis_limit
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
  scale_y_continuous(#set the lower and upper limit for x axis(flipped)
                     breaks = seq(-100, 100, 5),
                     labels = abs(seq(-100, 100, 5))) +
  labs(x = "Comobidity", y = "Proportion (%) of Children", title = paste0(thisyear," Pediatric ","Comos", " diagnosis by CKD status")) +
  ggthemes::theme_tufte() +
  ggsci::scale_fill_nejm(labels = c("CKD","No-CKD")) +
  theme(plot.title = element_blank(),
        legend.title=element_blank(),
        legend.position = "top",
        #axis.title = element_text(family = "sans",size = 14),
        text = element_text(family = "sans", size = 14)) +
  # reverse the order of items in legend
  guides(fill = guide_legend(reverse = TRUE))

print(p)

create_pptx(p,path = "plots/ppt/test.pptx")



