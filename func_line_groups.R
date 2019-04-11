# line groups function
linegroups <- function(thevar) {
  draw <- data %>% select(year,var,Overall,peds,adult,
                          pedsNoCKD,adultNoCKD,pedsCKD,adultCKD) %>%
    filter(var == thevar) %>%
    mutate(#overall_prop = extract_prop(Overall),
      #peds_prop = extract_prop(peds),
      #adult_prop = extract_prop(adult),
      adultNoCKD_prop = extract_prop(adultNoCKD),
      adultCKD_prop = extract_prop(adultCKD),
      pedsNoCKD_prop = extract_prop(pedsNoCKD),
      pedsCKD_prop = extract_prop(pedsCKD)
    ) %>%
    select(year, ends_with("_prop")) %>%
    gather(key = "key",value = "value",
           ends_with("_prop"))

  p <- ggplot(data = draw,
              aes(x = year,
                  y = value,
                  group = key,
                  color = key))
  p + geom_line(size = 2)+
    ggthemes::theme_economist()+
    #  ggsci::scale_fill_jama()+
    ggsci::scale_color_nejm(labels = c("Adult CKD","Adult No-CKD","Peds CKD","Peds No-CKD"))+
    ylab(label = paste0(thevar,"( %)"))+
    ggtitle(paste0(thevar," trend by age group")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right")+
    scale_fill_discrete(name = "Patient group") +
    theme(legend.title=element_blank())

  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/trends/",thevar,"TrendByAge_CKD.pdf"))
  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/trends/",thevar,"TrendByAge_CKD.png"))
}



# Peds linegroups function ------------------------------------------------
ped_linegroups <- function(thevar) {
  draw <- data %>% select(year,var,Overall,starts_with("age")) %>%
    filter(var == thevar) %>%
    mutate(overall = extract_prop(Overall),
           age0_4 = extract_prop(age0_5),
           age5_9 = extract_prop(age5_10),
           age10_13 = extract_prop(age10_14),
           age14_18 = extract_prop(age14_18),
           age18_21 = extract_prop(age18_22)
    ) %>%
    select(year, overall,age0_4,
             age5_9,
             age10_13,
           age14_18,
           age18_21) %>%
    gather(key = "key",value = "value",
           overall,starts_with("age"))

  p <- ggplot(data = draw,
              aes(x = year,
                  y = value,
                  group = key,
                  color = key))
  p + geom_line(size = 2)+
    ggthemes::theme_economist()+
    ggsci::scale_color_nejm(
      #labels = c("Age 0-4","Age 5-9", "Age 10-13", "Age 14-17","Age 18-21","Overall")
    )+
    ylab(label = paste0(thevar,"( %)"))+
    ggtitle(paste0(thevar," trend by age group")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right")+
    scale_fill_discrete(name = "Patient group") +
    theme(legend.title=element_blank())


  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/trends/",thevar,"TrendByAge_PEDCKD.pdf"))
  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/trends/",thevar,"TrendByAge_PEDCKD.png"))
}
