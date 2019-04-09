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


