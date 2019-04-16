# line groups function
peds_linegroups <- function(thevar) {

  draw <- data %>% select(year,var,Overall,starts_with("age")) %>%
    filter(var %in% c('n',!!thevar)) %>% #get cells for both denominator n and the varaible we want
    mutate_at(vars(Overall,starts_with("age")),list(ext_n)) %>% # extract counts(n) from the combined cell text and convert to `double``
    select(var,year, Overall,matches("\\d$")) %>% # select columns of need
    gather(key = "key",value = "value",
           Overall,starts_with("age")) %>% # create a long format, create key for all vars except "var"
    spread(key = var,value = value) %>% # spread the !!thevar and n into two columns for calculate the %
    rename(count = !!thevar) %>% # rename the !!thevar column to "count", workaound for "non-numeric argument to binary operator" problem
    mutate(prop = ((count)/n*100)) # create new prop % variable for drawing

  p <- ggplot(data = draw,
              aes(x = year,
                  y = prop,
                  group = key,
                  color = key))
  p + geom_line(size = 2)+
    ggthemes::theme_base()+
    ggsci::scale_color_nejm(#labels = c("Adult CKD","Adult No-CKD","Peds CKD","Peds No-CKD")
      )+
    ylab(label = paste0(thevar,"( %)"))+
    ggtitle(paste0(thevar," trend by age group")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right")+
    scale_fill_discrete(name = "Patient group") +
    scale_x_continuous(breaks = pretty(draw$year))+
    theme(legend.title=element_blank(),
          legend.position = "top")

  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/trends/pedspdf",thevar,"TrendByAge_PEDCKD.pdf"))
  ggsave(height = 6,
         width = 6*1.61,
         filename = paste0("plots/trends/pedspng",thevar,"TrendByAge_PEDCKD.png"))
}


