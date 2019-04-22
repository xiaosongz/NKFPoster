# ploting figures

# load data ---------------------------------------------------------------

source("Datapeds.R")

# Load libraries ----------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(ggsci)

draw <- data %>% select(year,var,Overall,peds,adult) %>%
  filter(var == "CKD") %>%
  mutate(overall_prop = extract_prop(Overall),
         peds_prop = extract_prop(peds),
         adult_prop = extract_prop(adult)) %>%
  select(year, ends_with("_prop")) %>%
  gather(key = "key",value = "value",
         overall_prop,peds_prop,adult_prop)

p <- ggplot(data = draw,
            aes(x = year,
                y = value,
                group = key,
                color = key))
p + geom_line(size = 2)+
  ggthemes::theme_economist()+
  ggsci::scale_fill_jama(labels = c("Adults","Overall", "Pediatrics"))+
  ggsci::scale_color_jama()+
  ylab(label = "Prevlence (%)")+
  ggtitle("CKD Diagnosis Trends by Patients' Age Group")+
  theme(plot.title = element_text(hjust = 0.5))


ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/CKDTrendByAgegroup.pdf")
ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/CKDTrendByAgegroup.png")
# ED visits trend by agegroups ----------------------------------------------


draw <- data %>% select(year,var,Overall,peds,adult) %>%
  filter(var == "ED") %>%
  mutate(overall_prop = extract_prop(Overall),
         peds_prop = extract_prop(peds),
         adult_prop = extract_prop(adult)) %>%
  select(year, ends_with("_prop")) %>%
  gather(key = "key",value = "value",
         overall_prop,peds_prop,adult_prop)

p <- ggplot(data = draw,
            aes(x = year,
                y = value,
                group = key,
                color = key))
p + geom_line(size = 2)+
  ggthemes::theme_economist()+
  #  ggsci::scale_fill_jama()+
  ggsci::scale_color_jama()+
  ylab(label = "Eemergency Room Usage (%)")+
  ggtitle("ED visits trend by age group") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/EDTrendByAgegroup.pdf")

ggsave(height = 6,
       width = 6*1.61,
       filename = "plots/EDTrendByAgegroup.png")

# line groups for CKD prevalence by agegroups -----------------------------

n <- data %>%select(year,var,Overall,starts_with("age")) %>%
  mutate_at(vars(Overall,starts_with("age")),list(ext_n)) %>% filter(var == "n")


draw <- data %>% select(year,var,Overall,starts_with("age")) %>%
  filter(var %in% c('n','CKD')) %>%
  mutate_at(vars(Overall,starts_with("age")),list(ext_n)) %>%
  select(var,year, Overall,matches("\\d$")) %>%
  gather(key = "key",value = "value",
         Overall,starts_with("age")) %>%
  spread(key = var,value = value) %>%
  mutate(prop = CKD/n*100)





p <- ggplot(data = draw,
            aes(x = year,
                y = prop,
                group = key,
                color = key))
p + geom_line(size = 2)+
  ggthemes::theme_economist()+
  ggsci::scale_color_nejm(
    #labels = c("Adults","Overall", "Pediatrics")
                          )+
  ylab(label = "Prevlence (%)")+
  ggtitle("PEDCKD Diagnosis Trends by Patients' Age Group")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/PEDCKDTrendByAgegroup.pdf")
ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/PEDCKDTrendByAgegroup.png")
# ED visits trend by agegroups ----------------------------------------------


draw <- data %>% select(year,var,Overall,peds,adult) %>%
  filter(var == "ED") %>%
  mutate(overall_prop = extract_prop(Overall),
         peds_prop = extract_prop(peds),
         adult_prop = extract_prop(adult)) %>%
  select(year, ends_with("_prop")) %>%
  gather(key = "key",value = "value",
         overall_prop,peds_prop,adult_prop)

p <- ggplot(data = draw,
            aes(x = year,
                y = value,
                group = key,
                color = key))
p + geom_line(size = 2)+
  ggthemes::theme_economist()+
#  ggsci::scale_fill_jama()+
   ggsci::scale_color_jama()+
  ylab(label = "Eemergency Room Usage (%)")+
  ggtitle("ED visits trend by age group") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(height = 6,
       width = 6*1.6,
       filename = "plots/EDTrendByAgegroup.pdf")

ggsave(height = 6,
       width = 6*1.61,
       filename = "plots/EDTrendByAgegroup.png")
# ED visits by Age and CKD status -----------------------------------------


draw <- data %>% select(year,var,Overall,peds,adult,
                        pedsNoCKD,adultNoCKD,pedsCKD,adultCKD) %>%
  filter(var == "ED") %>%
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
  ggsci::scale_color_jama()+
  ylab(label = "Eemergency Room Usage (%)")+
  ggtitle("ED visits trend by age group") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")+
  scale_fill_discrete(name = "Patient group")

ggsave(height = 6,
       width = 6*1.61,
       filename = "plots/EDTrendByAge_CKD.pdf")
ggsave(height = 6,
       width = 6*1.61,
       filename = "plots/EDTrendByAge_CKD.png")
