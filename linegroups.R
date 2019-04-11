# ploting figures

# load data ---------------------------------------------------------------

source("Datapeds.R")

# Load libraries ----------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(ggsci)


# line groups for CKD prevalence by agegroups -----------------------------


draw <- data %>% select(year,var,Overall,starts_with("age")) %>%
  filter(var == "CKD") %>%
  mutate(overall = extract_prop(Overall),
         age0_5 = extract_prop(age0_5),
         age5_11 = extract_prop(age5_11),
         age11_14 = extract_prop(age11_14),
         age14_19 = extract_prop(age14_19),
         age19_22 = extract_prop(age19_22)
         # age0_5_nockd = extract_prop(age0_5_nockd),
         # age5_11_nockd = extract_prop(age5_11_nockd),
         # age11_14_nockd = extract_prop(age11_14_nockd),
         # age14_19_nockd = extract_prop(age14_19_nockd),
         # age19_22_nockd = extract_prop(age19_22_nockd),
         # age0_5ckd = extract_prop(age0_5ckd),
         # age5_11ckd = extract_prop(age5_11ckd),
         # age11_14ckd = extract_prop(age11_14ckd),
         # age14_19ckd = extract_prop(age14_19ckd),
         # age19_22ckd = extract_prop(age19_22ckd)
         ) %>%
  select(year, overall,matches("\\d$")) %>%
  gather(key = "key",value = "value",
         overall,starts_with("age"))


draw <- data %>% select(year,var,Overall,starts_with("age")) %>%
  filter(var == "CKD") %>%
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
  select(year, overall,matches("\\d$")) %>%
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
