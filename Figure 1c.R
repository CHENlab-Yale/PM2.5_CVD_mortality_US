##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Figure 1c                                                                         ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)
library(ggplot2)
library(cowplot)
library(MetBrewer)

## filter for cardiovascular mortality data
mortality.CVD  <- mortality %>%
  filter(ICD.category == "I" & ICD.number >= 0 & ICD.number <= 99.99) %>%
  dplyr::select(year_month, res_FIPS, race, hispanic) %>%
  rename(GEOID = res_FIPS) %>%
  mutate(race_cat = case_when(race==1 & hispanic==0 ~ "non-Hispanic White",
                              race==2 & hispanic==0 ~ "non-Hispanic Black",
                              hispanic==1 ~ "Hispanic"),
         race_cat = factor(race_cat, levels = c("non-Hispanic White","non-Hispanic Black","Hispanic"))) %>%
  dplyr::select(GEOID, year_month, race_cat) %>%
  filter(is.na(race_cat)==F)

rm(mortality)


## select PM2.5 data
data.AP <- data %>%
  dplyr::select(GEOID, year_month, year, PM25)

## 12-month moving average of PM2.5 concentration
data.list <- split(data.AP, as.factor(data$GEOID))
for (j in 1:length(data.list)){
  data.list[[j]][,paste0("PM25_lag0",11)]<- tsModel::runMean(data.list[[j]]$PM25, c(0:11))
}

data.AP.01to16 <- bind_rows(data.list) %>%
  filter(year >= 2001) %>%
  dplyr::select(-PM25,-year)


## individual-level long-term PM2.5 exposure
AP.individual <- mortality.CVD %>%
  left_join(data.AP, by=c("GEOID","year_month")) %>%
  left_join(data.AP.01to16, by=c("GEOID","year_month")) 

## make the violin plot
theme_set(theme_cowplot())

plot.individual.12M <- ggplot(AP.individual, aes(x=race_cat, y=PM25_lag011, fill=race_cat)) +
  geom_violin() +
  scale_fill_met_d("Kandinsky", direction = 1) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2)+
  xlab("")+
  ylab("12-month moving average of PM2.5") +
  theme(legend.position="none")

