##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Figure 1ab                                                                        ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)
library(lubridate)
library(sf)

## average PM2.5 (Figure 1a)
PM25.ave <- data %>%
  group_by(GEOID) %>%
  summarise(PM25_mean = mean(PM25))

## PM2.5 change (Figure 1b)
PM25.2000 <- data %>%
  filter(year==2000) %>%
  group_by(GEOID) %>%
  summarise(PM25_2000 = mean(PM25))

PM25.2016 <- data %>%
  filter(year==2016) %>%
  group_by(GEOID) %>%
  summarise(PM25_2016 = mean(PM25))

PM25.change <- PM25.2000 %>%
  left_join(PM25.2016, by=c("GEOID")) %>%
  mutate(PM25_chg = (PM25_2016 - PM25_2000)/PM25_2000 * 100) %>%
  dplyr::select(GEOID, PM25_chg)


## join with shapefile
PM25.ave$GEOID <- as.character(PM25.ave$GEOID)
PM25.change$GEOID <- as.character(PM25.change$GEOID)

US.county.F1 <- US.county %>%
  left_join(PM25.ave, by="GEOID") %>%
  left_join(PM25.change, by="GEOID")



