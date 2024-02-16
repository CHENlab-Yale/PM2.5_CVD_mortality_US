##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Figure 2a                                                                         ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)
library(lubridate)
library(sf)

## calculate monthly county-level attributable burden
AN.data <- data[,c("GEOID","year_month","year","pop.total")]

source("function_AN for each month.R")

results.table.CVD <- results.table %>%
  filter(outcome == "CVD.adj")

cause <- "CVD"
coef <- results.table.CVD$coef[1]
coef_low <- results.table.CVD$coef_low[1]
coef_up <- results.table.CVD$coef_up[1]

burden.df <- AN.allyears(data.all = data,
                         AP = "PM25_lag011",
                         population = "pop.total",
                         coef = coef,
                         coef_low = coef_low,
                         coef_up = coef_up)

AN.data[,paste0(cause,".AN")] <- burden.df[,"AN"]
AN.data[,paste0(cause,".AN_low")] <- burden.df[,"AN_low"]
AN.data[,paste0(cause,".AN_up")] <- burden.df[,"AN_up"]
  

## aggregate to county level
AN.data.county <- aggregate(AN.data[,-c(1:3)], by=list(as.factor(AN.data$GEOID),as.factor(AN.data$year)), FUN = sum)
AN.data.county <- rename(AN.data.county, GEOID = Group.1, year = Group.2)

## mean AN by county
AN.data.mean <- aggregate(AN.data.county[,-c(1:2)], by=list(as.factor(AN.data.county$GEOID)), FUN = mean)
AN.data.mean <- rename(AN.data.mean, GEOID = Group.1)

cols <- colnames(AN.data.mean)
cols.new <- gsub(".", "_", cols,fixed = TRUE)
colnames(AN.data.mean) <- cols.new


## county-level mean population
pop.county <- AN.data[,c(1:2,4)]

pop.county <- pop.county %>%
  group_by(GEOID) %>%
  summarise(pop_total = mean(pop.total))

## AN per 1,000,000 population
AN.data.mean <- AN.data.mean %>% 
  left_join(pop.county, by="GEOID") %>%
  mutate(CVD_pop = CVD_AN / pop_total *1000000)


## merge with shapefile
AN.data.mean$GEOID <- as.character(AN.data.mean$GEOID)

US.county.AN <- US.county %>%
  left_join(AN.data.mean, by="GEOID")
