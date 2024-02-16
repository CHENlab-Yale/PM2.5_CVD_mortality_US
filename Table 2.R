##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Table 2                                                                           ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(tsModel)
library(phtt)
library(splines)

## select outcome variables
outcomes <- colnames(data)[16:41]

## 12-month moving average of PM2.5 concentration
data.list <- split(data, as.factor(data$GEOID))
for (j in 1:length(data.list)){
  data.list[[j]][,paste0("PM25_lag0",11)]<- tsModel::runMean(data.list[[j]]$PM25, c(0:11))
}

## restrict data from Jan 2001 to Dec 2016
data <- bind_rows(data.list) %>%
  filter(year >= 2001)

## estimate ERF
results.table <- data.frame(outcome = outcomes)

for (i in 1:length(outcomes)){
  print(i)
  data$deathrate <- data[,outcomes[i]]
  
  n.month <- length(unique(data$year_month))
  n.county <- length(unique(data$GEOID))
  
  Total.rate <- matrix(data$deathrate, n.month, n.county)
  PM25 <- matrix(data$PM25_lag011, n.month, n.county)
  
  Tmean.spline <- ns(data$Tmean, df=5)
  Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
  Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
  Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
  Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
  Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)
  
  model <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
               additive.effects = "twoways")
  
  summary <- summary(model)
  
  results.table[i,'coef'] <- summary$coefficients["PM25","Estimate"]
  results.table[i,'SE'] <- summary$coefficients["PM25","Std.Err"]
  
}

results.table <- results.table %>%
  mutate(coef_low = coef - 1.96*SE,
         coef_up = coef + 1.96*SE)

## format table
results.table.format <- results.table
results.table.format[,2:5] <- 1000000 * results.table.format[,2:5]
results.table.format[,2:5] <- round(results.table.format[,2:5], 2)

results.table.format <- results.table.format %>%
  mutate(coef_CI = paste0(coef, " (", coef_low,", ",coef_up,")")) 
