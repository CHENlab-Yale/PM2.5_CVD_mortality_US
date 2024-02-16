##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Figure 2c                                                                         ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)

## calculate monthly county-level attributable burden
AN.data <- data[,c("GEOID","year_month","year","pop.total")]

source("function_AN for each month.R")

results.table.CVD <- results.table %>%
  filter(outcome %in% c("CVD.adj.White","CVD.adj.Black","CVD.adj.Hispanic"))

pops <- colnames(data)[c(12:14)]

for (i in 1:nrow(results.table.CVD)){
  outcome <- results.table.CVD$outcome[i]
  pop <- pops[i]
  coef <- results.table.CVD$coef[i]
  coef_low <- results.table.CVD$coef_low[i]
  coef_up <- results.table.CVD$coef_up[i]
  
  burden.df <- AN.allyears(data.all = data,
                           AP = "PM25_lag011",
                           population = pop,
                           coef = coef,
                           coef_low = coef_low,
                           coef_up = coef_up)
  
  AN.data[,paste0(outcome,".AN")] <- burden.df[,"AN"]
  AN.data[,paste0(outcome,".AN_low")] <- burden.df[,"AN_low"]
  AN.data[,paste0(outcome,".AN_up")] <- burden.df[,"AN_up"]
}

## annual attributable CVD deaths for each group
AN.year <- AN.data[,-c(1,2,4)]
AN.year <- aggregate(AN.year[,-1], by=list(as.factor(AN.data$year)), FUN = sum)
AN.year <- rename(AN.year, year = Group.1)

AN.means <- colSums(AN.year[-1])/16
