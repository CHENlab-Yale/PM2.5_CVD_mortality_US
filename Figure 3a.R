##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Figure 3a                                                                         ####
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
AN.data.county <- aggregate(AN.data[,-c(1:4)], by=list(as.factor(AN.data$GEOID),as.factor(AN.data$year)), FUN = sum)
AN.data.county <- rename(AN.data.county, GEOID = Group.1, year = Group.2)

## 2001 to 2016 change AN by county
AN.data.2001 <- AN.data.county %>%
  filter(year == "2001")
AN.data.2016 <- AN.data.county %>%
  filter(year == "2016")

ANs <- colnames(AN.data.2001)[3:5]

AN.data.change <- data.frame(GEOID = AN.data.2001$GEOID)

for (i in 1:length(ANs)){
  colname <- ANs[i]
  AN.data.change[,paste0(colname,"_pct")] <- 100*(AN.data.2016[,colname] - AN.data.2001[,colname]) / AN.data.2001[,colname]
}

cols <- colnames(AN.data.change)
cols.new <- gsub(".", "_", cols,fixed = TRUE)
colnames(AN.data.change) <- cols.new

## merge with shapefile
AN.data.change$GEOID <- as.character(AN.data.change$GEOID)

US.county.AN <- US.county %>%
  left_join(AN.data.change, by="GEOID")
