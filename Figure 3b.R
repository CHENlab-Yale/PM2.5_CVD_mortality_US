##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Figure 3b                                                                         ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(MetBrewer)

## AN by year
## calculate monthly county-level attributable burden
AN.data <- data[,c("GEOID","year_month","year","pop.total")]

source("function_AN for each month.R")

results.table.CVD <- results.table %>%
  filter(outcome %in% c("CVD.adj","HHD.adj","Hypertensive.adj","IHD.adj","MI.adj","Stroke.adj"))

for (i in 1:nrow(results.table.CVD)){
  outcome <- results.table.CVD$outcome[i]
  coef <- results.table.CVD$coef[i]
  coef_low <- results.table.CVD$coef_low[i]
  coef_up <- results.table.CVD$coef_up[i]
  
  
  burden.df <- AN.allyears(data.all = data,
                           AP = "PM25_lag011",
                           population = "pop.total",
                           coef = coef,
                           coef_low = coef_low,
                           coef_up = coef_up)
  
  AN.data[,paste0(outcome,".AN")] <- burden.df[,"AN"]
  AN.data[,paste0(outcome,".AN_low")] <- burden.df[,"AN_low"]
  AN.data[,paste0(outcome,".AN_up")] <- burden.df[,"AN_up"]
  
}

## annual attributable deaths for each cause
AN.year <- AN.data[,-c(1,2,4)]
AN.year <- aggregate(AN.year[,-1], by=list(as.factor(AN.data$year)), FUN = sum)
AN.year <- rename(AN.year, year = Group.1)

## format the data for plot
AN.year.specific <- AN.year %>% 
  mutate(OtherIHD.AN = IHD.AN - MI.AN,
         OtherHD.AN = Hypertensive.AN - HHD.AN,
         OtherCVD.AN = CVD.AN - IHD.AN - Stroke.AN - Hypertensive.AN) %>%
  dplyr::select(year, MI.AN, OtherIHD.AN, Stroke.AN,
                HHD.AN, OtherHD.AN,OtherCVD.AN)

AN.year.specific.long <- data.frame(year = rep(AN.year$year, 6),
                                    AN = c(AN.year.specific$MI.AN, AN.year.specific$OtherIHD.AN,
                                           AN.year.specific$Stroke.AN, 
                                           AN.year.specific$HHD.AN, AN.year.specific$OtherHD.AN,
                                           AN.year.specific$OtherCVD.AN),
                                    Cause = rep(c("MI","Other IHD","Stroke","HHD","Other HD","Other CVD"), each = 16))

AN.year.specific.long$Cause_spc <- factor(AN.year.specific.long$Cause, 
                                          levels = c("MI","Other IHD","Stroke","HHD","Other HD","Other CVD"))


## make the plot
theme_set(theme_cowplot())
limits <- aes(ymin = AN_low, ymax = AN_up)

plot.AN.year <- ggplot()+
  geom_bar(data=AN.year.specific.long, aes(x=year, y=AN, fill=Cause_spc), position="stack", stat="identity")+
  geom_point(data=AN.year.CVD, aes(x=year, y=AN), size=0.4, color="black")+
  geom_line(data=AN.year.CVD, aes(x=year, y=AN, group=1), size=0.1,color="black")+
  geom_errorbar(data=AN.year.CVD, aes(x=year, ymin = AN_low, ymax = AN_up), width=0.2, size=0.1)+
  theme(legend.title=element_blank())+
  scale_fill_met_d("Archambault", direction = -1) +
  ylab("annual attributable deaths")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("")


