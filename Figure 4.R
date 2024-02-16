##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Figure 4                                                                          ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(MetBrewer)

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




## prepare data for plot
pop.county <- data[,c(1:2,10:12)]
pop.county <- pop.county %>%
  group_by(GEOID, year)%>%
  slice(1) %>%
  group_by(year) %>%
  summarise(pop.White = sum(pop.White, na.rm = T),
            pop.Black = sum(pop.Black, na.rm = T),
            pop.Hispanic = sum(pop.Hispanic, na.rm = T))%>%
  mutate(year = as.factor(year))

AN.year.pct <- AN.year %>%
  left_join(pop.county, by="year") %>%
  mutate(White.AN.pct = 1000000 * CVD.adj.White.AN / pop.White,
         White.AN.pct_low = 1000000 * CVD.adj.White.AN_low / pop.White,
         White.AN.pct_up = 1000000 * CVD.adj.White.AN_up / pop.White,
         Black.AN.pct = 1000000 * CVD.adj.Black.AN / pop.Black,
         Black.AN.pct_low = 1000000 * CVD.adj.Black.AN_low / pop.Black,
         Black.AN.pct_up = 1000000 * CVD.adj.Black.AN_up / pop.Black,
         Hispanic.AN.pct = 1000000 * CVD.adj.Hispanic.AN / pop.Hispanic,
         Hispanic.AN.pct_low = 1000000 * CVD.adj.Hispanic.AN_low / pop.Hispanic,
         Hispanic.AN.pct_up = 1000000 * CVD.adj.Hispanic.AN_up / pop.Hispanic)


subgroup.names <- c("White","Black","Hispanic")

subgroups <- c("non-Hispanic White","non-Hispanic Black","Hispanic")

AN.year.pct.long <- data.frame(year = rep(AN.year.pct$year, 3),
                               AN.pct = c(as.matrix(AN.year.pct[,paste0(subgroup.names,".AN.pct")])),
                               AN.pct_low = c(as.matrix(AN.year.pct[,paste0(subgroup.names,".AN.pct_low")])),
                               AN.pct_up = c(as.matrix(AN.year.pct[,paste0(subgroup.names,".AN.pct_up")])),
                               Subgroup = rep(subgroups, each = 16))

AN.year.pct.long$Subgroup <- factor(AN.year.pct.long$Subgroup, levels = subgroups)


### CVD AN change from 2001 to 2016 by subgroup
AN.year.pct.long.2001 <- filter(AN.year.pct.long, year=="2001")
AN.year.pct.long.2016 <- filter(AN.year.pct.long, year=="2016")

AN.year.pct.long.change <- data.frame(Subgroup = AN.year.pct.long.2001$Subgroup,
                                      AN = AN.year.pct.long.2016$AN.pct - AN.year.pct.long.2001$AN.pct,
                                      AN_pct = 100*(AN.year.pct.long.2016$AN.pct - AN.year.pct.long.2001$AN.pct)/AN.year.pct.long.2001$AN.pct)


AN.pct.compare <- data.frame(race = c("non-Hispanic Black", "Hispanic"),
                             value_2001 = c(AN.year.pct.long.2001[2,2]-AN.year.pct.long.2001[1,2],
                                            AN.year.pct.long.2001[3,2]-AN.year.pct.long.2001[1,2]),
                             value_2016 = c(AN.year.pct.long.2016[2,2]-AN.year.pct.long.2016[1,2],
                                            AN.year.pct.long.2016[3,2]-AN.year.pct.long.2016[1,2]),
                             ratio_2001 = c(AN.year.pct.long.2001[2,2]/AN.year.pct.long.2001[1,2],
                                            AN.year.pct.long.2001[3,2]/AN.year.pct.long.2001[1,2]),
                             ratio_2016 = c(AN.year.pct.long.2016[2,2]/AN.year.pct.long.2016[1,2],
                                            AN.year.pct.long.2016[3,2]/AN.year.pct.long.2016[1,2]))

AN.pct.compare$race <- factor(AN.pct.compare$race, levels = c("non-Hispanic Black", "Hispanic"))


## make the plot
## AN per pop by year
theme_set(theme_cowplot())
limits <- aes(ymin = AN.pct_low, ymax = AN.pct_up)

plot.year.race <- ggplot(data=AN.year.pct.long, 
                         aes(x=year, y=AN.pct, group=Subgroup, color=Subgroup, fill=Subgroup))+
  geom_point(size=1)+
  geom_ribbon(limits, alpha=0.2,colour = NA)+
  geom_line(size=1)+
  theme(legend.title=element_blank())+
  scale_fill_met_d("Kandinsky", direction = 1) +
  scale_color_met_d("Kandinsky", direction = 1) +
  theme(legend.position = c(0.7, 0.9))+
  ylab("attributable deaths per 1,000,000 people")+
  xlab(" ")+
  ggtitle("a")


plot.disparity.absolute <- ggplot(AN.pct.compare) +
  geom_segment(aes(x=race, xend=race, y=value_2001, yend=value_2016), color="grey") +
  geom_point(aes(x=race, y=value_2001), color="#A93226", size=3) +
  geom_point(aes(x=race, y=value_2016), color="#2E86C1", size=3) +
  #coord_flip()+
  theme(
    legend.position = "none") +
  xlab("") +
  ylab("absolute disparity using the non-Hispanic White as reference")+
  ggtitle("b")


plot.disparity.relative <- ggplot(AN.pct.compare) +
  geom_segment(aes(x=race, xend=race, y=ratio_2001, yend=ratio_2016), color="grey") +
  geom_point(aes(x=race, y=ratio_2001), color="#A93226", size=3, shape=15) +
  geom_point(aes(x=race, y=ratio_2016), color="#2E86C1", size=3, shape=15) +
  #coord_flip()+
  theme(
    legend.position = "none") +
  xlab("") +
  ylab("relative disparity using the non-Hispanic White as reference")+
  ggtitle("c")


plot.combine <- plot_grid(plot.year.race, plot.disparity.absolute,plot.disparity.relative,
                          ncol=3,
                          rel_widths = c(1.25,0.5,0.5))



