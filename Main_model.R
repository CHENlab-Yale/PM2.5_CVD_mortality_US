##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for the main model                                                                    ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### July 06, 2023                                                                                        ####
##############################################################################################################

library(phtt)
library(splines)

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