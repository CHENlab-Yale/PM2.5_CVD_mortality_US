##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Sensitivity Analysis                                                              ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)
library(lubridate)
library(tsModel)
library(phtt)
library(splines)

data.list <- split(data, as.factor(data$GEOID))

for (j in 1:length(data.list)){
  data.list[[j]][,paste0("PM25_lag0",11)]<- tsModel::runMean(data.list[[j]]$PM25, c(0:11))
  data.list[[j]][,paste0("NO2_lag0",11)]<- tsModel::runMean(data.list[[j]]$NO2, c(0:11))
  data.list[[j]][,paste0("O3_lag0",11)]<- tsModel::runMean(data.list[[j]]$O3, c(0:11))
  data.list[[j]][,paste0("CVD.adj_lag0",11)]<- tsModel::runMean(data.list[[j]]$CVD.adj, c(0:11))
}

data <- bind_rows(data.list) %>%
  filter(year >= 2001)


pop.5000 <- data %>%
  filter(pop.total < 5000)
pop.5000.ID <- unique(pop.5000$GEOID)

data.5K <- data %>%
  filter((GEOID %in% pop.5000.ID)==F)

pop.200K <- data %>%
  filter(pop.total > 200000)
pop.200K.ID <- unique(pop.200K$GEOID)

data.200K <- data %>%
  filter((GEOID %in% pop.200K.ID)==F)


models <- c("main","pop5K","pop200K","12MCVD","TWFE","NO2","O3","dewT","TMPdf4","TMPdf6")
results.table <- data.frame(model = models)

#### coef
### (1) main
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.main <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
                  additive.effects = "twoways")
summary <- summary(model.main)

results.table[1,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[1,'SE'] <- summary$coefficients["PM25","Std.Err"]

### (2) pop5K
n.month <- length(unique(data.5k$year_month))
n.county <- length(unique(data.5k$GEOID))

Total.rate <- matrix(data.5k$CVD.adj, n.month, n.county)
PM25 <- matrix(data.5k$PM25_lag011, n.month, n.county)

Tmean.spline <- ns(data.5k$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.5K <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
                 additive.effects = "twoways")
summary <- summary(model.5K)

results.table[2,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[2,'SE'] <- summary$coefficients["PM25","Std.Err"]

### (3) pop200K
n.month <- length(unique(data.5k$year_month))
n.county <- length(unique(data.5k$GEOID))

Total.rate <- matrix(data.5k$CVD.adj, n.month, n.county)
PM25 <- matrix(data.5k$PM25_lag011, n.month, n.county)

Tmean.spline <- ns(data.5k$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.200K <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
                 additive.effects = "twoways")
summary <- summary(model.200K)

results.table[3,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[3,'SE'] <- summary$coefficients["PM25","Std.Err"]


### (4) 12MCVD
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj_lag011, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.12M <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
                 additive.effects = "twoways")
summary <- summary(model.12M)

results.table[4,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[4,'SE'] <- summary$coefficients["PM25","Std.Err"]

### (5) TWFE
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.TWFE <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
                  additive.effects = "twoways", factor.dim = 0)
summary <- summary(model.TWFE)

results.table[5,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[5,'SE'] <- summary$coefficients["PM25","Std.Err"]


### (6) NO2
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)
NO2 <- matrix(data$NO2_lag011, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.NO2 <- Eup(Total.rate ~ PM25 + NO2 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
                 additive.effects = "twoways")
summary <- summary(model.NO2)

results.table[6,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[6,'SE'] <- summary$coefficients["PM25","Std.Err"]


### (7) O3
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)
O3 <- matrix(data$O3_lag011, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.O3 <- Eup(Total.rate ~ PM25 + O3 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5,
                additive.effects = "twoways")
summary <- summary(model.O3)

results.table[7,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[7,'SE'] <- summary$coefficients["PM25","Std.Err"]


### (8) dewT
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)
dewT <- matrix(data$dewT, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=5)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)

model.dewT <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4 + Tmean.ns5 + dewT,
                additive.effects = "twoways")
summary <- summary(model.dewT)

results.table[8,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[8,'SE'] <- summary$coefficients["PM25","Std.Err"]


### (9) df4
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=4)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)

model.df4 <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4,
                 additive.effects = "twoways")
summary <- summary(model.df4)

results.table[9,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[9,'SE'] <- summary$coefficients["PM25","Std.Err"]



### (10) df6
n.month <- length(unique(data$year_month))
n.county <- length(unique(data$GEOID))

Total.rate <- matrix(data$CVD.adj, n.month, n.county)
PM25 <- matrix(data$PM25_lag011, n.month, n.county)

Tmean.spline <- ns(data$Tmean, df=6)
Tmean.ns1 <- matrix(Tmean.spline[,1], n.month, n.county)
Tmean.ns2 <- matrix(Tmean.spline[,2], n.month, n.county)
Tmean.ns3 <- matrix(Tmean.spline[,3], n.month, n.county)
Tmean.ns4 <- matrix(Tmean.spline[,4], n.month, n.county)
Tmean.ns5 <- matrix(Tmean.spline[,5], n.month, n.county)
Tmean.ns6 <- matrix(Tmean.spline[,6], n.month, n.county)

model.df6 <- Eup(Total.rate ~ PM25 + Tmean.ns1 + Tmean.ns2 + Tmean.ns3 + Tmean.ns4+ Tmean.ns5 + Tmean.ns6,
                 additive.effects = "twoways")
summary <- summary(model.df6)

results.table[10,'coef'] <- summary$coefficients["PM25","Estimate"]
results.table[10,'SE'] <- summary$coefficients["PM25","Std.Err"]

### format the table
results.table <- results.table %>%
  mutate(coef_low = coef - 1.96*SE,
         coef_up = coef + 1.96*SE)

results.table.format <- results.table

results.table.format[,2:5] <- 1000000 * results.table.format[,2:5]

results.table.format[,2:5] <- round(results.table.format[,2:5], 2)

results.table.format <- results.table.format %>%
  mutate(coef_CI = paste0(coef, " (", coef_low,", ",coef_up,")")) 


### Burden
source("function_AN for each month.R")

for (i in c(1,4:10)){
  burden.df <- AN.allyears(data.all = data,
                           AP = "PM25_lag011",
                           population = "pop.total",
                           coef = results.table[i,"coef"],
                           coef_low = results.table[i,"coef_low"],
                           coef_up = results.table[i,"coef_up"])
  
  results.table[i,c("AN","AN_low","AN_up")] <- colSums(burden.df)/16
}

# pop 5k
burden.df <- AN.allyears(data.all = data.5K,
                         AP = "PM25_lag011",
                         population = "pop.total",
                         coef = results.table[2,"coef"],
                         coef_low = results.table[2,"coef_low"],
                         coef_up = results.table[2,"coef_up"])

results.table[2,c("AN","AN_low","AN_up")] <- colSums(burden.df)/16

# pop 200k
burden.df <- AN.allyears(data.all = data.200K,
                         AP = "PM25_lag011",
                         population = "pop.total",
                         coef = results.table[3,"coef"],
                         coef_low = results.table[3,"coef_low"],
                         coef_up = results.table[3,"coef_up"])

results.table[3,c("AN","AN_low","AN_up")] <- colSums(burden.df)/16

### format the table
AN.mean.table.formated <- results.table[,7:9]
AN.mean.table.formated <- round(AN.mean.table.formated,0)
AN.mean.table.formated$AN_CI <- paste0(AN.mean.table.formated$AN," (",AN.mean.table.formated$AN_low,", ",AN.mean.table.formated$AN_up,")")
