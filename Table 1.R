##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for Table 1                                                                           ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### Feb 15, 2024                                                                                         ####
##############################################################################################################

library(dplyr)

data.list <- split(data, as.factor(data$GEOID))

## 12-month moving average of PM2.5 concentration
for (j in 1:length(data.list)){
  data.list[[j]][,paste0("PM25_lag0",11)]<- tsModel::runMean(data.list[[j]]$PM25, c(0:11))
}

## restrict data from Jan 2001 to Dec 2016
data <- bind_rows(data.list) %>%
  filter(year >= 2001)


## describe key variables
data[,c("CVD.adj","IHD.adj","MI.adj","Stroke.adj","Hypertensive.adj","HHD.adj",
        "CVD.adj.Male","CVD.adj.Female")] <- 
  1000000*data[,c("CVD.adj","IHD.adj","MI.adj","Stroke.adj","Hypertensive.adj","HHD.adj",
                  "CVD.adj.Male","CVD.adj.Female")]


variables <- c("PM25_lag011","Tmean",
               "CVD.adj","IHD.adj","MI.adj","Stroke.adj","Hypertensive.adj","HHD.adj",
               "CVD.adj.Male","CVD.adj.Female")

table1 <- data.frame(row.names = variables)

for (i in 1:length(variables)){
  table1[variables[i],"Mean"]<- mean(data[,variables[i]])
  table1[variables[i],"SD"]<- sd(data[,variables[i]])
  table1[variables[i],"Min"]<- min(data[,variables[i]])
  table1[variables[i],"Median"]<- quantile(data[,variables[i]],probs = 0.50)
  table1[variables[i],"Max"]<- max(data[,variables[i]])
  table1[variables[i],"IQR"]<- IQR(data[,variables[i]])
}


## for race/ethnicity-specific mortality rate, restrict to counties with at least 1000 people of that race/ethnicity

GEOID.noWhite <- NA

GEOID.noBlack <- filter(data, pop.Black<= 1000)
GEOID.noBlack <- unique(GEOID.noBlack$GEOID)

GEOID.noHispanic <- filter(data, pop.Hispanic<= 1000)
GEOID.noHispanic <- unique(GEOID.noHispanic$GEOID)


GEOID.exclude <- list(GEOID.noWhite, GEOID.noBlack, GEOID.noHispanic)

data[,c("CVD.adj.White","CVD.adj.Black","CVD.adj.Hispanic")] <- 
  1000000*data[,c("CVD.adj.White","CVD.adj.Black","CVD.adj.Hispanic")]

variables <- c("CVD.adj.White","CVD.adj.Black","CVD.adj.Hispanic")

table2 <- data.frame(row.names = variables)


for (i in 1:length(variables)){
  data.i <- data %>%
    filter((GEOID %in% GEOID.exclude[[i]])==F)
  table2[variables[i],"Mean"]<- mean(data.i[,variables[i]])
  table2[variables[i],"SD"]<- sd(data.i[,variables[i]])
  table2[variables[i],"Min"]<- min(data.i[,variables[i]])
  table2[variables[i],"Median"]<- quantile(data.i[,variables[i]],probs = 0.50)
  table2[variables[i],"Max"]<- max(data.i[,variables[i]])
  table2[variables[i],"IQR"]<- IQR(data.i[,variables[i]])
}


## combine and format
table <- rbind(table1, table2)

table.formated <- round(table, 2)

table.formated[,"Mean (SD)"]<- paste0(table.formated$Mean, " (", table.formated$SD, ")")
table.formated[,"Median (IQR)"]<- paste0(table.formated$Median, " (", table.formated$IQR, ")")
