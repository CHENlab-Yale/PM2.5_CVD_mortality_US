##############################################################################################################
#### Racial/ethnic disparities in PM2.5-attributable cardiovascular mortality burden in the United States ####
#### Cleaned R code for AN calculation                                                                    ####
#### Yiqun Ma, Emma Zang, Ijeoma Opara, Yuan Lu, Harlan M. Krumholz, Kai Chen                             ####
#### Yale University, New Haven, CT                                                                       ####
#### July 06, 2023                                                                                        ####
##############################################################################################################

AN.allyears <- function(data.all, AP, population, coef, coef_low, coef_up){
  monthly.AP <- data.all[,AP]
  monthly.pop <- data.all[,population]
  
  monthly.coef <- monthly.AP * coef
  monthly.coef_low <- monthly.AP * coef_low
  monthly.coef_up <- monthly.AP * coef_up
  
  monthly.burden <- monthly.coef * monthly.pop
  monthly.burden_low <- monthly.coef_low * monthly.pop
  monthly.burden_up <- monthly.coef_up * monthly.pop
  
  burden.df <- data.frame(AN = monthly.burden,
                          AN_low = monthly.burden_low,
                          AN_up = monthly.burden_up)
  
  return(burden.df)
  
}