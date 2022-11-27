
#General concept of Return-Based Style Analysis applied to Brazilian hedge funds

#import packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(faraway)
library(PerformanceAnalytics)

#import weekly data of market factors and hedge fund close prices (provided by Economatica)
all_factors <- readxl::read_excel("data/factors/all_factors.xlsx", 
                          skip=0, col_types = c("date", rep("numeric", 12)))
fund_BRAD <- readxl::read_excel("data/funds/Bradesco.H.Fc.Mult.Aquamarine.xlsx", 
                                col_names = c("Date", "Value"), skip=4, col_types = c("date", "numeric"))
fund_CSHG <- readxl::read_excel("data/funds/CSHG.Verde.FICFI.Mult.xlsx", 
                                col_names = c("Date", "Value"), skip=4, col_types = c("date", "numeric"))
fund_OPPO <- readxl::read_excel("data/funds/Opportunity.Allocation.FICFI .Mult.xlsx", 
                                col_names = c("Date", "Value"), skip=4, col_types = c("date", "numeric"))
fund_SNTD <- readxl::read_excel("data/funds/Santander.Fc.FI.Mas.Mult.xlsx", 
                               col_names = c("Date", "Value"), skip=4, col_types = c("date", "numeric"))

#calculate returns 
calc_return <- function(x) {
  y <- ( x / dplyr::lag(x) ) - 1
  return(y)
}

all_factors_return <- all_factors[,1]
all_factors_return[colnames(all_factors)[-1]] <- sapply(all_factors[,-1], calc_return) %>% as.tibble()

fund_BRAD["Return"] <- calc_return(fund_BRAD[,2]) 
fund_CSHG["Return"] <- calc_return(fund_CSHG[,2])
fund_OPPO["Return"] <- calc_return(fund_OPPO[,2])
fund_SNTD["Return"] <- calc_return(fund_SNTD[,2])

#selection of regressors
all_factors.r <- all_factors %>%
  
  
  vcxdvcx
  
  
  
  
  #mapply(function(x) diff(x)/x[-length(x)], CDI)

  

PerformanceAnalytics::chart.Correlation(all_factors[,-1])
only_factors <- all_factors %>%
  select(-CDI, -IBOV, -`IMA-B`, -`IMA-B5`, -`IRF-M`, -`IRF-M1`)
PerformanceAnalytics::chart.Correlation(only_factors[,-1])





