# Spatial interpolation 
# teacher script
library(dplyr)
library(sf)
# functions written by Thibault Laurent
source("https://raw.githubusercontent.com/tibo31/spatial_project/master/AIM.R")


# import the census data
load("Data/Census/Census.RData")
summary(Census.state49)


# Estimate income on county level


# AWI ----
# area wiegthin interpolation



# Daisymetric ----



# Regression ----
# with auxilliary information
Census.state49 <- Census.state49 %>%
  mutate(unemp_rate = unemployedE/populationE )
  
Census.county49 <- Census.county49 %>%
  mutate(unemp_rate = unemployedE/populationE )

# i) Intensive variable (average income: incomepercapE)
# ==> normal distribution
lm_income <- lm(incomepercapE ~ unemp_rate + medianageE
                , data = Census.state49  # use source level data to fit the model
                , weights = populationE) # weighted least-square!

predict(lm_income,newdata = Census.county49,type = "response")

# ii) Extensive variable (bartenders)
# ==> poisson
lm_income <- lm(incomepercapE ~ unemp_rate + medianageE
                , data = Census.state49  # use source level data to fit the model
                , weights = populationE) # weighted least-square!

predict(lm_income,newdata = Census.county49 %>% select(-populationE),type = "response")








