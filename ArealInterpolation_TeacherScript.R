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
Census.county49 <- Census.county49 %>%
  mutate(unemp_rate = unemployedE/populationE,
         car_per_cap = carsE/populationE)

Census.state49 <- Census.state49 %>%
  mutate(unemp_rate = unemployedE/populationE,
         car_per_cap = carsE/populationE)



# create the matrix WX
Y_s <- Census.state49[,c("incomepercapE")] %>% st_drop_geometry()
X <- Census.county49[,c("unemp_rate","medianageE","ginicoefE","car_per_cap")] %>% st_drop_geometry()

W_0 <-
sapply(as.list(Census.state49$NAME), function(x) {
  Census.county49$populationE*(Census.county49$STATE==x)}) %>% t()
W <- diag(Census.state49$populationE^(-1)) %*% W_0
# contorl
rowSums(W) # as 

WX <- W %*% as.matrix(X) 
WX <- as.data.frame(WX)


# i) Intensive variable (average income: incomepercapE)
# ==> normal distribution
lm_income <- lm(incomepercapE ~ unemp_rate + medianageE + ginicoefE + car_per_cap
                , data = cbind(Y_s,WX)  # use source level data to fit the model
                , weights = Census.state49$populationE
                ) # weighted least-square!

pred_income <- 
predict(lm_income,newdata = Census.county49,type = "response")

hist(pred_income - Census.county49$incomepercapE,"FD")
t.test(pred_income - Census.county49$incomepercapE)

# ii) Extensive variable (bartenders)
# ==> poisson
pois_cars <- lm(incomepercapE ~ unemp_rate + medianageE
                , data = Census.state49  # use source level data to fit the model
                , weights = populationE) # weighted least-square!

predict(lm_income,newdata = Census.county49,type = "response") - Census.county49$incomepercapE








