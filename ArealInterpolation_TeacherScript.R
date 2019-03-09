# Spatial interpolation 
# teacher script
library(dplyr)
library(sf)
library(sp)
library(rgeos)

# functions written by Thibault Laurent
# https://github.com/tibo31/spatial_project/blob/master/AIM.R
source("https://raw.githubusercontent.com/tibo31/spatial_project/master/AIM.R")


# import the census data ----
load("Data/Census/Census.RData")

# illustrate target and source zones
par(mfrow = c(1,2))
plot(Census.state49$geometry, main = "Source Zones" ,sub = "49 states of the USA")
plot(Census.county49$geometry, main = "Target Zones",sub = "counties withon these 49 sates")


# Define target variable (Y) for all methodes
# (to have comparable results)

# intensive case: incomepercapE (GDP)  at source level 
Y_s.int <- Census.state49[,c("incomepercapE")] %>% st_drop_geometry()

# extensive case: carsE (number of cars)  at source level
Y_s.ext <- Census.state49[,c("carsE")] %>% st_drop_geometry()


# Methode 1: AWI ----
# areal wiegthin interpolation

County49.awi <- 
daw(nature = "intensive",
    sources = as(Census.state49,"Spatial"),  # daw works with spatial (sp) 
    targets = as(Census.county49,"Spatial"), # convert sf objects to sp objects!
    y = "incomepercapE")

(County49.awi$incomepercapEdaw - Census.county49$incomepercapE) %>% hist()


# Methode 2: Daisymetric ----


# Methode 3: Regression ----
# As first step construct the weight matrix W
W_0 <-
sapply(as.list(Census.state49$NAME), function(x) {
  Census.county49$populationE*(Census.county49$STATE==x)}) %>% t()
W <- diag(Census.state49$populationE^(-1)) %*% W_0
# control
rowSums(W) # 

# Then define X and calculate WX for each case
# (X contains the auxilliary information at target level)

# i) Intensive variable (average income: incomepercapE)
# which variables shoudl be included in the model ?
auxilliary.int <- c("unemp_rate","medianageE","ginicoefE"
                    ,"car_per_cap","area_m2")

X.int <- Census.county49[,aux_information.int] %>% st_drop_geometry()

WX.int <- W %*% as.matrix(X.int) 
WX.int <- as.data.frame(WX.int)

# Intensive variable ==> normal distribution ==> linear model
lm_income <- lm(incomepercapE ~ unemp_rate + medianageE + ginicoefE + car_per_cap + area_m2
                , data = cbind(Y_s.int,WX.int)  # use source level data to fit the model
                , weights = Census.state49$populationE
                ) # weighted least-square!

pred_income <- predict(lm_income,newdata = Census.county49,type = "response")

hist(pred_income - Census.county49$incomepercapE,"FD")

# ii) Extensive variable (number of cars: carsE)
# which variables shoudl be included in the model ?
auxilliary.ext <- c("unemp_rate","medianageE","ginicoefE"
                    ,"medianincomeE","populationE","area_m2")

X.ext <- Census.county49[,auxilliary.ext] %>% st_drop_geometry()

WX.ext <- W %*% as.matrix(X.ext) 
WX.ext <- as.data.frame(WX.ext)


# Intensive variable ==> poisson distribution ==> glm(family = "poisson)
pois_cars <- glm(carsE ~  populationE + area_m2 
                , family = poisson(link = "id")
                , data = cbind(Y_s.ext,WX.ext)  # use source level data to fit the model
                , weights = Census.state49$populationE
                )
pois_cars %>% summary()

pred_cars <- predict(pois_cars,newdata = Census.county49,type = "response")

(Census.county49$carsE - pred_cars) %>% summary()






