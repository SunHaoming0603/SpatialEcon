# Spatial interpolation 
# teacher script

library(dplyr)
library(sf)
# functions written by Thibault Laurent
source("https://raw.githubusercontent.com/tibo31/spatial_project/master/AIM.R")


# import the data
load("Data/Census/Census.RData")
str(Census.county49)


## area wie