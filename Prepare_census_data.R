# prepare the class data
library(tidycensus)
library(dplyr)
library(stringr)

# set key
key <- "df893564ae6ef336cd5b451d81b4fa233342acdb"
census_api_key(key, overwrite = FALSE, install = FALSE)

# variables to extract from the sample
vars_to_load <- c(
  medianage = "B01002_001",
  population = "B01003_001",
  medianincome = "B19326_001",
  unemployed = "B23025_005",
  laborforce = "B23025_002",
  incomepercap = "B19301_001"
)

# Exclude all states that are not mainland US
states.exclude <- c("Puerto Rico","Hawaii","Alaska")

# extract the data for diffrent aggregation levels
Census.county <- get_acs(
  geography = "county",
  variables = vars_to_load,
  output = "wide",
  survey = "acs5",
  geometry = TRUE
) 

Census.county49 <- Census.county %>% 
  mutate(STATE = str_extract(NAME, '\\b[^,]+$')) %>% # macth from the first word that is not followed by comma
  filter(!STATE%in%states.exclude) 
  

Census.state <- get_acs(
  geography = "state",
  variables = vars_to_load,
  output = "wide",
  survey = "acs5",
  geometry = TRUE
) 

Census.state49 <- Census.state %>% 
  filter(!NAME%in%states.exclude)

par(mfrow = c(1,2))
plot(Census.county49$geometry)
plot(Census.state49$geometry)

save(Census.state49, Census.county49, file = "Data/Census/Census.RData")
#load("Data/Census/Census.RData")
