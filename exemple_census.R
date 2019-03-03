library(dplyr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(lwgeom)


### Before all , getting key

key <- "df893564ae6ef336cd5b451d81b4fa233342acdb"
census_api_key(key, overwrite = FALSE, install = FALSE)

## an example
## to get from census , we can take state or county or more smaller
## census from 2013-2017
popacs5 <- get_acs(
  geography = "county",
  variables = c(pop_estimate = "B01003_001"),
  output = "wide",
  geometry = TRUE
)
popacs5 <- popacs5 %>%
  select(NAME, geometry)

### st_geod_area to obtain area from the geometry
superficie <- st_geod_area(popacs5) / 1000000
superficie2 <- as.numeric(superficie)
popacs5$superficie <- superficie2


##################################### VARS EXPS SOCIO DEMO ##################################################################

### kind of variable we can find

vars_exps <- get_acs(
  geography = "county",
  variables = c(
    medianage = "B01002_001",
    population = "B01003_001",
    medianincome = "B19326_001",
    unemployed = "B23025_005",
    laborforce = "B23025_002",
    incomepercap = "B19301_001"
  ),
  output = "wide",
  survey = "acs5"
)

vars_exps <- vars_exps[, !endsWith(colnames(vars_exps), "M")]



#### plot example

popacs6 <- get_acs(
  geography = "state",
  variables = c(pop_estimate = "B01003_001"),
  output = "wide",
  geometry = TRUE
)

popacs6 <- popacs6 %>%
  select(NAME, geometry)
###
plot(popacs5[, -c(1, 2, 3)])
### plot also for state
plot(popacs6[, -c(1, 2, 3)])


## it did't work good , better visualisation with other
## plot function   ( ans removind the spatial outlier)

## for comutation , we can also focus on California as state for example
## and the ward inside ( county) as agregation
## and tract


### juste use help of "get_acs function"
