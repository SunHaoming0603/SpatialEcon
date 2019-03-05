library(dplyr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(lwgeom)

source("https://raw.githubusercontent.com/tibo31/spatial_project/master/AIM.R")

load_variables(2016, dataset, cache = FALSE)
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

US_counties49 <- popacs5 %>%
  filter(!grepl(NAME,pattern = paste))

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
  survey = "acs5",
  geometry = TRUE
)

vars_exps <- vars_exps[, !endsWith(colnames(vars_exps), "M")]

str(vars_exps)
vars_exps$geometry

get_acs(example())
example(topic = get_acs,run.dontrun = TRUE)
#### plot example

popacs6 <- get_acs(
  geography = "state",
  variables = c(pop_estimate = "B01003_001"),
  output = "wide",
  geometry = TRUE
)

states.exclude <- c("Puerto Rico","Hawaii","Alaska")

US_states49 <- popacs6 %>% 
  filter(!NAME%in%states.exclude)

plot(US_states49)

popacs6 <- popacs6 %>%
  select(NAME, geometry)
###
plot(popacs5[, -c(1, 2, 3)])
### plot also for state
plot(US_states49[, -c(1, 2, 3)])


## it did't work good , better visualisation with other
## plot function   ( ans removind the spatial outlier)

## for comutation , we can also focus on California as state for example
## and the ward inside ( county) as agregation
## and tract


### juste use help of "get_acs function"
library(tidycensus)
library(tidyverse)
library(viridis)

tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)

ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")


vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")

vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")
