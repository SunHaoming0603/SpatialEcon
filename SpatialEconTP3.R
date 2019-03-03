# Tutorial Spatial Econometrics

require(spdep)
data(columbus)
head(columbus)
require(rgdal)


# CP indicates 1 = city center, 0 = periphery
# Color Do a map of Columbus : center => red, peripheral => blue

if (requireNamespace("rgdal", quietly = TRUE)) {
  library(rgdal)
  columbus <- readOGR(system.file("shapes/columbus.shp", package="spData")[1])
  columbus.nb <- read.gal(system.file("etc/weights/columbus.gal",package = "spdep")[1])
  columbus.lw <- nb2listw(columbus.nb)
  plot(columbus)
}

columbus.lw

# Continuous variable : Moran test
moran.test(columbus$HOVAL, columbus.lw, randomisation = TRUE)
geary.test(columbus$HOVAL, columbus.lw, randomisation = TRUE)

permutes <- sample(1:nrow(columbus), nrow(columbus) ,replace = FALSE)
moran.test(columbus$HOVAL[permutes], columbus.lw, randomisation = TRUE)

