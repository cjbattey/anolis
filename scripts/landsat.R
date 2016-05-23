#landsat data processing
library(RStoolbox);library(raster)
setwd("data/landsat/LM10040471972291AAA05/")
files <- list.files()
files <- files[grep(".TIF",files)]
sat <- stack(files)
sat <- crop(sat,ext)
sat <- projectRaster(sat,alt)
sat <- crop(sat,c(-66.4,-65.5,17.8,18.6))
sat <- reclassify(sat,c(0,0,NA))
unC <- unsuperClass(sat)
