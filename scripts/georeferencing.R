#anolis georeferencing
setwd("/R/anolis/")
library(data.table);library(raster);library(ggplot2);library(plyr);library(foreach)
ext <- extent(-67.5,-65.5,17.8,18.7)

##### loading and cleaning data #####
anolis <- data.frame(fread("~/Dropbox/anolis/data/locs/gbif_PRanolis/occurrence_xl.txt"))
anolis <- anolis[names(anolis) %in% c("gbifID","institutionCode","basisOfRecord","catalogNumber","recordedBy",
              "eventDate","verbatimEventDate","year","month","day","countryCode","stateProvince","county","municipality","locality",
              "verbatimLocality","verbatimElevation","locationRemarks","decimalLatitude","decimalLongitude",
              "coordinateUncertaintyInMeters","georeferenceProtocol","georeferenceSources",
              "georeferenceRemarks","species","infraspecificEpithet","elevation")]

anolis <- subset(anolis,anolis$year != "" 
                 & basisOfRecord=="PRESERVED_SPECIMEN"
                 & species != "") 

anolis$timebin <- factor(anolis$year < 1980)
levels(anolis$timebin) <- c("1980-2015","1955-1980")
anolis$timebin <- factor(anolis$timebin,levels=c("1955-1980","1980-2015"))
anolis$hascoords <- factor(!is.na(anolis$decimalLatitude) & !is.na(anolis$decimalLongitude))
levels(anolis$hascoords) <- c("locality","coordinates")
nocoords <- subset(anolis,hascoords=="locality")
coords <- subset(anolis,hascoords=="coordinates")

#read in altitude layer
alt <- crop(raster("data/alt_30s_bil/alt.bil"),ext)

#convert to spatial points
anolis.pts <- SpatialPointsDataFrame(data.frame(coords$decimalLongitude,coords$decimalLatitude),
                                     data=data.frame(coords$year,coords$month,coords$timebin,coords$species))

#extract altitudes & recombine with full dataset
anolis.alt <- extract(alt,anolis.pts) 
anolis$alt[anolis$hascoords=="coordinates"] <- anolis.alt


