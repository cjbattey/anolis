##Anolis gundlachi elevation shift analysis
library(data.table);library(raster);library(ggplot2);library(plyr)

#read in occurrence data
gundlachi <- fread("data/gbif_Agundlachi.csv")
gundlachi$pre1980 <- gundlachi$year < 1980
gundlachi <- subset(gundlachi,is.na(gundlachi$decimallatitude)==F & is.na(gundlachi$decimallongitude)==F)
anolis <- fread("data/gbif_anolis.csv")
anolis$pre1980 <- anolis$year < 1980
anolis <- subset(anolis,is.na(anolis$decimallatitude)==F & is.na(anolis$decimallongitude)==F)

#read in altitude layer
alt <- raster("data/alt_30s_bil/alt.bil")

#convert to spatial points
anolis.pts <- SpatialPointsDataFrame(data.frame(anolis$decimallongitude,anolis$decimallatitude),
                                     data=data.frame(anolis$year,anolis$month,anolis$pre1980))
gundlachi.pts <- SpatialPointsDataFrame(data.frame(gundlachi$decimallongitude,gundlachi$decimallatitude),
                                        data=data.frame(gundlachi$year,gundlachi$month,gundlachi$pre1980))

#extract altitudes
anolis.alt <- extract(alt,anolis.pts,df=T)
anolis.alt <- cbind(anolis.alt,anolis)

gundlachi.alt <- extract(alt,gundlachi.pts,df=T)
gundlachi.alt <- cbind(gundlachi.alt,gundlachi)

#test for significant difference in altitude pre/post 1980
t.test(anolis.alt$alt~anolis.alt$pre1980) #overall, anolis reports have moved down ~ 30M
t.test(gundlachi.alt$alt~gundlachi.alt$pre1980) #gundlachi reports have move


sp.loc <- SpatialPoints(gundlachi)