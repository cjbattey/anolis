#anolis exploratory data analysis: is A. gundlachi moving downhill and/or following forest regeneration in Puerto Rico? 
library(data.table);library(ggplot2);library(raster)

gbif <- data.frame(fread("~/Downloads/gbif_Agundlachi.csv"))
gbif$yearbin <- gbif$year > 1980

ext <- extent(-67.5,-65.5,17.8,18.7)
alt <- crop(raster("data/alt_30s_bil/alt.bil"),ext)
locs <- SpatialPointsDataFrame(data.frame(gbif$decimallongitude,gbif$decimallatitude),data=data.frame(gbif$year,gbif$month))

loc.alts <- extract(alt,locs,df=T)

map <- crop(shapefile("~/Documents/worldclim/country_outlines/cntry06/cntry06.shp"),ext)

plot(alt)+points(locs)

ggplot()+coord_map()+theme_bw()+xlim(-67.5,-65.5)+ylim(17.8,18.7)+
  geom_path(data=map,aes(x=long,y=lat,group=group))+
  geom_point(data=gbif,aes(x=decimallongitude,y=decimallatitude,col=yearbin))
