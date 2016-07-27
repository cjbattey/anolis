#landcover analyses
setwd("~/Dropbox/anolis/")
library(raster)
#1. load landcover raster (Helmer et al. 2008, JGRBiogeoscience), reclassify to combine soil/forest types as needed
roads <- shapefile("./data/roads/tl_2013_72_prisecroads/tl_2013_72_prisecroads.shp")
lc <- raster("./data/landcover/prforestage_foresttype/IITF_JGR113_puertorico_fortype_age.img")
m.all <- read.delim("./data/landcover/prforestage_foresttype/combineSoils_reclassMatrix.txt",header=F)
m.forests <- read.delim("./data/landcover/prforestage_foresttype/PRlandcover_combSoils_allForest.txt",header=F)
m.humidForests <- read.delim("./data/landcover/prforestage_foresttype/PRlandcover_combSoils_humidForest.txt",header=F)
m.dryForests <- read.delim("./data/landcover/prforestage_foresttype/PRlandcover_combSoils_dryForest.txt",header=F)
lc.all <- reclassify(lc,m.all)
lc.forests <- reclassify(lc,m.forests)
lc.humidForests <- reclassify(lc,m.humidForests)
lc.dryForests <- reclassify(lc,m.dryForests)
coasts <- shapefile("~/Documents/worldclim/ne_10m_coastline/ne_10m_coastline.shp") %>% crop(ext.pr) %>% spTransform(.,crs(lc))
#writeRaster(lc,"./data/landcover/prforestage_foresttype/PRlandcover_combSoils.tif")
#lc.all classfication key:
# 0	Bare
# 1	High Density Urban 1991
# 2	New Urban and Bare 1991-2000
# 3	Low Density Development 2000
# 4	Pasture and Agriculture
# 5	Forest Age 1-9yr
# 6	Forest Age 10-22yr
# 7	Forest Age 23-49yr
# 8	Forest Age 50-64yr
# 9 Emergent Wetland
# 10  Coastal Sand/Rock
# 11  Water

#2. reclassify by age to produce rasters for <1958, 1959-1985,1986-1998,1999-2007 ("age1"-"age4")
age1 <- reclassify(lc.all,as.matrix(data.frame(from=c(0:11),to=c(0,0,0,0,0,0,0,0,1,0,0,0))))
age2 <- reclassify(lc.all,as.matrix(data.frame(from=c(0:11),to=c(0,0,0,0,0,0,0,1,1,0,0,0)))) 
age3 <- reclassify(lc.all,as.matrix(data.frame(from=c(0:11),to=c(0,0,0,0,0,0,1,1,1,0,0,0))))
age4 <- reclassify(lc.all,as.matrix(data.frame(from=c(0:11),to=c(0,0,0,0,0,1,1,1,1,0,0,0))))

par(mfrow=c(2,2),oma = c(1,1,0,0) + 0.1,mar = c(2,2,2,2) + 0.1)
plot(age1,main="1935-1951",axes=F,legend=F)+plot(coasts,lwd=.5,add=T)
plot(age2==1,main="1952-1977",axes=F,legend=F)+plot(coasts,lwd=.5,add=T)
plot(age3==1,main="1978-1990",axes=F,legend=F)+plot(coasts,lwd=.5,add=T)
plot(age4==1,main="1991-2000",axes=F,legend=F)+plot(coasts,lwd=.5,add=T)
par(mfrow=c(1,1))

#3. get % reports in forested habitats for each sp. 
anolis.coords <- subset(anolis.full,!is.na(lat))
gundlachi.age2 <- subset(anolis,ageClass %in% c(2) & species=="Anolis gundlachi")
pts2 <- SpatialPoints(data.frame(gundlachi.age2$long,gundlachi.age2$lat),proj4string = crs(alt)) %>% spTransform(.,crs(lc))
gundlachi.age4 <- subset(anolis,ageClass %in% c(4) & species=="Anolis gundlachi")
pts4 <- SpatialPoints(data.frame(gundlachi.age4$long,gundlachi.age4$lat),proj4string = crs(alt)) %>% spTransform(.,crs(lc))
bgpts2 <- SpatialPoints(data.frame(anolis$long[anolis$ageClass %in% c(2)],anolis$lat[anolis$ageClass %in% c(2)]),proj4string = crs(alt)) %>% spTransform(.,crs(lc))
bgpts4 <- SpatialPoints(data.frame(anolis$long[anolis$ageClass %in% c(4)],anolis$lat[anolis$ageClass %in% c(4)]),proj4string = crs(alt)) %>% spTransform(.,crs(lc))
par(mfrow=c(2,1),oma = c(1,1,0,0) + 0.1,mar = c(2,2,2,2) + 0.1)
plot(age2,main="A. gundlachi, 1960-1977",axes=F,legend=F)+plot(coasts,add=T)+points(bgpts2,cex=.7)+points(pts2,cex=.2,col="red")
plot(age4,main="A. gundlachi, 1990-2015",axes=F,legend=F)+plot(coasts,add=T)+points(bgpts4,cex=.7)+points(pts4,cex=.2,col="red")
par(mfrow=c(1,1))

#crop for El Yunque NF xlim=c(-66,-65.6),ylim=c(18.2,18.5)
alt.yunque <- crop(projectRaster(from=alt1s,lc),c(825000,850000,2015000,2040000))
yunque.roads <- roads[roads@data$FULLNAME %in% c("Pr- 186","Pr- 191"),]

#pr186 and pr191 elevation profiles
pr186 <- roads[roads@data$FULLNAME %in% c("Pr- 186"),] %>% spTransform(.,crs(lc))
pr186.buffer <- buffer(pr186,width=1000)
pr186.r <- rasterize(pr186,lc) 

pr191 <- roads[roads@data$FULLNAME %in% c("Pr- 191"),] %>% spTransform(.,crs(lc))
pr191.buffer <- buffer(pr191,width=1000)
pr191.r <- rasterize(pr191,lc) 
  
pr3 <- roads[roads@data$FULLNAME %in% c("Pr- 3"),] %>% spTransform(.,crs(lc))

pts2.r <- rasterize(pts2,)

pr191.alt <- extract(alt.yunque,pr191)


#plot 186 & 191 localities
plot(crop(age2,c(825000,850000,2015000,2040000)))
  plot(rasterToContour(alt.yunque),add=T,col="grey",lwd=.5)
  plot(pr191,add=T)+plot(pr186,add=T)+
  points(pts4,col="red")
  points(pts2,col="blue")
  
pr186.alt <- extract(althires,pr186.pts) %>% unlist() %>% data.frame(alt=.,order=1:length(.))

ggplot()+geom_line(data=pr186.alt,aes(x=order,y=alt))+
  geom_point(data=subset(anolis,species=="Anolis gundlachi"),aes(x=))

plot(alt2)+plot(yunque.roads,add=T)+points(bgpts2)+points(pts2,col="red",cex=0.2)
plot(alt2)+plot(yunque.roads,add=T)+points(bgpts4)+points(pts4,col="red",cex=0.2)




















