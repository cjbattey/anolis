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

coords <- subset(anolis,!is.na(lat))
#3. get % reports in forested habitats for each sp. 
gundlachi.age2 <- subset(coords,ageClass %in% c(2) & species=="Anolis gundlachi")
pts2 <- SpatialPoints(data.frame(gundlachi.age2$long,gundlachi.age2$lat),proj4string = crs(proj4.wgs)) %>% spTransform(.,crs(proj4.utm))
gundlachi.age4 <- subset(coords,ageClass %in% c(4) & species=="Anolis gundlachi")
pts4 <- SpatialPoints(data.frame(gundlachi.age4$long,gundlachi.age4$lat),proj4string = crs(proj4.wgs)) %>% spTransform(.,crs(proj4.utm))
bgpts2 <- SpatialPoints(data.frame(coords$long[coords$ageClass %in% c(2)],coords$lat[coords$ageClass %in% c(2)]),proj4string = crs(proj4.wgs)) %>% spTransform(.,crs(proj4.utm))
bgpts4 <- SpatialPoints(data.frame(coords$long[coords$ageClass %in% c(4)],coords$lat[coords$ageClass %in% c(4)]),proj4string = crs(proj4.wgs)) %>% spTransform(.,crs(proj4.utm))
par(mfrow=c(2,1),oma = c(1,1,0,0) + 0.1,mar = c(2,2,2,2) + 0.1)
plot(age2,main="A. gundlachi, 1960-1977",axes=F,legend=F)+plot(coasts,add=T)+points(bgpts2,cex=.7)+points(pts2,cex=.2,col="red")
plot(age4,main="A. gundlachi, 1990-2015",axes=F,legend=F)+plot(coasts,add=T)+points(bgpts4,cex=.7)+points(pts4,cex=.2,col="red")
par(mfrow=c(1,1))


#get %forested habitat within uncertainty radius for all reports, subset by species
#note: this is unusably slow :( rewrite to run on localities
#double note: localities much faster, but now getting some values > 1, which is a problem...
locs <- ddply(anolis,.(eventID,lat,long,uncertainty,ageClass,species),summarize,n=length(day))
pc.forest <- ddply(locs,.(species,ageClass),function(i){
  pts <- SpatialPointsDataFrame(coords=i[,c("long","lat")],
                                data=data.frame(i$uncertainty),
                                proj4string=crs(proj4.wgs)) %>% spTransform(proj4.utm)
  buf <- gBuffer(pts,width=pts@data$i.uncertainty,byid=T)
  if(i$ageClass[1] == 2){
    df <- extract(age2,buf) 
  } else if (i$ageClass[1] ==4){
    df <- extract(age4,buf)
  }   
  df <- lapply(df,function(e) mean(na.omit(e))) %>% unlist() %>% data.frame()
  df$species <- i$species[1]
  df$ageClass <- i$ageClass[1]
  colnames(df) <- "percent.forested"
  df
  })


#rasterize species distributions at 1km

gund <- subset(anolis,species=="Anolis gundlachi")
pts <- SpatialPoints(gund[,c("long","lat")],proj4string=crs(proj4.wgs)) %>% spTransform(proj4.utm)
r <- rasterize(pts,alt,fun="count")

effort <- SpatialPoints(anolis[,c("long","lat")],proj4string=crs(proj4.wgs)) %>% 
            spTransform(proj4.utm) %>% 
              rasterize(alt,fun="count")
  

#get species composition for every locality
#1. 






