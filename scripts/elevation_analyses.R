##Anolis gundlachi elevation shift analysis w/prelim effort correction a la Rohwer's "abundance index"
library(data.table);library(raster);library(ggplot2);library(plyr)
setwd("/R/anolis/")
ext <- extent(-67.5,-65.5,17.8,18.7)

##### loading and cleaning data #####
anolis <- read.delim("data/locs/gbif_PRanolis/occurrence.txt")
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


######### descriptive stats and summary plots #########
#check number of pre/post 1980 reports by species, filter for species w n reports per era
sp.reps <- ddply(anolis,.(species,timebin),summarize,reports=length(species))
common.species <- sp.reps$species[sp.reps$reports > 200]
anolis <- subset(anolis,species %in% common.species)
anolis$species <- factor(anolis$species)

#distribution of collections across museums
museums <- ddply(anolis,.(institutionCode,timebin,hascoords),summarize,Reports=length(species))
ggplot(data=museums,aes(x=institutionCode,y=Reports,fill=timebin))+theme(axis.text.x = element_text(angle = 90, hjust =1,vjust=.5))+facet_grid(.~hascoords,scales="free_x",space="free_x")+
  geom_bar(stat="identity")+ylab("Specimens")+ggtitle("Specimens  per Institution")

#distribution of collections over time. 1972 was a bad year to be a Puerto Rican anole. 
years <- ddply(subset(anolis,year>1960),.(year,species),summarize,Reports=length(alt))
ggplot(data=years,aes(x=year,y=Reports,fill=species))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_bar(stat="identity")+ylab("Specimens")

########## statistics #########
#just gundlachi: 120M lower
anolis <- subset(anolis,is.na(alt)==F)
ddply(anolis,.(species),function (i){
  t <- t.test(i$alt~i$timebin)
  p <- t$p.value
  means <- round(t$estimate,digits=2)
  diff <- round(t$conf.int,digits=2)
  sum <- c(pre=means[1],post=means[2],diff=means[1]-means[2],ci=paste(round(diff[1],2),"-",round(diff[2],2)),p=p)
  names(sum) <- c("pre","post","difference","CI","p")
  sum
})

#Controlling for effort: does increased survey effort at low elevations explain the observed shift in A. gundlachi? 
anolis$altbin <- cut(anolis$alt,seq(0,max(anolis$alt),250))
altsummary <- ddply(anolis,.(timebin,altbin),summarize,n=length(species))
ggplot(data=altsummary,aes(x=timebin,y=n,fill=altbin))+geom_bar(stat="identity")

########## Elevation Change Plots ##########
#boxplot facets
ggplot(data=anolis,aes(x=timebin,y=alt))+facet_grid(.~species)+theme_bw()+xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Median Report Elevation, Pre vs. Post 1980")+
  geom_boxplot(notch=T,outlier.colour = NA)+
  geom_point(size=.5,alpha=0.1)

#regression facets
ggplot(data=subset(anolis,year>1960),aes(x=year,y=alt))+facet_grid(.~species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Change in Report Elevation, 1960-2016")+
  geom_point(size=1,alpha=0.3)+
  geom_smooth(method="lm")

#regressions on a single plot
ggplot(data=subset(anolis,year>1960),aes(x=year,y=alt,col=species))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Change in Report Elevation, 1960-2016")+
  geom_point(size=1,alpha=0.3)+
  geom_smooth(method="lm")


######### GIS/maps ######### (NOTE this is broken as of 5/17)
#map of pre/post 1980 A. gundlachi localities
plot(alt)+
  points(anolis.pts,col="blue")

#rasterize point localities (warning: rasterize requires ~8min each to run at 30" resolution)
anolis.r <- rasterize(SpatialPoints(anolis.pts),alt,fun="count")
anolis.pre <- subset(anolis,anolis$timebin==T)
anolis.pre.r <- rasterize(data.frame(anolis.pre$decimallongitude,anolis.pre$decimallatitude),alt,fun="count")
anolis.post <- subset(anolis,anolis$timebin==F)
anolis.post.r <- rasterize(data.frame(anolis.post$decimallongitude,anolis.post$decimallatitude),alt,fun="count")

gundlachi.r <- rasterize(SpatialPoints(gundlachi.pts),alt,fun="count")
gundlachi.pre <- subset(gundlachi,gundlachi$timebin==T)
gundlachi.pre.r <- rasterize(data.frame(gundlachi.pre$decimallongitude,gundlachi.pre$decimallatitude),alt,fun="count")
gundlachi.post <- subset(gundlachi,gundlachi$timebin==F)
gundlachi.post.r <- rasterize(data.frame(gundlachi.post$decimallongitude,gundlachi.post$decimallatitude),alt,fun="count")

#calculate effort-corrected gundlachi abundance pre/post 1980
gundlachi.pre.freq <- gundlachi.pre.r/anolis.pre.r
gundlachi.post.freq <- gundlachi.post.r/anolis.post.r

par(mfrow=c(2,1))
plot(gundlachi.pre.freq)
plot(gundlachi.post.freq)
par(mfrow=c(1,1))




