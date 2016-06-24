##Anolis elevation shift analysis
setwd("~/Dropbox/anolis/")
library(data.table);library(raster);library(ggplot2);library(plyr);library(foreach);library(stringr);library(magrittr)
ext <- extent(-67.5,-65.5,17.8,18.7)

##### loading and cleaning data #####
anolis <- data.frame(fread("~/Dropbox/anolis/data/locs/gbif_PRanolis/occurrence_xl.txt")) # read in data and trim to useful fields
anolis <- anolis[names(anolis) %in% c("gbifID","institutionCode","basisOfRecord","catalogNumber","recordedBy",
                                      "eventDate","verbatimEventDate","year","month","day","countryCode","stateProvince","county","municipality","locality",
                                      "verbatimLocality","verbatimElevation","locationRemarks","decimalLatitude","decimalLongitude",
                                      "coordinateUncertaintyInMeters","georeferenceProtocol","georeferenceSources",
                                      "georeferenceRemarks","species","infraspecificEpithet","elevation")]
anolis <- subset(anolis,basisOfRecord=="PRESERVED_SPECIMEN" & species != "") 

##load, merge, and re-add FMNH data to include private locality info
fmnh.mus <- read.csv("~/Dropbox/anolis/data/locs/fmnh_anolis.csv")
names(fmnh.mus) <- c("catalogNumber","collectionNumber","Taxon","verbatimLocality","Details","verbatimEventDate","Details2",
                     "recordedBy","IDnotes","specimen.loc","habitatNotes","sex")
fmnh.gbif <- subset(anolis,institutionCode == "FMNH") %>% arrange(.,catalogNumber)
anolis <- subset(anolis,institutionCode != "FMNH")
fmnh.merge <- merge(fmnh.gbif,fmnh.mus,by="catalogNumber")
fmnh.gbif$verbatimLocality <- fmnh.merge$verbatimLocality.y
fmnh.gbif$verbatimEventDate <- fmnh.merge$verbatimEventDate.y
anolis <- rbind(anolis,fmnh.gbif)

##Same for AMNH data
anolis <- subset(anolis,institutionCode != "AMNH")

###### dealing with missing and ambiguous dates
baddates <- subset(anolis,is.na(year))                                          # split out reports missing a year
anolis <- subset(anolis,!is.na(year))

baddates$verbatimEventDate[grep("no",baddates$verbatimEventDate)] <- NA         # if no date was recorded, standardize to NA
baddates$verbatimEventDate[which(baddates$verbatimEventDate == "")] <- NA 

yrange <- as.character(c(1800:2016))                                          
for(i in 1:nrow(baddates)){                                                     # for each row
  e <- baddates[i,]
  y <- str_locate(e$verbatimEventDate,yrange)                                 
  if(!all(is.na(y))){                                                           # if a year-range 4-digit numeral is in the verbatim date, use it. 
    start <- na.omit(y[,1])
    stop <- na.omit(y[,2])
    baddates$year[i] <- substr(e$verbatimEventDate,start,stop) %>% as.numeric()
  } else if(length(unlist(strsplit(e$verbatimEventDate,"/|-"))) == 3) {         # if the date splits in 3 on / or -, take the last element and add 19/20 as appropriate
    yrend <- unlist(strsplit(e$verbatimEventDate,c("/|-")))[3]
    if(as.numeric(yrend) > 16){ 
      yr <- as.numeric(paste("19",yrend,sep=""))
      baddates$year[i] <- yr
    } else {
      yr <- as.numeric(paste("20",yrend,sep=""))
      baddates$year[i] <- yr
    }
  }
}
baddates <- subset(baddates,!is.na(year))
#rbd <- subset(baddates,is.na(year))                                             #check remaining missing years
#ddply(rbd,.(institutionCode),summarize,length(day))                             #summarize by institution
#View(subset(rbd,institutionCode=="MVZ"))                                        #take a look

anolis <- rbind(anolis,baddates)                                                 #recombine datasets


#adding binary columns for easy binning
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
common.species <- ddply(sp.reps,.(species),summarize,reports=min(reports))
common.species <- common.species$species[common.species$reports >= 200]
anolis <- subset(anolis,species %in% common.species)
anolis$species <- factor(anolis$species)

par(mfrow=c(3,2),
    oma = c(1,1,0,0) + 0.1,
    mar = c(2,2,2,2) + 0.1)
for(i in levels(factor(anolis$species))){
  plot(alt,legend=F)+points(anolis.pts[anolis.pts@data$coords.species == i,],cex=.8)+title(i,line=0.2)
}
par(mfrow=c(1,1))

#distribution of collections across museums
museums <- ddply(anolis,.(institutionCode,timebin,hascoords),summarize,Reports=length(species))
ggplot(data=museums,aes(x=substr(institutionCode,1,4),y=Reports,fill=timebin))+theme(axis.text.x = element_text(angle = 90, hjust =1,vjust=.5,size=6))+facet_grid(.~hascoords,scales="free_x",space="free_x")+xlab("")+
  geom_bar(stat="identity")+ylab("Specimens")+ggtitle("Specimens  per Institution")

#across years
years <- ddply(subset(anolis,year>1960),.(year,species),summarize,Reports=length(alt))
ggplot(data=years,aes(x=year,y=Reports,fill=species))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_bar(stat="identity")+ylab("Specimens")+xlab("")

#t-tests on pre vs. post 1980 elevations
anolis <- subset(anolis,is.na(alt)==F)
ddply(anolis,.(species),function (i){
  t <- t.test(i$alt~i$timebin)
  p <- round(t$p.value,5)
  means <- round(t$estimate,digits=2)
  diff <- round(t$conf.int,digits=2)
  sum <- c(pre=means[1],post=means[2],diff=means[1]-means[2],ci=paste(round(diff[1],2),"-",round(diff[2],2)),p=p)
  names(sum) <- c("pre","post","difference","CI","p")
  sum
})

#elevation boxplots
ggplot(data=anolis,aes(x=timebin,y=alt))+facet_grid(.~species)+theme_bw()+xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x=element_text(size=6))+
  ggtitle("Median Report Elevation, Pre vs. Post 1980")+
  geom_boxplot(notch=F,outlier.colour = NA)+
  geom_point(size=.5,alpha=0.1)

#elevation regressions
ggplot(data=subset(anolis,year>1960),aes(x=year,y=alt))+facet_grid(.~species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x=element_text(size=6))+
  scale_x_continuous(breaks=c(1960,2000))+
  geom_point(size=1,alpha=0.3)+
  geom_smooth(method="lm")

###analyzing effort by elevation bins
anolis$altbin <- cut(anolis$alt,seq(0,max(anolis$alt),250),labels=c("0-250","250-500","500-750","750-1000"))
anolis.altbin <- subset(anolis,is.na(altbin)==F)
altsummary <- ddply(anolis.altbin,.(timebin,altbin,species),summarize,n=length(species))

#all Anolis
ggplot(data=altsummary,aes(x=timebin,y=n,fill=altbin))+geom_bar(stat="identity")+ylab("Specimens")+
  ggtitle("All Anolis")+theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")

#by species
ggplot(data=altsummary,aes(x=timebin,y=n,fill=altbin))+geom_bar(stat="identity")+ggtitle("Per Species")+
  facet_grid(.~species)+ylab("")+scale_fill_discrete(name="Altitude (M)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x = element_text(size = 6))

###chi-squared analyses for low-altitude reports
#just gundlachi
low <- subset(anolis,alt<=500)
n.sp.pre <- nrow(subset(low,species=="Anolis gundlachi" & timebin=="1955-1980"))
n.sp.post <- nrow(subset(low,species=="Anolis gundlachi" & timebin=="1980-2015"))
n.all.pre <- nrow(subset(low,timebin=="1955-1980"))
n.all.post <- nrow(subset(low,timebin=="1980-2015"))
chimatrix <- matrix(c(n.sp.pre,n.sp.post,n.all.pre,n.all.post),byrow = T, nrow = 2)
chisq.test(chimatrix)

#loop over all species
anolischi2fun <- function(i){
  n.sp.pre <- nrow(subset(low,species==i & timebin=="1955-1980"))
  n.sp.post <- nrow(subset(low,species==i & timebin=="1980-2015"))
  n.all.pre <- nrow(subset(low,timebin=="1955-1980"))
  n.all.post <- nrow(subset(low,timebin=="1980-2015"))
  chimatrix <- matrix(c(n.sp.pre,n.sp.post,n.all.pre,n.all.post),byrow = T, nrow = 2)
  chi <- chisq.test(chimatrix)
  c(i,round(n.sp.pre/n.all.pre,3),round(n.sp.post/n.all.post,3),round(chi$p.value,5))
}
chi2table <- data.frame(foreach(i=levels(anolis$species),.combine=rbind) %do% anolischi2fun(i),row.names=NULL)
names(chi2table) <- c("Species","relAbund.pre","relAbund.post" ,"p")
chi2table

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




