#Puerto Rico historical climate analyses

#load data
clim <- suppressWarnings(fread("data/climate/NCDC_longTermPRstations_742067.csv",na.strings="-9999"))
clim$year <- as.numeric(substr(clim$DATE,1,4))
clim$month <- as.numeric(substr(clim$DATE,5,6))
clim <- subset(clim,year < 2016)
clim$LATITUDE <- as.numeric(clim$LATITUDE)
clim$LONGITUDE <- as.numeric(clim$LONGITUDE)
clim <- clim[is.na(clim$MNTM)==F]
  
#check for year range by station
clim.sum <- ddply(clim,.(STATION_NAME),summarize,yrange=max(year)-min(year),minyear=min(year),maxyear=max(year))
good.stations <- clim.sum$STATION_NAME[clim.sum$maxyear>2014 & clim.sum$minyear<1961]
clim <- subset(clim,STATION_NAME %in% good.stations)

#remove station-years with < 12 months
data <- clim[1,]
for(i in levels(factor(clim$STATION_NAME))){
  station <- subset(clim,STATION_NAME==i)
  for (j in min(station$year):max(station$year)){
    year <- subset(station,year==j)
    n.months <- nrow(year)
    if(n.months==12){
      data <- rbind(data,year)
    }
  }
}

#get altitude of stations
alt <- crop(raster("data/alt_30s_bil/alt.bil"),ext)
coords <- data.frame(subset(data,!is.na(LATITUDE)))[,c("LONGITUDE","LATITUDE")]
data$alt[!is.na(data$LATITUDE)]<- extract(alt,SpatialPoints(coords))

avg <- ddply(data,.(year,STATION_NAME),summarize,temp=mean(MNTM))
regressions <- ddply(data,.(STATION_NAME,month),
            function(e) { 
              model <- lm(e$MNTM ~ e$year)
              p <- summary(model)[[4]][7]
              slope <- coef(model)[2]
              r2 <- summary(model)$r.squared
              c(p=p,r2=r2,slope=slope)
            })
sig.regressions <- subset(regressions,p<0.05)
sig.regressions$ID <- paste(sig.regressions$STATION_NAME,sig.regressions$month)
pghea("Average slope =",mean(sig.regressions$`slope.e$year`),"C/yr")

ggplot(data=avg,aes(x=year,y=temp,col=STATION_NAME))+theme_bw()+
  theme(legend.text=element_text(size=6),legend.key=element_rect(colour=NA,fill=NA))+
  geom_line(alpha=0.3)+
  geom_smooth(data=subset(avg,STATION_NAME %in% sig.regressions$STATION_NAME),method="lm",fill=NA,alpha=0.3)

#location of weather stations
map <- map_data("world")
ggplot()+coord_map()+xlim(-67.5,-65.5)+ylim(17.8,18.7)+
  geom_path(data=map,aes(x=long,y=lat,group=group))+
  geom_point(data=data.frame(data),aes(x=LONGITUDE,y=LATITUDE,col=STATION_NAME))

station.alt <- ddply(data,.(STATION_NAME),summarize,alt=round(mean(alt)))
#badass bimonthly facet climate plot
data$monthbin <- cut(data$month,seq(0,12,2),c("1-2","3-4","5-6","7-8","9-10","11-12"))
data$surveyID <- paste(data$STATION_NAME,data$month)

ggplot(data=data,aes(x=year,y=MNTM,col=STATION_NAME))+theme_bw()+facet_grid(.~monthbin)+
  scale_x_continuous(name="Year",breaks=c(1960,2000))+
  theme(legend.text=element_text(size=6))+
  geom_point(alpha=0.1,size=1)+
  geom_smooth(data=subset(data,surveyID %in% sig.regressions$ID),method="lm",fill=NA)

ggplot(data=subset(juncos),aes(x=year,y=MNTM,col=factor(month)))+theme_bw()+
  geom_point()+
  geom_smooth(method="lm")


