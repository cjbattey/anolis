anolis <- read.csv("~/Dropbox/anolis/data/anolis_data.csv")

#distribution of collections across museums
museums <- ddply(anolis,.(institutionCode,timebin),summarize,Reports=length(species))
ggplot(data=museums,aes(x=substr(institutionCode,1,4),y=Reports,fill=timebin))+
  theme(axis.text.x = element_text(angle = 90, hjust =1,vjust=.5,size=6))+
  geom_bar(stat="identity")+ylab("Specimens")+ggtitle("Specimens  per Institution")

#across years
years <- ddply(subset(anolis,year>1960),.(year,species),summarize,n=length(day))
ggplot(data=years,aes(x=year,y=n,fill=species))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_bar(stat="identity")+ylab("Specimens")+xlab("")

#t-tests on pre vs. post 1980 elevations
ddply(anolis,.(species),function (i){
  t <- t.test(i$pt.alt~i$timebin)
  p <- round(t$p.value,5)
  means <- round(t$estimate,digits=2)
  diff <- round(t$conf.int,digits=2)
  sum <- c(pre=means[1],post=means[2],diff=means[2]-means[1],ci=paste(round(diff[1],2),"-",round(diff[2],2)),p=p)
  names(sum) <- c("pre","post","difference","CI","p")
  sum
})

#elevation boxplots
ggplot(data=anolis,aes(x=timebin,y=pt.alt))+facet_grid(.~species)+theme_bw()+xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x=element_text(size=6))+
  ggtitle("Median Report Elevation, Pre vs. Post 1980")+
  geom_point(size=.3,position="jitter",col="grey",alpha=0.4)+
  geom_boxplot(notch=F,outlier.colour = NA,fill=NA)

#elevation regressions
ggplot(data=subset(anolis,year>1955),aes(x=year,y=pt.alt))+facet_grid(.~species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x=element_text(size=6))+
  scale_x_continuous(breaks=c(1960,2000))+
  geom_point(size=1,alpha=0.3)+
  geom_smooth(method="lm")

###analyzing effort by elevation bins
anolis$altbin <- cut(anolis$pt.alt,seq(0,max(na.omit(anolis$pt.alt)),250),labels=c("0-250","250-500","500-750","750-1000"))
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
low <- subset(anolis,pt.alt<=250)
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
chi2table <- data.frame(foreach(i=levels(factor(anolis$species)),.combine=rbind) %do% anolischi2fun(i),row.names = NULL)
names(chi2table) <- c("Species","relAbund.pre","relAbund.post" ,"p")
chi2table

######### GIS/maps ######### (NOTE this is broken as of 5/17)
anolis.pts <- SpatialPointsDataFrame(coords=data.frame(anolis$long,anolis$lat),proj4string=crs(alt),
                                     data=data.frame(anolis$species,anolis$eventID,anolis$year,anolis$timebin))

#map of pre/post 1980 A. gundlachi localities
plot(alt)+
  points(anolis.pts,col="blue")



#rasterize point localities (warning: rasterize requires ~8min each to run at 30" resolution)
anolis.r <- rasterize(SpatialPoints(anolis.pts),alt30,fun="count")
anolis.pre <- subset(anolis,anolis$timebin=="1955-1980")
anolis.pre.r <- rasterize(data.frame(anolis.pre$long,anolis.pre$lat),alt30,fun="count")
anolis.post <- subset(anolis,anolis$timebin=="1980-2015")
anolis.post.r <- rasterize(data.frame(anolis.post$long,anolis.post$lat),alt30,fun="count")

gundlachi.r <- rasterize(SpatialPoints(gundlachi.pts),alt,fun="count")
gundlachi.pre <- subset(anolis,timebin=="1955-1980" & species=="Anolis gundlachi")
gundlachi.pre.r <- rasterize(data.frame(gundlachi.pre$long,gundlachi.pre$lat),alt30,fun="count")
gundlachi.post <- subset(anolis,timebin=='1980-2015' & species=="Anolis gundlachi")
gundlachi.post.r <- rasterize(data.frame(gundlachi.post$long,gundlachi.post$lat),alt30,fun="count")

#calculate effort-corrected gundlachi abundance pre/post 1980
gundlachi.pre.freq <- gundlachi.pre.r/anolis.pre.r
gundlachi.post.freq <- gundlachi.post.r/anolis.post.r

par(mfrow=c(2,1))
contour(alt30,levels=c(.00001,200,400,600,800,1000,1200))+plot(gundlachi.pre.freq,add=T)
contour(alt30,levels=c(.00001,200,400,600,800,1000,1200))+plot(gundlachi.post.freq,add=T)
par(mfrow=c(1,1))




