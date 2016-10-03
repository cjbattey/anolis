
#crop for El Yunque NF xlim=c(-66,-65.6),ylim=c(18.2,18.5)
alt.yunque <- projectRaster(from=alt1s,lc) %>% crop(.,c(825000,850000,2015000,2040000))
yunque.roads <- roads[roads@data$FULLNAME %in% c("Pr- 186","Pr- 191"),]

#pr186 and pr191 elevation profiles
#note should rasterize anolis data to 1km, divide by all anolis, calculate frequencies
pts2.r <- rasterize(pts2,projectRaster(from=alt,to=projectExtent(alt,crs(lc)),res=1000),fun="count") 
bg2.r <- rasterize(bgpts2,projectRaster(from=alt,to=projectExtent(alt,crs(lc)),res=1000),fun="count")
pts2.freq <- pts2.r/bg2.r 
pts2.freq <- rasterToPoints(pts2.freq,spatial = T)

pts4.r <- rasterize(pts4,projectRaster(from=alt,to=projectExtent(alt,crs(lc)),res=1000),fun="count") 
bg4.r <- rasterize(bgpts4,projectRaster(from=alt,to=projectExtent(alt,crs(lc)),res=1000),fun="count")
pts4.freq <- pts4.r/bg4.r 
pts4.freq <- rasterToPoints(pts4.freq,spatial = T)

pr186 <- roads[roads@data$FULLNAME %in% c("Pr- 186"),] %>% spTransform(.,crs(lc))
pr186.buffer <- buffer(pr186,width=1000)
pr186.pts <- rasterize(pr186,projectRaster(from=alt,to=projectExtent(alt,crs(lc)),res=1000)) %>% rasterToPoints(.,spatial=T)
pr186.alt <- extract(alt.yunque,pr186.pts)
pr186.profile <- data.frame(order=1:length(pr186.alt),alt=pr186.alt)
pr186.profile$rdkm <- (pr186.profile$order * 30)/1000
pr186.profile$smoothed.alt <- loess(pr186.profile$alt ~ pr186.profile$rdkm,data=pr186.profile)$y

pts2.186 <- intersect(pts2.freq,pr186.buffer)
p <- apply(spDists(pr186.pts,pts2.186),2,which.min)
p <- data.frame(nearestRdPt=p,alt=unlist(extract(alt.yunque,pr186.pts[p,])))
p$rdkm <- (p$nearestRdPt * 30)/1000
p$freq <- pts2.186@data$layer
pts2.186 <- p

pts4.186 <- intersect(pts4.freq,pr186.buffer)
p <- apply(spDists(pr186.pts,pts4.186),2,which.min)
p <- data.frame(nearestRdPt=p,alt=unlist(extract(alt.yunque,pr186.pts[p,])))
p$rdkm <- (p$nearestRdPt * 30)/1000
p$freq <- pts4.186@data$layer
pts4.186 <- p

ggplot()+theme_bw()+
  #geom_smooth(data=pr186.profile,aes(x=rdkm,y=alt),span=.01,fill=NA,col="black",lwd=.5)+
  geom_line(data=pr186.profile,aes(x=rdkm,y=alt),col="black",lwd=.5)+
  geom_point(data=pts2.186,aes(x=rdkm,y=alt,size=freq),col="blue",alpha=0.5)+
  geom_point(data=pts4.186,aes(x=rdkm,y=alt,size=freq),col="red",alpha=0.5)

#elevation profile plot of Pr-191
pr191 <- roads[roads@data$FULLNAME %in% c("Pr- 191"),] %>% spTransform(.,crs(lc))
pr191.buffer <- buffer(pr191,width=1000)
pr191.pts <- rasterize(pr191,lc) %>% rasterToPoints(.,spatial=T)
pr191.alt <- extract(alt.yunque,pr191.pts)
pr191.profile <- data.frame(order=1:length(pr191.alt),alt=pr191.alt)
pr191.profile$rdkm <- (pr191.profile$order * 30)/1000

pts2.191 <- intersect(pts2.freq,pr191.buffer)
p <- apply(spDists(pr191.pts,pts2.191),2,which.min)
p <- data.frame(nearestRdPt=p,alt=unlist(extract(alt.yunque,pr191.pts[p,])))
p$rdkm <- (p$nearestRdPt * 30)/1000
p$freq <- pts2.191@data$layer
pts2.191 <- p

pts4.191 <- intersect(pts4.freq,pr191.buffer)
p <- apply(spDists(pr191.pts,pts4.191),2,which.min)
p <- data.frame(nearestRdPt=p,alt=unlist(extract(alt.yunque,pr191.pts[p,])))
p$rdkm <- (p$nearestRdPt * 30)/1000
p$freq <- pts4.191@data$layer
pts4.191 <- p

ggplot()+theme_bw()+
  #geom_smooth(data=pr191.profile,aes(x=rdkm,y=alt),span=.,fill=NA,col="black",lwd=.5)+
  geom_line(data=pr191.profile,aes(x=rdkm,y=alt),col="black",lwd=.5)+
  geom_point(data=pts2.191,aes(x=rdkm,y=alt,size=freq),col="blue")+
  geom_point(data=pts4.191,aes(x=rdkm,y=alt,size=freq),col="red")



#plot 186 & 191 localities
plot(crop(age4,c(825000,850000,2015000,2040000)))
plot(rasterToContour(alt.yunque),add=T,col="grey",lwd=.5)
plot(pr191,add=T)+plot(pr186,add=T)+
  points(pts4,col="red")
points(pts2,col="blue")

pr186.alt <- extract(alt.yunque,pr186.pts) %>% unlist() %>% data.frame(alt=.,order=1:length(.))

ggplot()+geom_line(data=pr186.alt,aes(x=order,y=alt))+
  geom_point(data=subset(anolis,species=="Anolis gundlachi"),aes(x=))

plot(alt2)+plot(yunque.roads,add=T)+points(bgpts2)+points(pts2,col="red",cex=0.2)
plot(alt2)+plot(yunque.roads,add=T)+points(bgpts4)+points(pts4,col="red",cex=0.2)





