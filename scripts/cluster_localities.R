#clustering and ID'ing resurveyed localities
anolis <- read.csv("~/Dropbox/anolis/data/anolis_data.csv")                  #read in 
coords <- subset(anolis.full,!is.na(lat))                                    #pull rows with coordinates
coords <- ddply(coords,.(eventID,long,lat,ageClass),summarize,nobs=length(day)) #summarize by locality+ageClass
pts <- SpatialPoints(coords=data.frame(coords$long,coords$lat),proj4string=crs(proj4.wgs))
dist <- spDists(pts,pts) %>% as.dist()                                       #get pairwise distance matrix
fit <- hclust(dist,method="single")                                          #cluster
clusters <- cutree(fit,h=1)                                                  #group points within h km
coords$localityGroup <- clusters                                             #add clusters to locality summary
anolis <- join(anolis,coords,by=c("eventID","long","lat","ageClass"))        #add localityGroup to full dataset
groups <- ddply(anolis,.(localityGroup),summarize,nAgeClass=length(unique(ageClass))) %>% subset(nAgeClass>1) #find locality groups w/specimens in multiple ageClasses
rsv <- subset(anolis,localityGroup %in% groups$localityGroup)                #subset to resurveyed locality groups
groups <- ddply(rsv,.(localityGroup,ageClass),summarize,n=length(day)) %>%   #find groups w min specimens > 10 per ageClass
  ddply(.(localityGroup),summarize,min=min(n)) %>%
  subset(min>=5) %>% 
  .$localityGroup
rsv <- subset(anolis,localityGroup %in% groups) 

#check clusters
map <- map_data("world")
ggplot()+coord_map()+theme_bw()+xlim(-67.5,-65.5)+ylim(17.5,19)+
  geom_path(data=map,aes(x=long,y=lat,group=group))+
  geom_point(data=rsv,aes(x=long,y=lat,col=factor(localityGroup)))

#species composition by cluster
spCounts <- ddply(rsv,.(localityGroup,species),summarize,n=length(day)) 
groupEffort <- ddply(rsv,.(localityGroup),summarize,total=length(day))
spCounts <- join(spCounts,groupEffort,by="localityGroup")
spCounts$freq <- spCounts$n/spCounts$total
ggplot(spCounts,aes(x=species,y=freq))+facet_wrap(~localityGroup)+
  geom_bar(stat="identity")

### compare gundlachi frequency in clusters in ageClass 2 v 4
has.gundlachi <- ddply(rsv,.(localityGroup),summarize,hasGund="Anolis gundlachi" %in% species) %>% subset(hasGund==T) %>% .$localityGroup
gund <- subset(rsv,localityGroup %in% has.gundlachi)
gund <- subset(gund,ageClass %in% c(2,4))
groupEffort <- ddply(gund,.(localityGroup,ageClass),summarize,total=length(day),alt=mean(alt))
spCount <- ddply(gund,.(localityGroup,ageClass,species),summarize,spCount=length(day))
spCount <- join(spCount,groupEffort,by=c("localityGroup","ageClass"))
spCount$freq <- spCount$spCount/spCount$total
g <- subset(spCount,species=="Anolis gundlachi")
ggplot(g,aes(x=ageClass,y=freq,fill=alt))+facet_wrap(~localityGroup)+
  geom_bar(stat="identity")
