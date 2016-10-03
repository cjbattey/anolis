anolis <- read.csv("~/Dropbox/anolis/data/anolis_data.csv")

#distribution of collections across museums
museums <- ddply(anolis,.(institutionCode,ageClass),summarize,Reports=length(species))
ggplot(data=museums,aes(x=substr(institutionCode,1,4),y=Reports,fill=factor(ageClass)))+
  theme(axis.text.x = element_text(angle = 90, hjust =1,vjust=.5,size=6))+
  geom_bar(stat="identity")+ylab("Specimens")+ggtitle("Specimens  per Institution")

#across years
years <- ddply(subset(anolis,year>1960),.(year,species),summarize,n=length(day))
ggplot(data=years,aes(x=year,y=n,fill=species))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_bar(stat="identity")+ylab("Specimens")+xlab("")

#wilcox test on elevation distributions
ddply(subset(anolis,ageClass %in% c(2,4)),.(species),function (i){
  t <- wilcox.test(i$alt~i$ageClass,conf.int=T)
  sum <- c(pre=mean(i$alt[i$ageClass==2]),
           post=mean(i$alt[i$ageClass==4]),
           diff=round(t$estimate,digits=2),
           ci=t$conf.int,
           p=round(t$p.value,5))
  names(sum) <- c("pre","post","difference","CI_lower","CI_upper","p")
  sum
})

#elevation boxplots
ggplot(data=subset(anolis,ageClass %in% c(2,4)),aes(x=factor(ageClass),y=alt))+facet_grid(.~species)+theme_bw()+xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x=element_text(size=6))+
  ggtitle("Median Report Elevation, Pre vs. Post 1980")+
  #geom_point(size=.5,position="jitter",col="grey",alpha=0.6)+
  #geom_boxplot(notch=F,outlier.colour = NA,fill=NA)
  geom_boxplot()

#species elevation distributions
ggplot()+facet_wrap(~species)+
  geom_histogram(data=subset(anolis,ageClass==2),aes(alt,fill=ageClass),bins=20,fill="red",alpha=0.5)+
  geom_histogram(data=subset(anolis,ageClass==4),aes(alt,fill=ageClass),bins=20,fill="blue",alpha=0.5)

ggplot(subset(anolis,ageClass %in% c(2,4)),aes(x=pt.alt,fill=factor(ageClass)))+
  facet_wrap(~species)+
  geom_density(alpha=0.5,adjust=2)

anolis <- ddply(anolis,.(species,ageClass),summarize,sp.mean.alt=mean(na.omit(alt)),sp.sd.alt=sd(na.omit(alt))) %>% 
  join(.,anolis,by=c("species","ageClass"),type="right")

ggplot(data=subset(anolis,ageClass %in% c(2,4)),aes(x=factor(ageClass),y=alt))+facet_grid(.~species)+theme_bw()+xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x=element_text(size=6))+
  ggtitle("Mean Report Elevation")+
  geom_point(size=.6,position="jitter",col="grey",alpha=0.6)+
  geom_point(aes(y=sp.mean.alt),col="red")+
  geom_errorbar(aes(ymin=sp.mean.alt-sp.sd.alt,ymax=sp.mean.alt+sp.sd.alt))

#elevation regressions
ggplot(data=subset(anolis,year>1955),aes(x=year,y=alt))+facet_grid(.~species)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x=element_text(size=6))+
  scale_x_continuous(breaks=c(1960,2000))+
  geom_point(size=1,alpha=0.3)+
  geom_smooth(method="lm")

###analyzing effort by elevation bins
anolis$altbin <- cut(anolis$alt,seq(0,max(na.omit(anolis$alt)),250),labels=c("0-250","250-500","500-750","750-1000"))
anolis.altbin <- subset(anolis,is.na(altbin)==F)
altsummary <- ddply(anolis.altbin,.(timebin,altbin,species),summarize,n=length(species))

#all Anolis
ggplot(data=altsummary,aes(x=timebin,y=n,fill=altbin))+geom_bar(stat="identity")+ylab("Specimens")+
  ggtitle("All Anolis")+theme(axis.text.x = element_text(angle = 45, hjust = 1))

#by species
ggplot(data=altsummary,aes(x=timebin,y=n,fill=altbin))+geom_bar(stat="identity")+ggtitle("Per Species")+
  facet_grid(.~species)+ylab("")+scale_fill_discrete(name="Altitude (M)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),strip.text.x = element_text(size = 6))

###effort by elevation continuously
ggplot(data=anolis,aes(x=anolis$alt))+facet_wrap(~ageClass)+
  geom_histogram()
test <- subset(anolis,ageClass %in% c(2,4)) 
wilcox.test(test$alt~test$ageClass)

###chi-squared analyses for low-altitude reports
#just gundlachi
low <- subset(anolis,alt<=250)
n.sp.pre <- nrow(subset(low,species=="Anolis gundlachi" & ageClass==2))
n.sp.post <- nrow(subset(low,species=="Anolis gundlachi" & ageClass==4))
n.all.pre <- nrow(subset(low,ageClass==2))
n.all.post <- nrow(subset(low,ageClass==4))
chimatrix <- matrix(c(n.sp.pre,n.sp.post,n.all.pre,n.all.post),byrow = T, nrow = 2)
chisq.test(chimatrix)

#loop over all species
anolischi2fun <- function(i){
  n.sp.pre <- nrow(subset(low,species==i & ageClass==2))
  n.sp.post <- nrow(subset(low,species==i & ageClass==4))
  n.all.pre <- nrow(subset(low,ageClass==2))
  n.all.post <- nrow(subset(low,ageClass==4))
  chimatrix <- matrix(c(n.sp.pre,n.sp.post,n.all.pre,n.all.post),byrow = T, nrow = 2)
  chi <- chisq.test(chimatrix)
  c(i,round(n.sp.pre/n.all.pre,3),round(n.sp.post/n.all.post,3),chi$p.value)
}
chi2table <- data.frame(foreach(i=levels(factor(anolis$species)),.combine=rbind) %do% anolischi2fun(i),row.names = NULL)
names(chi2table) <- c("Species","relAbund.pre","relAbund.post" ,"p")
chi2table
