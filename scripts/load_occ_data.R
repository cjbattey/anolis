#load, filter, and georeference occurrence records
setwd("~/Dropbox/anolis/")
library(data.table);library(raster);library(ggplot2);library(plyr);library(foreach);library(stringr);library(magrittr);library(dismo);library(rgeos)
ext.pr <- extent(-67.6,-65.2,17.8,18.7)
ext.yunque <- extent(825000,850000,2015000,2040000)

#remove embedded nulls in GBIF output (UTF-16?)
file <- "./data/locs/gbif_PRanolis/occurrence.txt"
tt <- tempfile()  
system(paste0("tr < ", file, " -d '\\000' >", tt))
anolis <- data.frame(fread(tt))

#anolis <- data.frame(fread("~/Dropbox/anolis/data/locs/gbif_PRanolis/occurrence_xl.txt",encoding="Latin-1")) 
anolis <- anolis[names(anolis) %in% c("gbifID","institutionCode","basisOfRecord","catalogNumber","recordedBy",
                                      "eventDate","verbatimEventDate","year","month","day","countryCode","stateProvince","county","municipality","locality",
                                      "verbatimLocality","verbatimElevation","locationRemarks","decimalLatitude","decimalLongitude",
                                      "coordinateUncertaintyInMeters","georeferenceProtocol","georeferenceSources",
                                      "georeferenceRemarks","species","infraspecificEpithet","elevation")]
anolis <- subset(anolis,basisOfRecord=="PRESERVED_SPECIMEN" & species != "") 

### Dealing with museums that suppress locality or date on GBIF 
anolis <- subset(anolis,locality != "")

#FMNH
fmnh.mus <- read.csv("~/Dropbox/anolis/data/locs/fmnh_anolis.csv")
names(fmnh.mus) <- c("catalogNumber","collectionNumber","Taxon","verbatimLocality","Details","verbatimEventDate","Details2",
                     "recordedBy","IDnotes","specimen.loc","habitatNotes","sex")
fmnh.gbif <- subset(anolis,institutionCode == "FMNH") %>% arrange(.,catalogNumber) #sort by catalogNumber bc merge will sort on the "by" column
anolis <- subset(anolis,institutionCode != "FMNH")
fmnh.merge <- merge(fmnh.gbif,fmnh.mus,by="catalogNumber")
fmnh.gbif$locality <- fmnh.merge$verbatimLocality.y
fmnh.gbif$verbatimEventDate <- fmnh.merge$verbatimEventDate.y
anolis <- rbind(anolis,fmnh.gbif)

anolis <- subset(anolis,institutionCode %in% c("AMNH","Royal Ontario Museum: ROM")==F)

#LSU, UMMZ also suppress most data

#######################################################################
##################### Missing/Ambiguous Dates #########################
#######################################################################
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
anolis <- rbind(anolis,baddates)                                                 # recombine reports

# Add binary columns for easy binning
anolis$timebin <- factor(anolis$year < 1980)
levels(anolis$timebin) <- c("1980-2015","1955-1980")
anolis$timebin <- factor(anolis$timebin,levels=c("1955-1980","1980-2015"))
anolis$eventID <- paste(anolis$institutionCode,anolis$locality,anolis$year,sep=",")

##################################################################
################ Adding Verbatim Elevations ######################
##################################################################
#for specimens with "VERBATIM ELEVATION:" in the verbatim locality, convert to meters and copy to elevation column
for(i in 1:nrow(anolis)){  
  row <- anolis[i,]
  if(grepl("ELEVATION",row$locality)){                                            # if ELEVATION in locality, copy text after ":"
    ve.string <- strsplit(row$locality,"ELEVATION:") %>% unlist() %>% .[2]
    if(length(unlist(strsplit(ve.string,"-")))==2){                               # if a range is provided, take mean.
      range1 <- strsplit(ve.string,"-|[A-z]|'") %>% unlist() %>% .[1] %>% as.numeric()
      range2 <- strsplit(ve.string,"-|[A-z]|'") %>% unlist() %>% .[2] %>% as.numeric()
      ve <- mean(c(range1,range2))
    } else if(grepl("ca.",ve.string)){                                            
      ve <- strsplit(ve.string,"ca.|[A-Za-z]|'|-") %>% unlist() %>% .[2] %>% as.numeric()
    } else {                                        
      ve <- as.numeric(unlist(strsplit(ve.string,"[A-Za-z]|'|-"))[1])
    }
    
    if(ve > 1400 | grepl("ft|FT|feet|Feet|'",ve.string)){                         # convert to meters as needed
      ve <- ve * 0.3048
      anolis$elevation[i] <- ve
    } 
    if(grepl("M|m|meters|Meters",ve.string)){                 
      anolis$elevation[i] <- ve
    }
  }
}

#############################################################
########### georeferencing text localities ##################
#############################################################
#write locality table to file for manual georeferencing
# georef <- subset(anolis,grepl("gps|GPS",anolis$locality) | !(year>=2000 & hascoords=="coordinates"))
# localities <- ddply(georef,.(locality,verbatimLocality,institutionCode,recordedBy,year,georeferenceSources,decimalLatitude,
#                              decimalLongitude,coordinateUncertaintyInMeters,elevation),summarize,n=length(day))
# loc <- ddply(anolis,.(locality,eventID),summarize,n=length(gbifID))
# write.csv(loc,"anolis_localities.csv",row.names = F,fileEncoding = "UTF-8")

#add georeferenced coordinates. use gbif coords if collected by gps.
#note: looks like some likely gps coords have NA sources on gbif.
localities <- data.frame(fread("~/Dropbox/anolis/data/locs/anolis_localities_21Jul16.csv",encoding="UTF-8"))
anolis <- join(anolis,localities,by=c("locality","eventID"),type="left",match="first")
anolis$lat[anolis$year>1997 & !is.na(anolis$decimalLatitude)] <- anolis$decimalLatitude[anolis$year>1997 & !is.na(anolis$decimalLatitude)]
anolis$long[anolis$year>1997 & !is.na(anolis$decimalLatitude)] <- anolis$decimalLongitude[anolis$year>1997 & !is.na(anolis$decimalLatitude)]
anolis$uncertainty[anolis$year>1997 & !is.na(anolis$decimalLatitude)] <- 30

##############################################################
############### extract coordinate elevations ################
##############################################################
####read in and merge altitude rasters (1-arc-sec = ~30M)
# setwd("~/Dropbox/anolis/data/elevation/")
# folders <- list.files()
# elev <- lapply(folders,FUN=function(i){
#   files <- list.files(paste0("./",i)) 
#   imageFile <- files[grep(".img",files)]
#   raster(paste0("./",i,"/",imageFile))
# })
# alt <- merge(elev[1][[1]],elev[2][[1]],elev[3][[1]],elev[4][[1]],elev[5][[1]],elev[6][[1]],elev[7][[1]],elev[8][[1]])
# writeRaster(alt,"PuertoRico_altitude_1sec_USGSNED2013.tif")

#load merged 1" altitudes, extract from coordinates
alt <- raster("~/Dropbox/anolis/data/alt_30s_bil/alt.bil") %>% crop(ext.pr)
alt1s<- raster("~/Dropbox/anolis/data/elevation/PuertoRico_altitude_1sec_USGSNED2013.tif") %>% crop(ext.pr)

#extract point localities
#anolis$pt.alt[!is.na(anolis$lat)] <- extract(alt,SpatialPoints(data.frame(anolis$long[!is.na(anolis$lat)],anolis$lat[!is.na(anolis$lat)]))) 

# #extract mean altitude of buffered uncertainty radius (note: extract v slow, try overnight/try rewriting function)
anolis <- subset(anolis,!is.na(lat))
pt <- data.frame(anolis$long,anolis$lat) %>%
        SpatialPoints(proj4string=crs(alt)) %>%
          spTransform("+proj=utm +zone=19 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
poly <- gBuffer(pt,byid=T,width=anolis$uncertainty) %>% spTransform(crs(alt))
poly.alts <- extract(alt,poly)
mean.alt <- unlist(lapply(poly.alts,function(e) mean(na.omit(e))))
anolis$pt.alt <- mean.alt
  
# use verbatim elevation where available
anolis$pt.alt[!is.na(anolis$elevation)] <- anolis$elevation[!is.na(anolis$elevation)]

#add age classes
anolis$ageClass[anolis$year>1935 & anolis$year<=1951] <- 1
anolis$ageClass[anolis$year>1951 & anolis$year<=1977] <- 2
anolis$ageClass[anolis$year>1977 & anolis$year<=1990] <- 3
anolis$ageClass[anolis$year>1990] <- 4

#final data filters
anolis <- subset(anolis,uncertainty <= 1000)
good.species <- ddply(anolis,.(species,timebin),summarize,n=length(day)) %>% subset(.,n>300) %>% .$species
anolis <- subset(anolis,species %in% good.species)
write.table(anolis,"./data/anolis_data.csv",sep=",")
