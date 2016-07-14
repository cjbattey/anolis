#load, filter, and georeference occurrence records
setwd("~/Dropbox/anolis/")
library(data.table);library(raster);library(ggplot2);library(plyr);library(foreach);library(stringr);library(magrittr);library(dismo);library(rgeos)
ext <- extent(-67.5,-65.5,17.8,18.7)

#use bash to remove embedded nulls in GBIF output (accented characters apparently not in UTF-8...)
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

#AMNH
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
anolis$hascoords <- factor(!is.na(anolis$decimalLatitude) & !is.na(anolis$decimalLongitude))
levels(anolis$hascoords) <- c("locality","coordinates")

anolis <- subset(anolis,locality != "")
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

#13787 records with locality
#9080 records with coordinates or verbatim elevations
#7380 with one of: GPS, verbatim elevation, or stated coordinate uncertainty

#############################################################
########### georeferencing text localities ##################
#############################################################
# georef <- subset(anolis,grepl("gps|GPS",anolis$locality) | !(year>=2000 & hascoords=="coordinates"))
# localities <- ddply(georef,.(locality,verbatimLocality,institutionCode,recordedBy,year,georeferenceSources,decimalLatitude,
#                              decimalLongitude,coordinateUncertaintyInMeters,elevation),summarize,n=length(day))
# loc <- ddply(anolis,.(locality,eventID),summarize,n=length(gbifID))
# write.csv(loc,"anolis_localities.csv",row.names = F,fileEncoding = "UTF-8")

#manually check localities in Google Earth and join back to anolis table
localities <- data.frame(fread("~/Downloads/anolis_localities - anolis_localities (2).csv",encoding="UTF-8"))
anolis <- join(anolis,localities,by=c("locality","eventID"),type="left",match="first")
anolis$lat[grep("GPS|gps",anolis$georeferenceSources)] <- anolis$decimalLatitude[grep("GPS|gps",anolis$georeferenceSources)]
anolis$long[grep("GPS|gps",anolis$georeferenceSources)] <- anolis$decimalLongitude[grep("GPS|gps",anolis$georeferenceSources)]

#add UTM zone and coordinates
anolis$utmZone[anolis$long < -66] <- 19
anolis$utmZone[anolis$long > -66] <- 20
for(i in 1:nrow(anolis)){
  if(!is.na(anolis[i,]$utmZone)){
    utm <- data.frame(anolis[i,"long"],anolis[i,"lat"]) %>%
                          SpatialPoints(.,proj4string=crs(alt)) %>%
                            spTransform(.,crs(paste0("+proj=utm +zone=",anolis[i,"utmZone"],"+north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))) %>%
                              data.frame(.)
    anolis[i,"utmW"] <- utm[1]
    anolis[i,"utmN"] <- utm[2]
  }
}

##############################################################
############# get altitude from coordinates ##################
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
alt <- raster("~/Dropbox/anolis/data/elevation/PuertoRico_altitude_1sec_USGSNED2013.tif")

#extract point localities (test only)
anolis$pointAlt[!is.na(anolis$lat)] <- extract(alt,SpatialPoints(data.frame(anolis$long[!is.na(anolis$lat)],anolis$lat[!is.na(anolis$lat)]))) 

#extract mean altitude of buffered uncertainty radius
for(i in 1:nrow(anolis)){
  row <- anolis[i,]
  if(!is.na(row$utmW)){
    
  }
}


bufferedPts <- for(i in 1:nrow(coords)){
  row <- coords[i,]
  pt <- SpatialPoints(data.frame(row$decimalLongitude,row$decimalLatitude),proj4string=crs(alt))
  if(grepl("GPS|gps",row$georeferenceSources) | row$year > 1995){                                #extract elevation if pt is GPS
    coords[i,]$elevation <- extract(alt,pt)
  } else if (is.na(row$elevation) & !is.na(row$coordinateUncertaintyInMeters)){                  #take mean elevation over uncertainty radius for georeferenced points
    if(row$decimalLongitude < -66){
      pt <- spTransform(pt,crs("+proj=utm +zone=19 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    } else if(row$decimalLongitude > -66){
      pt <- spTransform(pt,crs("+proj=utm +zone=20 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    }
      bufpt <- buffer(pt,row$coordinateUncertaintyInMeters)
      coords[i,]$elevation <- spTransform(bufpt,crs(alt)) %>% extract(alt,.) %>% unlist() %>% mean()
  }
}

coords.pts <- SpatialPointsDataFrame(data.frame(coords$decimalLongitude,coords$decimalLatitude),
                                     data=data.frame(coords$year,coords$month,coords$timebin,coords$species))

#extract altitudes & recombine with full dataset


coords.elevation <- extract(alt,anolis.pts) 

anolis$elevation[anolis$hascoords=="coordinates"] <- anolis.alt








