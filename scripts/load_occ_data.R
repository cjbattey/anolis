#load, filter, and georeference occurrence records
setwd("~/Dropbox/anolis/")
library(data.table);library(raster);library(ggplot2);library(plyr);library(foreach);library(stringr);library(magrittr)

anolis <- data.frame(fread("~/Dropbox/anolis/data/locs/gbif_PRanolis/occurrence_xl.txt")) 
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
fmnh.gbif$verbatimLocality <- fmnh.merge$verbatimLocality.y
fmnh.gbif$verbatimEventDate <- fmnh.merge$verbatimEventDate.y
anolis <- rbind(anolis,fmnh.gbif)

#AMNH
anolis <- subset(anolis,institutionCode != "AMNH")

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

# Add binary column for easy binning
anolis$timebin <- factor(anolis$year < 1980)
levels(anolis$timebin) <- c("1980-2015","1955-1980")
anolis$timebin <- factor(anolis$timebin,levels=c("1955-1980","1980-2015"))

anolis$hascoords <- factor(!is.na(anolis$decimalLatitude) & !is.na(anolis$decimalLongitude))
levels(anolis$hascoords) <- c("locality","coordinates")

##################################################################
################ Adding Verbatim Elevations ######################
##################################################################
#for specimens with "VERBATIM ELEVATION:" in the verbatim locality, convert to meters and copy to elevation column
for(i in 1:nrow(anolis)){  
  row <- anolis[i,]
  if(grepl("ELEVATION",row$locality)){    
    ve.string <- strsplit(row$locality,"ELEVATION:") %>% unlist() %>% .[2]
    if(length(unlist(strsplit(ve.string,"-")))==2){                               # if a range is provided
      range1 <- strsplit(ve.string,"-|[A-z]|'") %>% unlist() %>% .[1] %>% as.numeric()
      range2 <- strsplit(ve.string,"-|[A-z]|'") %>% unlist() %>% .[2] %>% as.numeric()
      ve <- mean(c(range1,range2))
    } else if(grepl("ca.",ve.string)){                                            # if preceded by "ca."
      ve <- strsplit(ve.string,"ca.|[A-Za-z]|'|-") %>% unlist() %>% .[2] %>% as.numeric()
    } else {
      ve <- as.numeric(unlist(strsplit(ve.string,"[A-Za-z]|'|-"))[1])
    }
    if(ve > 1400 | grepl("ft|FT|feet|Feet|'",ve.string)){                         # convert to meters     
      ve <- ve * 0.3048
      anolis$elevation[i] <- ve
    } 
    if(grepl("M|m|meters|Meters",ve.string)){                 
      anolis$elevation[i] <- ve
    }
  }
}

















