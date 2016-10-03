encodings <- iconvlist()

for(i in encodings){
  tryCatch({
    a <- read.csv("./data/locs/gbif_PRanolis/occurrence.txt",header=T,fileEncoding = "UTF-16BE",nrows=2) %>% names()
    names(a)
    i
  },error=function(e){}
  )
}

file <- "./data/locs/gbif_PRanolis/occurrence.txt"
encodings

read.csv("./data/locs/gbif_PRanolis/occurrence.txt",header=T,fileEncoding = "UTF-8-MAC",nrows=2) %>% names()

read.csv("~/Dropbox/anolis/anolis_localities_27sept2016.csv",fileEncoding="UTF-16",nrows=2) %>% names()
loc <- read.csv("~/Dropbox/anolis/anolis_localities_27sept2016.csv",fileEncoding="UTF-8",nrows=2)
