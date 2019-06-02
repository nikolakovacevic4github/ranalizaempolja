# Kreirao: Milic Stevan
# Datum: 17.5.2019.

# Opis: Istrazivanje (testiranje) zavisnosti u podacima

rm(list=ls(all=TRUE))

cat("\014")

library(sqldf)
library(data.table)
library(sp)
library(rgdal)
library(tidyr)
library(data.table)


### podesavanje radnog direktorijuma 

project_dir <- "D:/DataScience/Projects/working_dir/script"
setwd(project_dir);

### postavljanje direktorijuma sa podacima na radni direktorijum 
data_dir <- "D:/DataScience/Projects/working_dir/data"
setwd(data_dir)

### lista svih .csv fajlava sa podacima
csvFiles <- unlist(lapply(data_dir, function(dir) list.files(path = dir, pattern = '\\.csv$')))

### objedinjavanje svih podataka (iz svih .csv fajlova) tekuceg direktorijuma u jedan dataset 

df.lista <- list()
df.lista.obdanica <- list()
df.lista.noc <- list()
i <- 1
for(f in csvFiles){

  ### iscitati metapodatke (header) iz .csv fajla
  header <- read.csv(f, nrows = 7, header=FALSE, sep=",", encoding = "UTF-8")
  
  ### trasponovati metapodatke (header) i preurediti imenovanje metapodataka
  metadata <- transpose(header)
  colnames(metadata) = metadata[1, ] 
  metadata = metadata[-1, ] 
  
  ### iscitati merene podatke iz .csv fajla
  data <- read.csv(f, skip = 7, header=TRUE, sep=",", encoding = "UTF-8")
  
  m <- nrow(data) #broj redova

  # ID stanice ce predstavaljati naziv fajl
  station_id = gsub("\\.csv$","", f)
  
  #dodavanje kolone sa ID stanice (ID stanice je naziv .csv fajla)
  data$ID_STANICE  <- rep(station_id, m)
  
  #preimenovati pojedine kolone (iz razloga UTF-8 kodiranja podataka)
  names(metadata)[3]<-"GPS_POZICIJA"  
  names(metadata)[1]<-"LOKACIJA"  
  
  # pozicija stanice
  position <- metadata$GPS_POZICIJA
  lon <- as.numeric(strsplit(position, ",")[[1]][1])
  lat <- as.numeric(strsplit(position, ",")[[1]][2])

  #dodavanje kolone sa longitude i latitude koordinatama stanice
  data$LON <- rep(lon, m)
  data$LAT <- rep(lat, m)
  
  #custovanje faktor tipa u kolone koje sadrze datum i vreme (odvojeno)
  data$DATUM_VREME <- strptime(data$DATUM_VREME , format = "%Y-%m-%d %H:%M:%S")
  data$DATUM <- as.Date(data$DATUM_VREME , format = "%Y-%m-%d")
  data$SAT <- as.integer(strftime(data$DATUM_VREME, format = "%H"))
  
  #uklanjanje suvisnih kolona
  #data <- data[, setdiff(names(data), c("RBR"))]
  
  #izdvojiti samo neke kolone
  data <- subset(data, select=c("ID_STANICE", "LON", "LAT", "DATUM", "SAT","VREDNOST_POLJA", "TEMPERATURA", "VLAZNOST"))
  
  #uklanjanje eventualnih NA vrednosti
  data <- data[ which( !(is.na(data$ID_STANICE))),]
  data <- data[ which( !(is.na(data$LON))),]
  data <- data[ which( !(is.na(data$LAT))),]
  data <- data[ which( !(is.na(data$DATUM))),]
  data <- data[ which( !(is.na(data$SAT))),]
  data <- data[ which( !(is.na(data$VREDNOST_POLJA))),]
  data <- data[ which( !(is.na(data$TEMPERATURA))),]
  data <- data[ which( !(is.na(data$VLAZNOST))),]
  
  #dodavanje atribuda doba daba
  data.obdanica <- data[which(data$SAT >= 6 & data$SAT <= 18), ]
  data.noc <- data[which(data$SAT < 6 | data$SAT > 18), ]
  
  #uklanjanje suvisnih kolona
  data.obdanica <- data.obdanica[, setdiff(names(data.obdanica), c("SAT"))]
  data.noc  <- data.noc [, setdiff(names(data.noc ), c("SAT"))]
  
  #pakovanje podataka za ekstrakciju jednog seta sa svim podacima
  df.lista[[i]] <- data
  
  #agregacija na nivou doba dana i datuma za odredjenu stanicu
  data.obdanica.agrgirano_po_datumu <- aggregate(.~ID_STANICE+LON+LAT+DATUM, data=data.obdanica, mean, na.rm=TRUE)
  df.lista.obdanica[[i]] <- data.obdanica.agrgirano_po_datumu
  
  data.noc.agrgirano_po_datumu <- aggregate(.~ID_STANICE+LON+LAT+DATUM, data=data.noc, mean, na.rm=TRUE)
  df.lista.noc[[i]] <- data.noc.agrgirano_po_datumu
  i <- i + 1
  
}

##objedinjeni df sa svim podacima po dobu dana
df.obdanica <- do.call(rbind, df.lista.obdanica)
df.noc <- do.call(rbind, df.lista.noc)

#svih datumi u kojima su se realizovala neka merenja
datumi <-  unique(c(df.obdanica$DATUM, df.noc$DATUM))

#kreiranje novog direktorijuma ukoliko vec ne postoji
subDir<- "ulazni-podaci-sredjeno"
ifelse(!dir.exists(file.path(data_dir, subDir)), dir.create(file.path(data_dir, subDir)), FALSE)
setwd(file.path(data_dir, subDir))


for(d in datumi){
  
  #izvajanje podataka za odredjeni datum
  tmp.df.obdanica <- df.obdanica[which(df.obdanica$DATUM == d), ]
  tmp.df.noc <- df.noc[which(df.noc$DATUM == d), ]
  
  # kreranje SpatialDataFrame
  coordinates(tmp.df.obdanica) <- c("LON", "LAT")
  proj4string(tmp.df.obdanica) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  coordinates(tmp.df.noc) <- c("LON", "LAT")
  proj4string(tmp.df.noc) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  #eksport podataka u .shp
  writeOGR(tmp.df.obdanica, as.character(unique(tmp.df.obdanica$DATUM)), paste(toString(unique(tmp.df.obdanica$DATUM)), "obdanica", sep = "-"), driver="ESRI Shapefile")
  writeOGR(tmp.df.noc, as.character(unique(tmp.df.noc$DATUM)), paste(toString(unique(tmp.df.noc$DATUM)), "noc", sep = "-"), driver="ESRI Shapefile")
  
}


#objedinjeni podaci 
df.data <- do.call(rbind, df.lista)

# kreranje SpatialDataFrame
coordinates(df.data) <- c("LON", "LAT")
proj4string(df.data) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#eksport podataka u .shp
writeOGR(df.data, "objedinjeni-podaci", "stanice-svi-podaci", driver="ESRI Shapefile")




