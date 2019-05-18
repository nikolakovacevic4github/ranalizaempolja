# Kreirao: Milic Stevan
# Datum: 17.5.2019.

# Opis: Istrazivanje (testiranje) zavisnosti u podacima

rm(list=ls(all=TRUE))

cat("\014")

library(sqldf)
library(data.table)
library(sp)
library(rgdal)


### podesavanje radnog direktorijuma 

project_dir <- "D:/DataScience/Projects/working_dir/script"
setwd(project_dir);

### postavljanje direktorijuma sa podacima na radni direktorijum 
data_dir <- "D:/DataScience/Projects/working_dir/data"
setwd(data_dir)

### lista svih .csv fajlava sa podacima
csvFiles <- unlist(lapply(data_dir, function(dir) list.files(path = dir, pattern = '\\.csv$')))

### objedinjavanje svih podataka (iz svih .csv fajlova) tekuceg direktorijuma u jedan dataset 
#df <- data.frame() #rezultujuci dataset 
for(f in csvFiles){
  ### iscitati metapodatke (header) iz .csv fajla
  header <- read.csv(f, nrows = 6, header=FALSE, sep=",", encoding = "UTF-8")
  
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
  
  #uklanjanje suvisnih kolona
  data <- data[, setdiff(names(data), c("RBR"))]
  
  #uniranje svih podataka u jedan dataset 
  #df <- merge(df, data, all=TRUE)
  
  # kreranje SpatialDataFrame objekta od DataFrame objekta
  coordinates(data) <- c("LON", "LAT")
  proj4string(data) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  #export dataset-a u .shp
  #writeOGR(data, "shp_dataset", paste(metadata$LOKACIJA, "dataset", sep = "_"), driver="ESRI Shapefile")
  writeOGR(data, "shp_dataset", metadata$LOKACIJA, driver="ESRI Shapefile")
  
}






