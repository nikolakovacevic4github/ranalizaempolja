# Opis: Istrazivanje (testiranje) zavisnosti u podacima

rm(list=ls(all=TRUE))

cat("\014")


#install.packages("data.table")
library(data.table)
#install.packages("sqldf")
library(sqldf)
#install.packages("outliers")
library(outliers)
#install.packages("sp")
library(sp)
#install.packages("gstat")
library(gstat)
#install.packages("rgdal")
library(rgdal)
#install.packages("raster")
library(raster)
#install.packages("tmap")
library(tmap)
#install.packages("GSIF")
library(GSIF)


#pocetak izvrsavanja
ptm <- proc.time()

  
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
  
  #pakovanje podataka za ekstrakciju jednog seta sa svim podacima
  df.lista[[i]] <- data
  
  i <- i + 1
}

##objedinjeni df sa svim podacima
df <- do.call(rbind, df.lista)
#names(df)
length(df$ID_STANICE)

########## ANALIZA GRUBIH GRESKA - Outlaiers ##################################################################################

#liminacija nepotrebnih kolona - SAT
df <- df[ ,setdiff(names(df), c("SAT"))]
names(df)

#agregacija na nivou datuma (DATUM) za odredjenu stanicu (ID_STANICE) na koordinatama LON i LAT
stat_fun <- function(x) c(Mean = mean(x), Max = max(x), SD = sd(x))
df.agregirano.statistika <- do.call(data.frame, aggregate(.~ ID_STANICE + DATUM + LON + LAT, data=df, stat_fun))
#names(df.agregirano.statistika)
length(df.agregirano.statistika$ID_STANICE)

#liminacija nepotrebnih kolona - LON/LAT
#df.agregirano.statistika <- df.agregirano.statistika[ ,setdiff(names(df.agregirano.statistika), c("LON", "LAT"))]

#join dva dataseta
df.merged <- merge(df, df.agregirano.statistika, by=c("ID_STANICE","DATUM", "LON", "LAT"))
#names(df.merged)
length(df.merged$ID_STANICE)

### eliminacija grubih gresaka po pojedinacnim atributima (feature-ima) 

stat_fun2 <- function(x) c(Mean = mean(x), SD = sd(x))

###### 1. dataset oslobonjen grubih gresaka merenja EM polja (p = 0.95)###
df.clean.EM <- df.merged[which(abs(df.merged$VREDNOST_POLJA - df.merged$VREDNOST_POLJA.Mean)  < 3 * df.merged$VREDNOST_POLJA.SD), ]
#names(df.clean.EM)
length(df.clean.EM$ID_STANICE)

#subset sa odgovarajucim atributima oslobodjen prethodno grubih gresaka
df.clean.EM.subset <- df.clean.EM[, c("ID_STANICE", "DATUM", "VREDNOST_POLJA", "LON", "LAT")]
#names(df.clean.EM.subset)
length(df.clean.EM.subset$ID_STANICE)

#agregacija 
data.agrgirano.EM <- do.call(data.frame, aggregate(.~ ID_STANICE + DATUM + LON + LAT, data=df.clean.EM.subset, stat_fun2))
#names(data.agrgirano.EM)
length(data.agrgirano.EM$ID_STANICE)


###### 2. dataset oslobonjen grubih gresaka merenja TEMPERATURE (p = 0.95)###
df.clean.TEMP <- df.merged[which(abs(df.merged$TEMPERATURA - df.merged$TEMPERATURA.Mean)  < 3 * df.merged$TEMPERATURA.SD), ]
#names(df.clean.TEMP)
length(df.clean.TEMP$ID_STANICE)

#subset sa odgovarajucim atributima oslobodjen prethodno grubih gresaka
df.clean.TEMP.subset <- df.clean.TEMP[, c("ID_STANICE", "DATUM", "TEMPERATURA", "LON", "LAT")]
#names(df.clean.TEMP.subset)
length(df.clean.TEMP.subset$ID_STANICE)

#agregacija 
data.agrgirano.TEMP <- do.call(data.frame, aggregate(.~ ID_STANICE + DATUM + LON + LAT, data=df.clean.TEMP.subset, stat_fun2))
#names(data.agrgirano.TEMP)
length(data.agrgirano.TEMP$ID_STANICE)


###### 3. dataset oslobonjen grubih gresaka merenja VLAZNOSTI (p = 0.95)###
df.clean.VLAZ <- df.merged[which(abs(df.merged$VLAZNOST - df.merged$VLAZNOST.Mean)  < 3 * df.merged$VLAZNOST.SD), ]
#names(df.clean.VLAZ)
length(df.clean.VLAZ$ID_STANICE)

#subset sa odgovarajucim atributima oslobodjen prethodno grubih gresaka
df.clean.VLAZ.subset <- df.clean.VLAZ[, c("ID_STANICE", "DATUM", "VLAZNOST", "LON", "LAT")]
#names(df.clean.VLAZ.subset)
length(df.clean.VLAZ.subset$ID_STANICE)

#agregacija 
data.agrgirano.VLAZ <- do.call(data.frame, aggregate(.~ ID_STANICE + DATUM + LON + LAT , data=df.clean.VLAZ.subset, stat_fun2))
#names(data.agrgirano.VLAZ)
length(data.agrgirano.VLAZ$ID_STANICE)


### spajanje svih pojedinacnih datasetova oslobodjenih grubih gresaka

df1 <- data.agrgirano.EM
df2 <- data.agrgirano.TEMP
df3 <- data.agrgirano.VLAZ

df.merged.out1 <- merge(df1, df2, bFy=c("ID_STANICE","DATUM", "LON", "LAT"))
#names(df.merged.out1)
length(df.merged.out1$ID_STANICE)

df.merged.out <- merge(df.merged.out1, df3, by=c("ID_STANICE","DATUM", "LON", "LAT"))
#names(df.merged.out)
length(df.merged.out$ID_STANICE)


# df_clean_EM_subset <- df.clean.EM.subset
# df_clean_TEMP_subset <- df.clean.TEMP.subset
# df_clean_VLAZ_subset <- df.clean.VLAZ.subset
# 
# df_merged_out1 <- sqldf("select * from df_clean_EM_subset as dat1
#                      left outer join df_clean_TEMP_subset as dat2 
#                         on dat1.ID_STANICE = dat2.ID_STANICE and dat1.DATUM = dat2.DATUM")
# 
# df.merged.out <- sqldf("select * from df.merged.out1 as dat1
#                      left outer join df_clean_VLAZ_subset as dat2 
#                         on dat1.ID_STANICE = dat2.ID_STANICE and dat1.DATUM = dat2.DATUM")



# df1 <- data.agrgirano.EM
# df2 <- data.agrgirano.TEMP
# df3 <- data.agrgirano.VLAZ
# 
# df.merged.out1 <- cbind(df1, df2[ (match(df1$ID_STANICE,df2$ID_STANICE) & match(df1$DATUM,df2$DATUM)),])
# names(df.merged.out1)
# length(df.merged.out1$ID_STANICE)
# 
# df.merged.out <- cbind(df.merged.out1, df3[ (match(df.merged.out1$ID_STANICE,df3$ID_STANICE) & match(df.merged.out1$DATUM,df3$DATUM)),])
# names(df.merged.out)
# length(df.merged.out$ID_STANICE)


#check is there NA/Inf value in dataset
any(is.na(df.merged.out$ID_STANICE) | is.infinite(df.merged.out$ID_STANICE))
any(is.na(df.merged.out$DATUM) | is.infinite(df.merged.out$DATUM))

any(is.na(df.merged.out$VREDNOST_POLJA.Mean) | is.infinite(df.merged.out$VREDNOST_POLJA.Mean))
any(is.na(df.merged.out$TEMPERATURA.Mean) | is.infinite(df.merged.out$TEMPERATURA.Mean))
any(is.na(df.merged.out$VLAZNOST.Mean) | is.infinite(df.merged.out$VLAZNOST.Mean))

#uklanjanje eventualnih NA vrednosti
df.merged.out <- df.merged.out[ which( !(is.na(df.merged.out$ID_STANICE))),]
df.merged.out <- df.merged.out[ which( !(is.na(df.merged.out$LON))),]
df.merged.out <- df.merged.out[ which( !(is.na(df.merged.out$LAT))),]
df.merged.out <- df.merged.out[ which( !(is.na(df.merged.out$DATUM))),]
df.merged.out <- df.merged.out[ which( !(is.na(df.merged.out$VREDNOST_POLJA.Mean))),]
df.merged.out <- df.merged.out[ which( !(is.na(df.merged.out$TEMPERATURA.Mean))),]
df.merged.out <- df.merged.out[ which( !(is.na(df.merged.out$VLAZNOST.Mean))),]

names(df.merged.out)
length(df.merged.out$ID_STANICE)

# kreranje SpatialDataFrame
coordinates(df.merged.out) <- c("LON", "LAT")
proj4string(df.merged.out) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#eksport svih podataka oslobodjenih grubih gresaka u .shp
writeOGR(df.merged.out, "objedinjeni-podaci_1dan_agregirano", "stanice-svi-podaci_1dan_agregirano", driver="ESRI Shapefile")


##########################################################################################################################################################

#svih datumi u kojima su se realizovala neka merenja
datumi <-  unique(df.merged.out$DATUM)

#kreiranje novog direktorijuma ukoliko vec ne postoji
subDir<- "ulazni-podaci-sredjeno_1dan"
ifelse(!dir.exists(file.path(data_dir, subDir)), dir.create(file.path(data_dir, subDir)), FALSE)
setwd(file.path(data_dir, subDir))

#AdmTeritory <- readOGR(dsn = "D:\\DataScience\\Projects\\working_dir\\data\\SerbiaTeritory-boundary", "SRB_adm0")
AdmTeritory <- readOGR(dsn = "D:\\DataScience\\Projects\\working_dir\\data\\SerbiaTeritory-boundary", "Grad_Beograd_poligon")

#test datum
#d = datumi[1]

#citanje podataka po datumima
for(d in datumi){

  #izvajanje podataka za odredjeni datum
  tmp.df <- df.merged.out[which(df.merged.out$DATUM == d), ]

  # kreranje SpatialDataFrame
  #coordinates(tmp.df) <- c("LON", "LAT")
  proj4string(tmp.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  #eksport agregiranih podataka u .shp
  writeOGR(tmp.df, as.character(unique(tmp.df$DATUM)), paste(toString(unique(tmp.df$DATUM)), "", sep = ""), driver="ESRI Shapefile")

  ############## IDW interpolacija   ################################################################ 
  
  #definisati prostorni obuhvat grida
  tmp.df@bbox <- AdmTeritory@bbox
  
  names(tmp.df)
  #kreitanje praznog grida
  grd              <- as.data.frame(spsample(tmp.df, "regular", n=500000))
  names(grd)       <- c("LON", "LAT")
  coordinates(grd) <- c("LON", "LAT")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  #dodavanje informacija o projekciji
  proj4string(grd) <- proj4string(tmp.df)
    
  names(tmp.df)
  length(tmp.df$ID_STANICE)
  
  #IDW interpolacija
  
  #Temperatura
  tmp.df.idw.TEMP <- gstat::idw(TEMPERATURA.Mean ~ 1, tmp.df, newdata=grd, idp=1.5)
  tmp.df.idw.raster.TEMP.grid <- raster(tmp.df.idw.TEMP)
  tmp.df.idw.raster.TEMP <- mask(tmp.df.idw.raster.TEMP.grid, AdmTeritory)
  
  #Temperatura
  tmp.df.idw.VLAZ <- gstat::idw(VLAZNOST.Mean ~ 1, tmp.df, newdata=grd, idp=1.5)
  tmp.df.idw.raster.VLAZ.grid <- raster(tmp.df.idw.VLAZ)
  tmp.df.idw.raster.VLAZ <- mask(tmp.df.idw.raster.VLAZ.grid, AdmTeritory)
  
  #Rastojanje od tacaka 
  #tmp.df.DIST <- distance(tmp.df.idw.raster.TEMP)
  
  #Kross-validacija i predikcija
  # power <-  seq(from = 1, to = 2, by = 0.1)
  # neigh <-  seq(from = 3, to = 5, by = 1)
  # results <- list()
  # results.cv <- list()
  # for (i in power) {
  #   for (j in neigh) {
  #     results[[paste0(i,"_",j)]] = idw (VREDNOST_POLJA.Mean ~ 1, tmp.df, grd, nmax = i, idp = j)
  #     #kros-validacija
  #     results.cv[[paste0(i,"_",j)]] = krige.cv (VREDNOST_POLJA.Mean ~ 1, tmp.df, nfold = 5)
  #     }
  # } 

  # #prikaz 1
  # tm_shape(tmp.df.idw.raster) +
  #   tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
  #             title="EM prediktovano") +
  #   tm_shape(tmp.df) + tm_dots(size=0.2) +
  #   tm_legend(legend.outside=TRUE)
  # #prikaz 2
  # spplot(tmp.df.idw["var1.pred"], main = "EM prediktovano")
  
  ###################################################################################################
  
  #eksport rastera interpolovanih prediktora
  writeRaster(tmp.df.idw.raster.TEMP, paste0( as.character(unique(tmp.df$DATUM)), "/", paste(toString(unique(tmp.df$DATUM)), "TEMP", sep = "-") ), format="GTiff", overwrite=TRUE)
  writeRaster(tmp.df.idw.raster.VLAZ, paste0( as.character(unique(tmp.df$DATUM)), "/", paste(toString(unique(tmp.df$DATUM)), "VLAZ", sep = "-") ), format="GTiff", overwrite=TRUE)
  
}
#kraj petlje


#### Svi podaci #####################################################################################

### objedinjeni podaci
d1<- df.clean.EM.subset
d2 <- df.clean.TEMP.subset
d3 <- df.clean.VLAZ.subset

df.out1 <- cbind(d1, d2[ (match(d1$ID_STANICE,d2$ID_STANICE) & match(d1$DATUM,d2$DATUM)),])
names(df.out1)
length(df.out1$ID_STANICE)

df.out <- cbind(df.out1, d3[ (match(df.out1$ID_STANICE,d3$ID_STANICE) & match(df.out1$DATUM,d3$DATUM)),])
#names(df.out)
length(df.out$ID_STANICE)

#uklanjanje eventualnih NA vrednosti
df.out <- df.out[ which( !(is.na(df.out$ID_STANICE))),]
df.out <- df.out[ which( !(is.na(df.out$LON))),]
df.out <- df.out[ which( !(is.na(df.out$LAT))),]
df.out <- df.out[ which( !(is.na(df.out$DATUM))),]
df.out <- df.out[ which( !(is.na(df.out$VREDNOST_POLJA))),]
df.out <- df.out[ which( !(is.na(df.out$TEMPERATURA))),]
df.out <- df.out[ which( !(is.na(df.out$VLAZNOST))),]

#prikaz podataka 
df.out <- df.out[, c("ID_STANICE", "DATUM", "VREDNOST_POLJA", "TEMPERATURA", "VLAZNOST", "LON", "LAT")]
names(df.out)
length(df.out$ID_STANICE)

# kreranje SpatialDataFrame
coordinates(df.out) <- c("LON", "LAT")
proj4string(df.out) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#eksport podataka u data direktorijumu
data_dir <- "D:/DataScience/Projects/working_dir/data"
setwd(data_dir)
#eksport svih podataka oslobodjenih grubih gresaka u .shp
writeOGR(df.out, "objedinjeni-podaci_1dan", "stanice-svi-podaci_1dan", driver="ESRI Shapefile")

#kraj izvrsavanja
proc.time() - ptm
