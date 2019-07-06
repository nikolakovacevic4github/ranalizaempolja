# Kreirao: Milic Stevan
# Datum: 04.6.2019.

# Opis: Analiza merenja i prostrna regresija

rm(list=ls(all=TRUE))

cat("\014")

library(sqldf)
library(data.table)
library(tidyr)
library(sp)
library(rgdal)

library(rsample)      # data splitting
library(ranger)       # a faster implementation of randomForest
library(randomForest) # basic implementation
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

#pocetak izvrsavanja
ptm <- proc.time()


#putanja do fajla sa vektorskim podacima
agregirani_podaci.putanja <- "D:/DataScience/Projects/working_dir/data/objedinjeni-podaci_1dan_agregirano/stanice-svi-podaci_1dan_agregirano.shp"

#putanja do direktorijuma sa poddirektorijumima sa ulaznim podacima
prediktori.putanja <- "D:/DataScience/Projects/working_dir/data/ulazni-podaci-sredjeno_1dan"

#citanje podataka sa merenjima
ulazni_podaci <- readOGR(dsn = agregirani_podaci.putanja)
str(ulazni_podaci)
names(ulazni_podaci)

############ MODELIRANJE ############################################################################

# rmse = function(actual, predicted) {
#   sqrt(mean((actual - predicted) ^ 2))
# }

#lm.Regresioni_model <- lm(VREDNOS ~ TEMPERA + VLAZNOS + DTM, vrednost_em)

#lm.Regresioni_model <- lm(VREDNOST_P ~ TEMPERATUR + VLAZNOST_M, ulazni_podaci)
#summary(lm.Regresioni_model)
# residuals(lm.Regresioni_model)

#listanje svih direktorijuma sa podacima
dirs <- list.dirs(prediktori.putanja, recursive = TRUE)

#d <- dirs[1]

for(i in 2:length(dirs)){
  
  d = dirs[i]
  
  #Ucitavanje prediktora
  # DTM
  
  #Temperatura
  raster.TEMP <- list.files(d, pattern = '\\TEMP.tif$')
  r1  <- paste(d, raster.TEMP, sep = "/")
  str(r1)
  
  #Vlaznost
  raster.VLAZ <- list.files(d, pattern = '\\VLAZ.tif$')
  r2  <- paste(d, raster.VLAZ, sep = "/")
  str(r2)
  
  Prediktori.ov <- readGDAL(r1)
  names(Prediktori.ov) <- c("TEMPERATUR");

  Prediktori.ov$VLAZNOST_M<- readGDAL(r2)$band1
  #names(Prediktori.ov) <- c("VLAZNOST_M");
  
  
  #kreiranje RasterLayer objekta
  prediktori <- stack(Prediktori.ov)
  #class(prediktori) #klasa "raster"
  
  names(prediktori)
  
  # Objekat fitovanja - glm()
  m1 <- glm(VREDNOST_P ~ TEMPERATUR + VLAZNOST_M, data = ulazni_podaci@data)
  summary(m1)
  
  predikcija.raster <- predict(prediktori, m1)
  #class(predikcija.raster)
  
  #pisanje rastera
  writeRaster(predikcija.raster,filename = paste(basename(file.path(d)), "Predikcija_EM", sep = "-"), format="GTiff", overwrite=TRUE)
  
}
#kraj petlje


#kraj izvrsavanja
proc.time() - ptm



