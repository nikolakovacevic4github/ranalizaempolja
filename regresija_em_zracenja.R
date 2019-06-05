# Kreirao: Milic Stevan
# Datum: 04.6.2019.

# Opis: Analiza merenja i prostrna regresija

rm(list=ls(all=TRUE))

cat("\014")

library(sqldf)
library(data.table)
library(sp)
library(rgdal)
library(tidyr)
library(data.table)
library(gstat)

### podesavanje radnog direktorijuma 
project_dir <- "D:/DataScience/Projects/working_dir/script"
setwd(project_dir);

root_data_dir <- "D:/DataScience/Projects/working_dir/data/"

### postavljanje direktorijuma sa podacima na radni direktorijum 
data_dir <- "D:/DataScience/Projects/working_dir/data/ulazni-podaci-sredjeno"
setwd(data_dir)

#lista svih diretkorijuma sa podacima
#dir_list <- list.dirs(data_dir)

#listanje svih shp fajlova unutar svi podirektorujma
files_list <- list.files(pattern = "\\.shp$", recursive = TRUE)

podaci_regresije <- list()

#file = files_list[1]

for(file in files_list){
  
  #citanje podataka po datumima
  vrednost_em <- readOGR(dsn = file)
  names(vrednost_em)
  
### KRIGING ##############################################################################################################
  
  ### racunanje prediktora
  setwd(root_data_dir)
  
  #0. DTM
  dtm_grid <- readGDAL("D:/DataScience/Projects/working_dir/data/prediktori/dtm/dtm_wgs84.tif")
  grid <- as(dtm_grid,'SpatialPixelsDataFrame')
  names(grid)<- c("DEM") 
  
  #1. Temperatura
  
  
  #2. Vlaznost
  
  
  # Prostorni prekolom tacaka merenja i prediktora
  vrednost_em.ov <- over(vrednost_em, grid)
  vrednost_em@data<- cbind(vrednost_em@data, vrednost_em.ov)
  str(vrednost_em)
   
  # Objekat fitovanja - lm() 
  lm.Regresioni_model <- lm(VREDNOS ~ TEMPERA + VLAZNOS, vrednost_em)
  #summary(lm.Regresioni_model)
  #residuals(lm.Regresioni_model)
  
  # Eksperimentalni poluvariograma
  vgm <- variogram(residuals(lm.Regresioni_model) ~ 1, vrednost_em)
  #plot(vgm, plot.numbers = TRUE, col="red", main="Eksperimentalni poluvariogram EM zracenja - reziduali")
  
  vm <- vgm(nugget=0.001, model="Exp", range=sqrt(diff(vrednost_em@bbox["coords.x1",])^2 + diff(vrednost_em@bbox["coords.x2",])^2), 
                psill=var(residuals(lm.Regresioni_model)))
  
  # Uklapanje eksperimentalnog poluvariograma u empirijski model 
  vgm.fit <- fit.variogram(vgm, model=vm)
  vgm.fit
  
  print(plot(vgm, vgm.fit, pch = 19, pl = TRUE,  col = "blue", main = "Poluvariogram"))
  
##########################################################################################################################
  
  dir_name <- dirname(file)
  #setwd(dir_name)

  #sacuvati podatke
  if(grepl("noc", file)){
    key = toString(paste(c(dir_name, "noc"), sep="-"))
    podaci_regresije[[key]] <- lm.Regresioni_model
  }
  
  if(grepl("obdanica", file)){
    key = toString(paste(c(dir_name, "obdanica"), sep="-"))
    podaci_regresije[[key]] <- lm.Regresioni_model
  }
  
  ### predikcija EM polja
  
  # Regresioni kriging - predikcija
  #krige.pred <- krige(VREDNOS ~ TEMPERA + VLAZNOS, locations=vrednost_em, newdata=grid, model=vgm.fit, nmax=30)
  krige.pred <- krige(VREDNOS ~ DEM, locations=vrednost_em, newdata=grid, model=vgm.fit, nmax=30)
  
  setwd(data_dir)
  dir <- paste(getwd(), dir_name, sep="/")
  setwd(dir)
  
  # Eksport rastera predikcije 
  if(grepl("noc", file)){
    writeGDAL(krige.pred["var1.pred"],paste(getwd(), "noc_pred.asc", sep="/"), "AAIGrid")
  }
  
  if(grepl("obdanica", file)){
    writeGDAL(krige.pred["var1.pred"],paste(getwd(), "obdanica_pred.asc", sep="/"), "AAIGrid")
  }
  
  setwd(data_dir)
  
}













