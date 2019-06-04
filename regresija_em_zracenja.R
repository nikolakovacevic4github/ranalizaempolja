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

### podesavanje radnog direktorijuma 
project_dir <- "D:/DataScience/Projects/working_dir/script"
setwd(project_dir);

### postavljanje direktorijuma sa podacima na radni direktorijum 
data_dir <- "D:/DataScience/Projects/working_dir/data/ulazni-podaci-sredjeno"
setwd(data_dir)

#lista svih diretkorijuma sa podacima
#dir_list <- list.dirs(data_dir)

#listanje svih shp fajlova unutar svi podirektorujma
files_list <- list.files(pattern = "\\.shp$", recursive = TRUE)

podaci_regresije <- list()
for(file in files_list){
  
  #citanje podataka po datumima
  vrednost_em <- readOGR(dsn = file)
  names(vrednost_em)
   
  # Objekat fitovanja - lm() 
  lm.Regresioni_model <- lm(VREDNOS ~ TEMPERA + VLAZNOS, vrednost_em)
  #summary(lm.Regresioni_model)
  #residuals(lm.Regresioni_model)
  
  dir_name <- dirname(file)
  #setwd(dir_name)
  

  #sacuvati podatke u odgovarajuci direktorijum
  if(grepl("noc", file)){
    key = toString(paste(c(dir_name, "noc"), sep="-"))
    podaci_regresije[[key]] <- lm.Regresioni_model
  # writeOutput_to_file(lm.Regresioni_model, "noc.txt")
  }
  
  if(grepl("obdanica", file)){
    key = toString(paste(c(dir_name, "obdanica"), sep="-"))
    podaci_regresije[[key]] <- lm.Regresioni_model
  #  writeOutput_to_file(lm.Regresioni_model, "obdanica.txt")
  }

  #cuvanje regresionog modela
  #podaci_regresije[[file]] <- lm.Regresioni_model
  
  #setwd(data_dir)
}

### racunanje prediktora --kriging predikcijom

#1. temperatura



#2. vlaznost


### predikcija EM polja



### eksport rastera predikcije











