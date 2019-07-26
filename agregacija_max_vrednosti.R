# Opis: Racinanje maximuma po lokaciji za celokupan period predikcije

rm(list=ls(all=TRUE))

cat("\014")

#install.packages("raster")
library(raster)
#install.packages("rgdal")
library(rgdal)
#install.packages("sp")
library(sp)


#pocetak izvrsavanja
ptm <- proc.time()

### podesavanje radnog direktorijuma
project_dir <- "D:/DataScience/Projects/working_dir/script"
setwd(project_dir);

### postavljanje direktorijuma sa podacima na radni direktorijum 
data_dir <- "D:/DataScience/Projects/working_dir/data/PREDIKCIJA"
setwd(data_dir)

### lista svih .tiff fajlava (predikcija)
tiffFiles <- unlist(lapply(data_dir, function(dir) list.files(path = dir, pattern = '\\.tif$')))

tiffFilesRasters <- lapply(tiffFiles, function(r) raster(r))

r <- tiffFilesRasters[[1]]

e<-r@extent
n_rows <- r@nrows
n_cols <- r@ncols

rastersResampled <- list()

for(i in 1:length(tiffFilesRasters)){
  s<-raster(e, nrows=n_rows, ncols=n_cols, crs=r@crs)
  r1<-resample(tiffFilesRasters[[i]], s, method="ngb")
  rastersResampled[i] <- r1
}

#kreiranje raster steka (nad kojim ce se primeniti tackaste prostorne operacije) 
rasterStack <- raster::stack(rastersResampled)

#nlayers(rasterStack)

#raster koji na lokaciji piksela (x, y) sadrzi maksimalnu vrednost svih rastera za istu poziciju (x, y)
maxRasterStack <- max(rasterStack, na.rm = TRUE)
#raster koji na lokaciji piksela (x, y) sadrzi prosecnu vrednost svih rastera za istu poziciju (x, y)
avgRasterStack <- mean(rasterStack, na.rm = TRUE)
#raster koji na lokaciji piksela (x, y) sadrzi prosecnu vrednost svih rastera za istu poziciju (x, y)
minRasterStack <- min(rasterStack, na.rm = TRUE)

#podrucije od interesa
AdmTeritory <- readOGR(dsn = "D:\\DataScience\\Projects\\working_dir\\data\\SerbiaTeritory-boundary", "Grad_Beograd_poligon")

###TEST ###
#maxRasterStack <- readGDAL("Max_EM_vrednost.tif")
#avgRasterStack <- readGDAL("Avg_EM_vrednost.tif")
#######

#eliminisanje oblasti koja se ne prikazuje 
maxRasterStack_croped <- mask(maxRasterStack, AdmTeritory)
avgRasterStack_croped <- mask(avgRasterStack, AdmTeritory)
minRasterStack_croped <- mask(minRasterStack, AdmTeritory)

#cuvanje rastera sa max vrednostima
writeRaster(maxRasterStack_croped, filename = "Max_EM_vrednost ", format="GTiff", overwrite=TRUE)
writeRaster(avgRasterStack_croped, filename = "Avg_EM_vrednost ", format="GTiff", overwrite=TRUE)
writeRaster(minRasterStack_croped, filename = "Min_EM_vrednost ", format="GTiff", overwrite=TRUE)

#kraj izvrsavanja
proc.time() - ptm


