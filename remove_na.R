library(raster)
library(stringr)

setwd("C:/DATASETS")
path <- "PA_CLIPS_2018"
out_path <- "PA_CLIPS_2018_FINAL"
files <- list.files(path = path, pattern = ".tif$")
#files <- files[22:length(files)]
#files <- files[22:23]
#files <- errores
#errores <- c()
#sin_erroneos <- c()

for(i in 1:length(files)){
  landsat <- brick(paste(path, files[i], sep ="/"))
  na_pixels <- is.na(values(landsat))
  prob_pixels <- c()
  for(j in 1:nrow(na_pixels)){
    if(length(unique(na_pixels[j,])) == 2){
      prob_pixels <- c(prob_pixels, j)
      }
  }
  if(length(prob_pixels != 0)){
    landsat[prob_pixels][which(is.na(landsat[prob_pixels]))] <- 0
    if(!anyNA(landsat[prob_pixels])){
      writeRaster(landsat,
                  filename = paste(out_path, "/", str_replace(files[i], ".tif", ""), "_2000", ".tif", sep=""),
                  format = "GTiff",
                  datatype = dataType(landsat),
                  overwrite = TRUE)
      print(paste(files[i], "fue procesado correctamente!"))
      } else {
        print(paste(files[i], "NO fue procesado correctamente!"))
        errores <- c(errores, files[i])
        }
  } else {
    writeRaster(landsat,
                filename = paste(out_path, "/", str_replace(files[i], ".tif", ""), "_2000", ".tif", sep=""),
                format = "GTiff",
                datatype = dataType(landsat),
                overwrite = TRUE)
    sin_erroneos <- c(sin_erroneos, files[i])
    print(paste(files[i], "NO tiene pixeles erroneos"))
  }
  rm(landsat, na_pixels, prob_pixels)
  gc()
}

###DIVIDIR EN MITADES LOS RASTER QUE NO SE PUDDIERONN PROCESAR POR SU TAMANO
recortes <- c()
for(i in 1: length(errores)){
  landsat <- brick(paste(path, errores[i], sep ="/"))
  izq <- extent(landsat, 1, nrow(landsat), 1, ncol(landsat)/2)
  der <- extent(landsat, 1, nrow(landsat), (ncol(landsat)/2), ncol(landsat))
  I <- crop(landsat, izq)
  II <- crop(landsat, der)
  writeRaster(I,
              filename = paste(path, "/", str_replace(errores[i], ".tif", ""), "split1", ".tif", sep=""),
              format = "GTiff",
              datatype = dataType(landsat),
              overwrite = TRUE)
  writeRaster(II,
              filename = paste(path, "/", str_replace(errores[i], ".tif", ""), "split2", ".tif", sep=""),
              format = "GTiff",
              datatype = dataType(landsat),
              overwrite = TRUE)
  recortes <- c(recortes,
                paste(str_replace(errores[i], ".tif", ""), "split1", ".tif", sep=""),
                paste(str_replace(errores[i], ".tif", ""), "split2", ".tif", sep=""))
  rm(landsat, I, II)
  gc()
}

#PROCESAR LAS MITADES
setwd("C:/DATASETS")
path <- "SPLITS_2018"
out_path <- "SPLITS_2018"
files <- list.files(path = path, pattern = ".tif$")
errores <- c()
sin_erroneos <- c()

for(i in 1:length(files)){
  landsat <- brick(paste(path, files[i], sep ="/"))
  na_pixels <- is.na(values(landsat))
  prob_pixels <- c()
  for(j in 1:nrow(na_pixels)){
    if(length(unique(na_pixels[j,])) == 2){
      prob_pixels <- c(prob_pixels, j)
    }
  }
  if(length(prob_pixels != 0)){
    landsat[prob_pixels][which(is.na(landsat[prob_pixels]))] <- 0
    if(!anyNA(landsat[prob_pixels])){
      writeRaster(landsat,
                  filename = paste(out_path, "/", str_replace(files[i], ".tif", ""), "_2000", ".tif", sep=""),
                  format = "GTiff",
                  datatype = dataType(landsat),
                  overwrite = TRUE)
      print(paste(files[i], "fue procesado correctamente!"))
    } else {
      print(paste(files[i], "NO fue procesado correctamente!"))
      errores <- c(errores, files[i])
    }
  } else {
    writeRaster(landsat,
                filename = paste(out_path, "/", str_replace(files[i], ".tif", ""), "_2000", ".tif", sep=""),
                format = "GTiff",
                datatype = dataType(landsat),
                overwrite = TRUE)
    sin_erroneos <- c(sin_erroneos, files[i])
    print(paste(files[i], "NO tiene pixeles erroneos"))
  }
  rm(landsat, na_pixels, prob_pixels)
  gc()
}

#Unir nuevamente las mitades
setwd("C:/DATASETS")
path <- "SPLITS_2018"
out_path <- "PA_CLIPS_2018_FINAL"
files <- list.files(path = path, pattern = ".tif$")
for(i in seq(1, length(files), 2)){
  x <- brick(paste(path, files[i], sep="/"))
  y <- brick(paste(path, files[i+1], sep="/"))
  z <- mosaic(x, y, fun=max)
  writeRaster(z,
              filename = paste(out_path, "/", str_replace(files[i], "split1_2018.tif", ""), "_2018", ".tif", sep=""),
              format = "GTiff",
              datatype = "INT2S",
              overwrite = TRUE)
  print(paste(str_replace(files[i], "split1_2018.tif", ""), "ha sido procesado!"))
  rm(x, y, z)
  gc()
}



###DIVIDIR EN 4 LA SERRANIA DEL CHIRIBIQUETE
setwd("C:/DATASETS")
path <- "PA_CLIPS_2018"
out_path <- "SPLITS_2018"
recortes <- c()
errores <- c("SERRANIA_DE_CHIRIBIQUETE.tif")
for(i in 1: length(errores)){
  landsat <- brick(paste(path, errores[i], sep ="/"))
  sup_izq <- extent(landsat, 1, nrow(landsat)/2, 1, ncol(landsat)/2)
  inf_izq <- extent(landsat, nrow(landsat)/2, nrow(landsat), 1, ncol(landsat)/2)
  sup_der <- extent(landsat, 1, nrow(landsat)/2, ncol(landsat)/2, ncol(landsat))
  inf_der <- extent(landsat, nrow(landsat)/2, nrow(landsat), ncol(landsat)/2, ncol(landsat))
  I <- crop(landsat, sup_izq)
  II <- crop(landsat, sup_der)
  III <- crop(landsat, inf_izq)
  IV <- crop(landsat, inf_der)
  writeRaster(I,
              filename = paste(path, "/", str_replace(errores[i], ".tif", ""), "split1", ".tif", sep=""),
              format = "GTiff",
              datatype = dataType(landsat),
              overwrite = TRUE)
  writeRaster(II,
              filename = paste(path, "/", str_replace(errores[i], ".tif", ""), "split2", ".tif", sep=""),
              format = "GTiff",
              datatype = dataType(landsat),
              overwrite = TRUE)
  writeRaster(III,
              filename = paste(path, "/", str_replace(errores[i], ".tif", ""), "split3", ".tif", sep=""),
              format = "GTiff",
              datatype = dataType(landsat),
              overwrite = TRUE)
  writeRaster(IV,
              filename = paste(path, "/", str_replace(errores[i], ".tif", ""), "split4", ".tif", sep=""),
              format = "GTiff",
              datatype = dataType(landsat),
              overwrite = TRUE)
  recortes <- c(recortes,
                paste(str_replace(errores[i], ".tif", ""), "split1", ".tif", sep=""),
                paste(str_replace(errores[i], ".tif", ""), "split2", ".tif", sep=""),
                paste(str_replace(errores[i], ".tif", ""), "split3", ".tif", sep=""),
                paste(str_replace(errores[i], ".tif", ""), "split4", ".tif", sep=""))
  rm(landsat, I, II, III, IV)
  gc()
}

#PROCESAR LOS CUARTOS
setwd("C:/DATASETS")
path <- "PA_CLIPS_2000"
out_path <- "SPLITS_2000"
files <- recortes
errores <- c()
sin_erroneos <- c()

for(i in 1:length(files)){
  landsat <- brick(paste(path, files[i], sep ="/"))
  na_pixels <- is.na(values(landsat))
  prob_pixels <- c()
  for(j in 1:nrow(na_pixels)){
    if(length(unique(na_pixels[j,])) == 2){
      prob_pixels <- c(prob_pixels, j)
    }
  }
  if(length(prob_pixels != 0)){
    landsat[prob_pixels][which(is.na(landsat[prob_pixels]))] <- 0
    if(!anyNA(landsat[prob_pixels])){
      writeRaster(landsat,
                  filename = paste(out_path, "/", str_replace(files[i], ".tif", ""), "_2000", ".tif", sep=""),
                  format = "GTiff",
                  datatype = dataType(landsat),
                  overwrite = TRUE)
      print(paste(files[i], "fue procesado correctamente!"))
    } else {
      print(paste(files[i], "NO fue procesado correctamente!"))
      errores <- c(errores, files[i])
    }
  } else {
    writeRaster(landsat,
                filename = paste(out_path, "/", str_replace(files[i], ".tif", ""), "_2000", ".tif", sep=""),
                format = "GTiff",
                datatype = dataType(landsat),
                overwrite = TRUE)
    sin_erroneos <- c(sin_erroneos, files[i])
    print(paste(files[i], "NO tiene pixeles erroneos"))
  }
  rm(landsat, na_pixels, prob_pixels)
  gc()
}

#Unir nuevamente las mitades
setwd("C:/DATASETS")
path <- "SPLITS_2018"
out_path <- "PA_CLIPS_2018_FINAL"
files <- list.files(path = path, pattern = ".tif$")
for(i in seq(1, length(files), 2)){
  x <- brick(paste(path, files[i], sep="/"))
  y <- brick(paste(path, files[i+1], sep="/"))
  z <- mosaic(x, y, fun=max)
  writeRaster(z,
              filename = paste(out_path, "/", str_replace(files[i], "split1_2018.tif", ""), "_2018", ".tif", sep=""),
              format = "GTiff",
              datatype = "INT2S",
              overwrite = TRUE)
  print(paste(str_replace(files[i], "split1_2018.tif", ""), "ha sido procesado!"))
  rm(x, y, z)
  gc()
}