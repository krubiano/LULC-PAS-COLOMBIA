library(raster)
require(rgdal)
library(gdalUtils)
library(sf)
library(geodist)
#library(rgrass7)
library(writexl)
library(stringr)
library(RPyGeo)


# -------------------------------------------------------------------------
# AJUSTE DEL TAMANO DEL RASTER PARA CORREGIR EL AUMENTO DE UN PIXEL AL APLICAR MAJORITY FILTER

parks <- sort(tools::file_path_sans_ext(list.files(path = "training_samples", pattern = "*.shp")))
years <- c("2000", "2018")

for (park in parks){
  for (year in years){
    maj_class <- readRDS(paste0("majority_classifications/", park, "_", year))

    maj_class_filter <- readRDS(paste0("filtered_classifications/", park, "_", year))
    maj_class_filter[is.na(maj_class)] <- NA
    saveRDS(maj_class_filter, file = paste0("filtered_classifications2/", park, "_" , year))
    writeRaster(maj_class_filter,
                filename = paste0("filtered_classifications2/", park, "_", year, ".tif"),
                format = "GTiff",
                datatype='INT1U',
                overwrite=TRUE)

    maj_class_filter_mask <- readRDS(paste0("masked_classifications/", park, "_", year, ".RDS"))
    maj_class_filter_mask[is.na(maj_class)] <- NA
    saveRDS(maj_class_filter_mask, file = paste0("masked_classifications2/", park, "_", year))

    writeRaster(maj_class_filter_mask,
                filename = paste0("masked_classifications2/", park, "_", year, ".tif"),
                format = "GTiff",
                datatype='INT1U',
                overwrite=TRUE)
  }
}


# -------------------------------------------------------------------------
# AGREGACION DE LAS CLASIFICACIONES 2001 Y 2018 EN 4 CLASES DE CAMBIO

parks <- sort(tools::file_path_sans_ext(list.files(path = "training_samples", pattern = "*.shp")))
years <- c("2000", "2018")

aggregation_matrix <- matrix(c(NA, NA,
                               1, 4,
                               2, 1,
                               3, 2,
                               4, 2,
                               5, 2,
                               6, 3,
                               7, 3,
                               8, 3,
                               9, 4,
                               12, 4,
                               13, 3),
                             ncol = 2, byrow = TRUE)

for (park in parks){
  for (year in years){
    maj_class_filter_mask <- readRDS(paste0("masked_classifications/", park, "_", year, ".RDS"))
    aggregated <- reclassify(maj_class_filter_mask, aggregation_matrix, include.lowest = TRUE)

    saveRDS(aggregated, file = paste0("aggregated_classifications/", park, "_", year))

    writeRaster(aggregated,
            filename = paste0("aggregated_classifications/", park, "_", year, ".tif"),
            format = "GTiff",
            datatype='INT1U',
            overwrite=TRUE)
  }
}


# -------------------------------------------------------------------------
# GENERAR MAPAS DE CAMBIO

parks <- sort(tools::file_path_sans_ext(list.files(path = "training_samples", pattern = "*.shp")))

for (park in parks){
  lc2000 <- readRDS(paste0("aggregated_classifications/", park, "_2000"))
  lc2018 <- readRDS(paste0("aggregated_classifications/", park, "_2018"))
  change <- raster(lc2000)

  change[is.na(lc2000) | is.na(lc2018)] <- NA
  change[lc2000 == 1 & lc2018 == 1] <- 1
  change[lc2000 == 1 & lc2018 == 2] <- 2
  change[lc2000 == 1 & lc2018 == 3] <- 3
  change[lc2000 == 1 & lc2018 == 4] <- 3
  change[lc2000 == 2 & lc2018 == 1] <- 4
  change[lc2000 == 3 & lc2018 == 1] <- 4
  change[lc2000 == 4 & lc2018 == 1] <- 4
  change[lc2000 == 2 & lc2018 == 2] <- 5
  change[lc2000 == 2 & lc2018 == 3] <- 6
  change[lc2000 == 2 & lc2018 == 4] <- 6
  change[lc2000 == 3 & lc2018 == 2] <- 7
  change[lc2000 == 4 & lc2018 == 2] <- 7
  change[lc2000 == 3 & lc2018 == 3] <- 8
  change[lc2000 == 4 & lc2018 == 4] <- 9
  change[lc2000 == 3 & lc2018 == 4] <- 10
  change[lc2000 == 4 & lc2018 == 3] <- 10

  saveRDS(change, file = paste0("lcc/", park, ".RDS"))

  writeRaster(change,
              filename = paste0("lcc/", park, ".tif"),
              format = "GTiff",
              datatype='INT1U',
              overwrite=TRUE)

  rm(list = setdiff(ls(), c("parks", "years", "park", "year", "aggregation_matrix")))
  gc()
}


# -------------------------------------------------------------------------
## SE GENERA EL BUFFER DE LAS CLASES DE CAMBIO PARA ESTIMACION DE ESTRATOS Y CREACION DE MUESTRAS DE VALIDACION
# CREAR MOSAICO
parks <- sort(list.files(path = "lcc", pattern = ".tif$", full.names= TRUE))
template <- raster()
writeRaster(template, file="lcc_mosaics/lcc_mosaic.tif", format="GTiff", overwrite = TRUE)
system.time(gdalUtils::mosaic_rasters(gdalfile = parks, dst_dataset = file.path("lcc_mosaics/lcc_mosaic.tif"), of = "GTiff", verbose = TRUE))


# -------------------------------------------------------------------------
# GENERAR BUFFER DE CLASES DE CAMBIO PARA TODO EL PAIS

rast <- raster("lcc_mosaics/lcc_mosaic.tif")
rcl <- as.matrix(data.frame(class = 1:10, new_class = c(NA, NA, 1, NA, NA, NA, NA, NA, NA, NA)))
floss <- reclassify(rast, rcl)
saveRDS(floss, "lcc_mosaics/floss.RDS")
writeRaster(floss, file="lcc_mosaics/floss.tif", format="GTiff", overwrite = TRUE)
floss <- readRDS("lcc_mosaics/floss.RDS")

rcl2 <- as.matrix(data.frame(class = 1:10, new_class = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA)))
estf <- reclassify(rast, rcl2)
saveRDS(estf, "lcc_mosaics/estf.RDS")
writeRaster(estf, file="lcc_mosaics/estf.tif", format="GTiff", overwrite = TRUE)
estf <- readRDS("lcc_mosaics/estf.RDS")


#El buffer se genero en QGIS con la herramienta proximidad que devuelve el anillo del buffer
flossb <- raster("lcc_mosaics/lcc_mosaic_buffer.tif")

#se extraen las zonas del anillo que corresponden a bosque
stacked <- stack(flossb, rast)
rc <- function(x1, x2){
  ifelse((x1 == 1) & (x2 == 1), 1, NA)
}
beginCluster()
system.time(final_buff <- clusterR(stacked, overlay, args = list(fun = rc)))
endCluster()
saveRDS(final_buff, file = "lcc_mosaics/lcc_mosaic_buffer_forest.RDS")
writeRaster(final_buff,
            filename = "lcc_mosaics/lcc_mosaic_buffer_forest.tif",
            format = "GTiff",
            datatype='INT1U',
            overwrite=TRUE)

#se reemplazan en el raster LCC las zonas de bosque que corresponden al buffer
#el buffer se asigna a la clase 11
stacked1 <- stack(rast, final_buff)
rc1 <- function(x1, x2){
  ifelse((x1 == 1) & (is.na(x2)), x1, ifelse((x1 ==1) & (!is.na(x2)), 11, x1))
}
beginCluster()
system.time(final <- clusterR(stacked1, overlay, args = list(fun = rc1)))
endCluster()
saveRDS(final, file = "lcc_mosaics/lcc_mosaic_with_buffer.RDS")
writeRaster(final,
            filename = "lcc_mosaics/lcc_mosaic_with_buffer.tif",
            format = "GTiff",
            datatype='INT1U',
            overwrite=TRUE)


# -------------------------------------------------------------------------
#EXTRAER LA FRECUENCIA DE CADA CLASE PARA LAS AREAS DE ESTUDIO EN TODO EL PAIS

system.time(frecuencia <- freq(final))
frecuencia <- as.data.frame(frecuencia)
saveRDS(frecuencia, file = "lcc_proportions/lcc_complete_proportions.RDS")
write_xlsx(frecuencia, "lcc_proportions/lcc_complete_proportions.xlsx")


# -------------------------------------------------------------------------
#GENERAR MUESTRAS ALEATORIAS PARA VALIDACION

final_buff <- raster("lcc_mosaics/lcc_mosaic_with_buffer.tif")
#se generan 500 muestras para cada clase
set.seed(100)
system.time(samples <- sampleStratified(final_buff, size = 500, na.rm = TRUE, xy = TRUE, sp = TRUE))
samples <- st_as_sf(samples)
st_write(subsamples, "validation/samples.shp")

#se hace un subsampling para obtener el 150% de las muestras requeridas en la metodolgia
#para la clase 1 se seleccionan 500
sizes <- c("1" = 499, "2" = 150, "3" = 150, "4" = 150, "5" = 200, "6" = 150, "7" = 150, "8" = 200,
           "9" = 100, "10" = 100, "11" = 100)
subsamples <- samples
for (i in 1:length(sizes)) {
  a <- which(subsamples$lcc_mosaic_with_buffer==i)
  b <- sample(a, as.integer(sizes[i]))
  subsamples <- subsamples[-setdiff(a, b),]
}
st_write(subsamples, "validation/subsamples.shp")

#ajustar los datos a la estructura del survey de collect para que sirvan como grid de collect earth
subsamples <- subsamples[-1]
names(subsamples)[1] <- "XCoordinate"
names(subsamples)[2] <- "YCoordinate"
names(subsamples)[3] <- "class"
subsamples$id <- seq.int(nrow(subsamples))
sample_count <- c()
for (i in 1:length(sizes)) {
  sample_count <- c(sample_count, seq.int(1, sizes[i]))
}
subsamples$class_sample_number <- sample_count
subsamples_bc <- subsamples #Guarda un backup de las muestras antes de volverlas df
st_write(subsamples_bc, "validation/subsamples_bc.shp")

subsamples <- as.data.frame(subsamples)
subsamples <- subsamples[-4]
subsamples <- subsamples[,c(4, 2, 1, 3, 5)]
write.csv(subsamples, "validation/collect_earth_grid.csv", row.names = FALSE)


# MUESTRAS ADICIONALES (ALGUNAS MUESTRAS SE DESCARTARON SEGUN PROTOCOLO Y SE NECESITARON MAS)
clases <- c(4, 6, 7)
subsamples2 <- samples[samples$lcc_mosaic_with_buffer %in% c("4", "6", "7"),]
for (i in clases) {
  a <- which(subsamples2$lcc_mosaic_with_buffer==i)
  b <- sample(a, "150")
  subsamples2 <- subsamples2[-setdiff(a, b),]
}

subsamples2 <- subsamples2[-1]
names(subsamples2)[1] <- "XCoordinate"
names(subsamples2)[2] <- "YCoordinate"
names(subsamples2)[3] <- "class"
subsamples2 <- as.data.frame(subsamples2)
subsamples2 <- subsamples2[-4]
subsamples <- subsamples[-c(1,5)]
subsamples2 <- subsamples2[,c(2,1,3)]
samples_not_common <- setdiff(subsamples[subsamples$class %in% c(4, 6, 7),], subsamples2)
samples_not_common$id <- seq.int(nrow(samples_not_common))
sample_count <- c()
for (i in clases) {
  sample_count <- c(sample_count, seq.int(1, length(samples_not_common$class[samples_not_common$class == i])))
}
samples_not_common$class_sample_number <- sample_count
samples_not_common <- samples_not_common[,c(4, 2, 1, 3, 5)]
samples_not_common <- samples_not_common[,c(1, 3, 2, 4, 5)]
samples_not_common$id <- paste0(samples_not_common$id, "a")
write.csv(samples_not_common, "validation/collect_earth_grid_2.csv", row.names = FALSE)


# -------------------------------------------------------------------------
# EXTRACCION DE LAS AREAS Y PROPORCIONES DE LAS CLASES DE CAMBIO POR PARQUE
# CALCULA LAS AREAS PARA LAS PA Y EL BUFFER DE 20KM

parks <- sort(list.files(path = "lcc", pattern = "*.RDS"))
# parks <- parks[1:3]
df <- data.frame(matrix(ncol = 12, nrow = 0))
names(df) <- c("protected_area", seq(1:10), "total")

for(park in parks) {

  raster <- readRDS(paste0("lcc/", park))
  df[nrow(df)+1,1] <- str_remove(park, ".RDS")
  df2 <- as.data.frame(table(values(raster)))
  # df2 <- as.data.frame(freq(raster))
  # df2$value[nrow(df2)] <- 20
  for(class in df2$Var1) {
    df[nrow(df), as.integer(class)+1] <- df2$Freq[df2$Var1 == as.integer(class)]
  }
  # df[nrow(df), 2:12] <- table(values(raster))
  df[nrow(df), "total"] <- sum(table(values(raster)))
  print(paste(str_remove(park, ".RDS"), "Ha sido procesado!"))
}

df[is.na(df)] <- 0

saveRDS(df, "lcc_statistics/paa_buffer_total.RDS")
write_xlsx(df, "lcc_statistics/paa_buffer_total.xlsx")



# -------------------------------------------------------------------------
# #EXTRACCIÓN DE LAS AREAS Y PROPORCIONES DE LAS CLASES DE CAMBIO POR PARQUE Y GLOBAL
# #CALCULA LAS AREAS PARA LOS DIFERENTES RANGOS DE BUFFER DE CADA PA

parks <- sort(list.files(path = "lcc", pattern = "*.RDS"))
parks_sel <- readOGR("boundaries_shapes/SPNN_SEL.shp")
parks_index <- as.data.frame(parks_sel[, c(1, 4)])
names(parks_index) <- (c("id", "protected_area"))
parks_index[2] <- str_replace_all(parks_index$protected_area, " ", "_")
parks_index[2] <- str_replace_all(parks_index$protected_area, "__", "_")
parks_index[2] <- toupper(parks_index$protected_area)

#boundaries <- readOGR("boundaries_shapes/BUFFER_RINGS_M.shp")

# arcpy <- rpygeo_build_env(x64 = TRUE,
#                           workspace = getwd(),
#                           overwrite = TRUE,
#                           extensions = "Spatial")

#arcpy$Merge_management(inputs = c("/boundaries_shapes/BUFFER_RINGS_M.shp", "/boundaries_shapes/SPNN_SEL.shp"),
#                                      output = "boundaries_shapes/PARKS_AND_RINGS.shp")

parks_rings <- readOGR("boundaries_shapes/PARKS_AND_RINGS.shp")
parks_rings$type_ring[is.na(parks_rings$type_ring)] <- 0
parks_rings$gid[parks_rings$gid == 0] <- parks_rings$OBJECTID_1[parks_rings$OBJECTID_1  != 0]

buffers <- unique(parks_rings$type_ring)

for(buffer in buffers) {
  df <- data.frame(matrix(ncol = 13, nrow = 0))
  names(df) <- c("protected_area", "id", seq(1:10), "total")
  buffer_boundary <- subset(parks_rings, type_ring == buffer)
  
  for(park in parks) {
    raster <- readRDS(paste0("lcc/", park))
    park_boundary <- subset(buffer_boundary, gid == parks_index$id[parks_index$protected_area == str_remove(park, ".RDS")])
    raster <- mask(raster, park_boundary)
    df[nrow(df)+1,1] <- str_remove(park, ".RDS")
    df[nrow(df),2] <- parks_index$id[parks_index$protected_area == str_remove(park, ".RDS")]
    df2 <- as.data.frame(table(values(raster)))
    # df2 <- as.data.frame(freq(raster))
    # df2$value[nrow(df2)] <- 20
    
    for(class in df2$Var1) {
      df[nrow(df), as.integer(class)+2] <- df2$Freq[df2$Var1 == as.integer(class)]
    }
    # df[nrow(df), 2:12] <- table(values(raster))
    df[nrow(df), "total"] <- sum(table(values(raster)))
    print(paste(str_remove(park, ".RDS"), "Ha sido procesado!"))
  }
  df[is.na(df)] <- 0
  df[3:13] <- (df[3:13]*900)/10000
  df[1] <- str_replace_all(df$protected_area, "_", " ")
  saveRDS(df, paste0("lcc_statistics_end/", "LCC_STATISTICS_",str_replace_all(toupper(buffer), " ", "_"), ".RDS"))
  write_xlsx(df, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".xlsx"))
}



# -------------------------------------------------------------------------
#CREA UNA HOJA DE CALCULO CON LOS IDS DE CADA PA
parks_index2 <- parks_index[order(parks_index$protected_area),]
write_xlsx(parks_index2, path = "maps_and_figures/lcc_statistics/protected_areas_ids.xlsx", col_names = TRUE)


# -------------------------------------------------------------------------
# OTROS SCRIPTS PARA REUSO

# # REPROYECTAR A COORDENADAS PLANAS (EPSG: 9377; MAGNA origen nacional) (no se uso al final, se logro hacer buffers en lat/long)
# parks <- sort(tools::file_path_sans_ext(list.files(path = "lcc_4326", pattern = ".tif$")))
# new_crs <- "+proj=tmerc +lat_0=4.0 +lon_0=-73.0 +k=0.9992 +x_0=5000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# 
# for (park in parks) {
#   input <- readRDS(paste0("lcc_4326/", park, ".RDS"))
#   output <- projectRaster(input, crs = new_crs, method = "ngb", res =c(30, 30))
#   
#   saveRDS(output, file = paste0("lcc_9377/", park))
#   
#   writeRaster(output,
#               filename = paste0("lcc_9377/", park, ".tif"),
#               format = "GTiff",
#               datatype='INT1U',
#               overwrite=TRUE)
# }


# # GENERAR BUFFER DE CLASES DE CAMBIO PARQUE A PARQUE
# todo <- sort((tools::file_path_sans_ext(list.files(path = "lcc", pattern = ".tif$", full.names= FALSE))))
# ready <- sort((tools::file_path_sans_ext(list.files(path = "lcc_buffers", pattern = ".tif$", full.names= FALSE))))
# parks <- setdiff(todo, ready)
# park <- parks[1]
# parks <- setdiff(parks, c("AMACAYACU", "CAHUINARI", "CORDILLERA_DE_LOS_PICACHOS"))
# 
# for (park in parks) {
#   # rast <- readRDS(paste0("lcc/", park, ".RDS"))
#   rast <- raster(paste0("lcc/", park, ".tif"))
# 
#   rcl <- as.matrix(data.frame(class = 1:10, new_class = c(NA, NA, 1, NA, NA, NA, NA, NA, NA, NA)))
#   floss <- reclassify(rast, rcl) #capa de forest loss
# 
#   rcl2 <- as.matrix(data.frame(class = 1:10, new_class = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA)))
#   estf <- reclassify(rast, rcl2) #capa de stable forest
# 
#   # beginCluster()
#   # system.time(flossb <- clusterR(floss, gridDistance, args = list(origin = 1)))
#   # endCluster()
# 
#   system.time(flossb <- gridDistance(floss, origin = 1)) #genera la capa de distancias
# 
#   #se extraen las distancias menores a 90m
#   rc <- function(x1){
#     ifelse(x1 <= 90, 1, NA)
#   }
#   beginCluster()
#   system.time(buff <- clusterR(flossb, calc, args = list(fun = rc)))
#   endCluster()
#   saveRDS(buff, file = paste0("buffers/", park, ".RDS"))
#   writeRaster(buff,
#               filename = paste0("buffers/", park, ".tif"),
#               format = "GTiff",
#               datatype='INT1U',
#               overwrite=TRUE)
# 
#   #se extrae el anillo del buffer
#   stacked <- stack(floss, buff)
#   rc1 <- function(x1, x2){
#     ifelse((is.na(x1)) & (!is.na(x2)), 1, NA)
#   }
#   beginCluster()
#   system.time(buff1 <- clusterR(stacked, overlay, args = list(fun = rc1)))
#   endCluster()
#   saveRDS(buff1, file = paste0("buffers_ring/", park, ".RDS"))
#   writeRaster(buff1,
#               filename = paste0("buffers_ring/", park, ".tif"),
#               format = "GTiff",
#               datatype='INT1U',
#               overwrite=TRUE)
# 
#   #se extraen las zonas del anillo que corresponden a bosque
#   stacked1 <- stack(buff, estf)
#   rc2 <- function(x1, x2){
#     ifelse((!is.na(x1)) & (!is.na(x2)), 1, NA)
#   }
#   beginCluster()
#   system.time(buff2 <- clusterR(stacked1, overlay, args = list(fun = rc2)))
#   endCluster()
#   saveRDS(buff2, file = paste0("buffers_forest/", park, ".RDS"))
#   writeRaster(buff2,
#               filename = paste0("buffers_forest/", park, ".tif"),
#               format = "GTiff",
#               datatype='INT1U',
#               overwrite=TRUE)
# 
#   #se reemplazan las zonas de bosque del raster lcc con el buffer correspondiente
#   stacked2 <- stack(rast, buff2)
#   rc2 <- function(x1, x2){
#     ifelse((x1 == 1) & (is.na(x2)), x1, ifelse((x1 ==1) & (!is.na(x2)), 11, x1))
#   }
#   beginCluster()
#   system.time(final <- clusterR(stacked2, overlay, args = list(fun = rc2)))
#   endCluster()
#   saveRDS(final, file = paste0("lcc_buffers/", park, ".RDS"))
#   writeRaster(final,
#               filename = paste0("lcc_buffers/", park, ".tif"),
#               format = "GTiff",
#               datatype='INT1U',
#               overwrite=TRUE)
# 
#   rm(list = setdiff(ls(), c("parks", "park")))
#   gc()
# 
# }
