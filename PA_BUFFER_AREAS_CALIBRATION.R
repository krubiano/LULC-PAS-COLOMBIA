library(raster)
library(sf)
library(writexl)
library(stringr)
library(rgdal)
#library(RPyGeo)

# #EXTRACCIÓN DE LAS AREAS Y PROPORCIONES DE LAS CLASES DE CAMBIO POR PARQUE Y GLOBAL
# #CALCULA LAS AREAS PARA LOS DIFERENTES RANGOS DE BUFFER DE CADA PA

parks <- sort(list.files(path = "lcc", pattern = "*.RDS"))

parks_sel <- readOGR("boundaries_shapes/SPNN_SEL.shp")
parks_index <- as.data.frame(parks_sel[, c(1, 4)])
names(parks_index) <- (c("id", "protected_area"))
parks_index[2] <- str_replace_all(parks_index$protected_area, " ", "_")
parks_index[2] <- str_replace_all(parks_index$protected_area, "__", "_")
parks_index[2] <- toupper(parks_index$protected_area)

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
    
    for(class in df2$Var1) {
      df[nrow(df), as.integer(class)+2] <- df2$Freq[df2$Var1 == as.integer(class)]
    }

    df[nrow(df), "total"] <- sum(table(values(raster)))
    print(paste(str_remove(park, ".RDS"), "Ha sido procesado!"))
  }
  df[is.na(df)] <- 0
  df[3:13] <- (df[3:13]*900)/10000
  df[1] <- str_replace_all(df$protected_area, "_", " ")
  saveRDS(df, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".RDS"))
  write_xlsx(df, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".xlsx"))
}


##CALIBRACION DE LAS AREAS DE CADA AP Y BUFFER USANDO LA MATRIZ GLOBAL DE PRECISION

# load confusion matrix in standard format
confmat <- read.csv("lcc_statistics_end/accuracy_matrix.csv")

# number of reference points per class
n_i <- rowSums(confmat)

# number of classes in the matrix
nclass <- nrow(confmat)

# set pixel area resolution in squered meters. To be used if need to compute areas from pixel counts
pixel_area <- 900

#set confidence interval (95%) for CI
conf <- 1.96

#vector with buffer distances
parks_rings <- readOGR("boundaries_shapes/PARKS_AND_RINGS.shp")
parks_rings$type_ring[is.na(parks_rings$type_ring)] <- 0
buffers <- c(unique(parks_rings$type_ring), "0 to 10 km", "10 to 20 km")

#buffer <- buffers[5]
for (buffer in buffers) {
  
  if (buffer == "0 to 10 km") {
    areas1 <- readRDS(paste0("lcc_statistics_end/", "LCC_STATISTICS_", "0_TO_5_KM", ".RDS"))
    areas2 <- readRDS(paste0("lcc_statistics_end/", "LCC_STATISTICS_", "5_TO_10_KM", ".RDS"))
    areas <- areas1
    areas[3:13] <- areas[3:13] + areas2[3:13]
    
    saveRDS(areas, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".RDS"))
    write_xlsx(areas, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".xlsx"))
    
  } else if (buffer == "10 to 20 km") {
    areas1 <- readRDS(paste0("lcc_statistics_end/", "LCC_STATISTICS_", "10_TO_15_KM", ".RDS"))
    areas2 <- readRDS(paste0("lcc_statistics_end/", "LCC_STATISTICS_", "15_TO_20_KM", ".RDS"))
    areas <- areas1
    areas[3:13] <- areas[3:13] + areas2[3:13]
    
    saveRDS(areas, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".RDS"))
    write_xlsx(areas, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".xlsx"))
    
  } else {
    areas <- readRDS(paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), ".RDS"))
    
  }
  
  #create a df to store CI
  areas_ci <- data.frame(matrix(ncol = 12, nrow = 0))
  names(areas_ci) <- c("protected_areas", "id", "sf", "fonv", "fplaso", "fg", "sonv", "onvl", "onvg", "splas", "so", "ot")
  
  areas <- areas[order(areas$id),]
  names(areas) <- c("protected_areas", "id", "sf", "fonv", "fplaso", "fg", "sonv", "onvl", "onvg", "splas", "so", "ot", "total")
  calib_areas <- areas
  
  #i <- 2
  for (i in 1:nrow(areas)) {
    
    # area mapped as class i
    maparea <- areas[i, 3:12]
    maparea <- t(maparea)
    
    # total map area
    A <- sum(maparea)
    
    # proportion of area mapped as class i
    w_i <- maparea / A
    
    # population error matrix (Eq.4)
    p <- w_i * confmat/n_i
    p[is.na(p)] <- 0
    
    # calibrated area estimation
    p_area <- colSums(p) * A
    
    # area estimation confidence interval (Eq.10)
    p_area_ci <- conf * A * sqrt(colSums((w_i * p - p ^ 2) / (n_i - 1)))
    
    # overwrite data in calib_areas dataframe
    calib_areas[i, 3:12] <- p_area
    calib_areas[i, 13] <- sum(p_area)
    
    # write areas CI
    areas_ci[i, 1:2] <- calib_areas[i, 1:2]
    areas_ci[i, 3:ncol(areas_ci)] <- p_area_ci
  }
  
  saveRDS(calib_areas, paste0("lcc_statistics_end/", "LCC_STATISTICS_",str_replace_all(toupper(buffer), " ", "_"), "_CALIBRATED.RDS"))
  write_xlsx(calib_areas, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), "_CALIBRATED.xlsx"))
  
  saveRDS(areas_ci, paste0("lcc_statistics_end/", "LCC_STATISTICS_",str_replace_all(toupper(buffer), " ", "_"), "_CALIBRATED_CI.RDS"))
  write_xlsx(areas_ci, paste0("lcc_statistics_end/", "LCC_STATISTICS_", str_replace_all(toupper(buffer), " ", "_"), "_CALIBRATED_CI.xlsx"))
}


##CALCULO DE TNVL BY PA AND  BUFFERS

data <- read.csv("lcc_statistics_end/protected_areas_data.csv")

#PAs
park <- readRDS("lcc_statistics_end/LCC_STATISTICS_0_CALIBRATED.RDS")
park$region <- data$region
park$sinap_cat <- data$sinap_cat
park$iucn_cat <- data$iucn_cat
park <- park[, c(1, 2, 14, 15, 16, 3:13)]

park$forest_t0 <- park$sf + park$fplaso
park$forest_t1 <- park$sf
park$forest_loss <- park$forest_t0 - park$forest_t1
park$forest_loss_perc <- ((park$forest_t0 - park$forest_t1)/park$forest_t0)*100

park$onv_t0 <- park$sonv + park$onvl
park$onv_t1 <- park$sonv
park$onv_loss <- park$onv_t0 - park$onv_t1
park$onv_loss_perc <- ((park$onv_t0 - park$onv_t1)/park$onv_t0)*100

park$tnv_t0 <- park$forest_t0 + park$onv_t0
park$tnv_t1 <- park$forest_t1 + park$onv_t1
park$tnv_loss <- park$tnv_t0 - park$tnv_t1
park$tnv_loss_perc <- ((park$tnv_t0 - park$tnv_t1)/park$tnv_t0)*100

saveRDS(park, "lcc_statistics_end/LCC_STATISTICS_PARK_TNVL.RDS")
write_xlsx(park, "lcc_statistics_end/LCC_STATISTICS_PARK_TVNL.xlsx")

#10-KM BUFFER
buffer <- readRDS("lcc_statistics_end/LCC_STATISTICS_0_TO_10_KM_CALIBRATED.RDS")
buffer$region <- data$region
buffer$sinap_cat <- data$sinap_cat
buffer$iucn_cat <- data$iucn_cat
buffer <- buffer[, c(1, 2, 14, 15, 16, 3:13)]

buffer$forest_t0 <- buffer$sf + buffer$fplaso
buffer$forest_t1 <- buffer$sf
buffer$forest_loss <- buffer$forest_t0 - buffer$forest_t1
buffer$forest_loss_perc <- ((buffer$forest_t0 - buffer$forest_t1)/buffer$forest_t0)*100

buffer$onv_t0 <- buffer$sonv + buffer$onvl
buffer$onv_t1 <- buffer$sonv
buffer$onv_loss <- buffer$onv_t0 - buffer$onv_t1
buffer$onv_loss_perc <- ((buffer$onv_t0 - buffer$onv_t1)/buffer$onv_t0)*100

buffer$tnv_t0 <- buffer$forest_t0 + buffer$onv_t0
buffer$tnv_t1 <- buffer$forest_t1 + buffer$onv_t1
buffer$tnv_loss <- buffer$tnv_t0 - buffer$tnv_t1
buffer$tnv_loss_perc <- ((buffer$tnv_t0 - buffer$tnv_t1)/buffer$tnv_t0)*100

saveRDS(buffer, "lcc_statistics_end/LCC_STATISTICS_BUFFER_TNVL.RDS")
write_xlsx(buffer, "lcc_statistics_end/LCC_STATISTICS_BUFFER_TVNL.xlsx")

#20-KM BUFFER
buffer <- readRDS("lcc_statistics_end/LCC_STATISTICS_10_TO_20_KM_CALIBRATED.RDS")
buffer$region <- data$region
buffer$sinap_cat <- data$sinap_cat
buffer$iucn_cat <- data$iucn_cat
buffer <- buffer[, c(1, 2, 14, 15, 16, 3:13)]

buffer$forest_t0 <- buffer$sf + buffer$fplaso
buffer$forest_t1 <- buffer$sf
buffer$forest_loss <- buffer$forest_t0 - buffer$forest_t1
buffer$forest_loss_perc <- ((buffer$forest_t0 - buffer$forest_t1)/buffer$forest_t0)*100

buffer$onv_t0 <- buffer$sonv + buffer$onvl
buffer$onv_t1 <- buffer$sonv
buffer$onv_loss <- buffer$onv_t0 - buffer$onv_t1
buffer$onv_loss_perc <- ((buffer$onv_t0 - buffer$onv_t1)/buffer$onv_t0)*100

buffer$tnv_t0 <- buffer$forest_t0 + buffer$onv_t0
buffer$tnv_t1 <- buffer$forest_t1 + buffer$onv_t1
buffer$tnv_loss <- buffer$tnv_t0 - buffer$tnv_t1
buffer$tnv_loss_perc <- ((buffer$tnv_t0 - buffer$tnv_t1)/buffer$tnv_t0)*100

saveRDS(buffer, "lcc_statistics_end/LCC_STATISTICS_BUFFER_20_TNVL.RDS")
write_xlsx(buffer, "lcc_statistics_end/LCC_STATISTICS_BUFFER_20_TVNL.xlsx")

#AGGREGATE BY REGIONS SUM
#PAs
park_regions <- aggregate(x = park[6:ncol(park)], by = list(region = park$region), FUN = sum)
park_regions$forest_loss_perc <- ((park_regions$forest_t0 - park_regions$forest_t1)/park_regions$forest_t0)*100
park_regions$onv_loss_perc <- ((park_regions$onv_t0 - park_regions$onv_t1)/park_regions$onv_t0)*100
park_regions$tnv_loss_perc <- ((park_regions$tnv_t0 - park_regions$tnv_t1)/park_regions$tnv_t0)*100
saveRDS(park_regions, "lcc_statistics_end/LCC_STATISTICS_PARK_REG_TNVL.RDS")
write_xlsx(park_regions, "lcc_statistics_end/LCC_STATISTICS_PARK_REG_TVNL.xlsx")

#10-km buffer
buffer_regions <- aggregate(x = buffer[6:ncol(buffer)], by = list(region = buffer$region), FUN = sum)
buffer_regions$forest_loss_perc <- ((buffer_regions$forest_t0 - buffer_regions$forest_t1)/buffer_regions$forest_t0)*100
buffer_regions$onv_loss_perc <- ((buffer_regions$onv_t0 - buffer_regions$onv_t1)/buffer_regions$onv_t0)*100
buffer_regions$tnv_loss_perc <- ((buffer_regions$tnv_t0 - buffer_regions$tnv_t1)/buffer_regions$tnv_t0)*100
saveRDS(buffer_regions, "lcc_statistics_end/LCC_STATISTICS_BUFFER_REG_TNVL.RDS")
write_xlsx(buffer_regions, "lcc_statistics_end/LCC_STATISTICS_BUFFER_REG_TVNL.xlsx")

#20-km buffer
buffer_regions <- aggregate(x = buffer[6:ncol(buffer)], by = list(region = buffer$region), FUN = sum)
buffer_regions$forest_loss_perc <- ((buffer_regions$forest_t0 - buffer_regions$forest_t1)/buffer_regions$forest_t0)*100
buffer_regions$onv_loss_perc <- ((buffer_regions$onv_t0 - buffer_regions$onv_t1)/buffer_regions$onv_t0)*100
buffer_regions$tnv_loss_perc <- ((buffer_regions$tnv_t0 - buffer_regions$tnv_t1)/buffer_regions$tnv_t0)*100
saveRDS(buffer_regions, "lcc_statistics_end/LCC_STATISTICS_BUFFER_20_REG_TNVL.RDS")
write_xlsx(buffer_regions, "lcc_statistics_end/LCC_STATISTICS_BUFFER_20_REG_TVNL.xlsx")



#AGGREGATE BY SINAP CATEGORY SUM

#PAs
park_sinap <- aggregate(x = park[6:ncol(park)], by = list(sinap_cat = park$sinap_cat), FUN = sum)
park_sinap$forest_loss_perc <- ((park_sinap$forest_t0 - park_sinap$forest_t1)/park_sinap$forest_t0)*100
park_sinap$onv_loss_perc <- ((park_sinap$onv_t0 - park_sinap$onv_t1)/park_sinap$onv_t0)*100
park_sinap$tnv_loss_perc <- ((park_sinap$tnv_t0 - park_sinap$tnv_t1)/park_sinap$tnv_t0)*100
saveRDS(park_sinap, "lcc_statistics_end/LCC_STATISTICS_PARK_SINAP_TNVL.RDS")
write_xlsx(park_sinap, "lcc_statistics_end/LCC_STATISTICS_PARK_SINAP_TVNL.xlsx")

#10-km buffer
buffer_sinap <- aggregate(x = buffer[6:ncol(buffer)], by = list(sinap_cat = buffer$sinap_cat), FUN = sum)
buffer_sinap$forest_loss_perc <- ((buffer_sinap$forest_t0 - buffer_sinap$forest_t1)/buffer_sinap$forest_t0)*100
buffer_sinap$onv_loss_perc <- ((buffer_sinap$onv_t0 - buffer_sinap$onv_t1)/buffer_sinap$onv_t0)*100
buffer_sinap$tnv_loss_perc <- ((buffer_sinap$tnv_t0 - buffer_sinap$tnv_t1)/buffer_sinap$tnv_t0)*100
saveRDS(buffer_sinap, "lcc_statistics_end/LCC_STATISTICS_BUFFER_SINAP_TNVL.RDS")
write_xlsx(buffer_sinap, "lcc_statistics_end/LCC_STATISTICS_BUFFER_SINAP_TVNL.xlsx")

#20-km buffer
buffer_sinap <- aggregate(x = buffer[6:ncol(buffer)], by = list(sinap_cat = buffer$sinap_cat), FUN = sum)
buffer_sinap$forest_loss_perc <- ((buffer_sinap$forest_t0 - buffer_sinap$forest_t1)/buffer_sinap$forest_t0)*100
buffer_sinap$onv_loss_perc <- ((buffer_sinap$onv_t0 - buffer_sinap$onv_t1)/buffer_sinap$onv_t0)*100
buffer_sinap$tnv_loss_perc <- ((buffer_sinap$tnv_t0 - buffer_sinap$tnv_t1)/buffer_sinap$tnv_t0)*100
saveRDS(buffer_sinap, "lcc_statistics_end/LCC_STATISTICS_BUFFER_20_SINAP_TNVL.RDS")
write_xlsx(buffer_sinap, "lcc_statistics_end/LCC_STATISTICS_BUFFER_20_SINAP_TVNL.xlsx")

#CALIBRATE SUMMARY AREAS FOR COMPLETE STUDY AREA BY BUFFER
# load confusion matrix in standard format
confmat <- read.csv("lcc_statistics_end/accuracy_matrix.csv")

# number of reference points per class
n_i <- rowSums(confmat)

# number of classes in the matrix
nclass <- nrow(confmat)

#set confidence interval (95%) for CI
conf <- 1.96

#vector fo iterate
areas <- c("0", "0_TO_10_KM", "10_TO_20_KM")

#i <- 1
for (i in 1:length(areas)) {
  
  data <- readRDS(paste0("lcc_statistics_end/LCC_STATISTICS_", areas[i], ".RDS"))
  data <- as.data.frame(colSums(data[3:12]))
  names(data) <- "non_adjusted"
  
  # area mapped as class i
  maparea <- data$non_adjusted
  
  # total map area
  A <- sum(maparea)
  
  # proportion of area mapped as class i
  w_i <- maparea / A
  
  # population error matrix (Eq.4)
  p <- w_i * confmat/n_i
  p[is.na(p)] <- 0
  
  # calibrated area estimation
  p_area <- colSums(p) * A
  
  # area estimation confidence interval (Eq.10)
  p_area_ci <- conf * A * sqrt(colSums((w_i * p - p ^ 2) / (n_i - 1)))
  
  #join dataframe
  data$adjusted <- p_area
  data$ci <- p_area_ci
  data$class <- c("sf", "fonv", "fplaso", "fg", "sonv", "onvl", "onvg", "splas", "so", "ot")
  data <- data[, c(4, 1, 2, 3)]
  
  saveRDS(data, paste0("lcc_statistics_summary_end/", "LCC_STATISTICS_", areas[i], "_SUMMARY.RDS"))
  write_xlsx(data, paste0("lcc_statistics_summary_end/", "LCC_STATISTICS_", areas[i], "_SUMMARY.xlsx"))
}
