library(raster)
library(rgdal)
library(stringr)
library(writexl)

#agreement among ML algorithms for the study area
parks <- sort(list.files(path = "lcc", pattern = ".tif$"))
parks <- str_remove(parks, ".tif")

parks_index <- readOGR("boundaries_shapes/SPNN_SEL.shp")
parks_index <- as.data.frame(parks_index[, c(1, 4)])
names(parks_index) <- (c("id", "protected_area"))
parks_index[2] <- str_replace_all(parks_index$protected_area, " ", "_")
parks_index[2] <- str_replace_all(parks_index$protected_area, "__", "_")
parks_index[2] <- toupper(parks_index$protected_area)

years <- c(2000, 2018)
raster_format <- ".tif"

df <- data.frame(matrix(ncol = 9, nrow = 0))
names(df) <- c("protected_area", "id", "year", paste0(rep("X",5), seq(1:5)), "total")

for (year in years) {
  for (park in parks) {
    raster <- raster(paste0("frequency_raster/", park, "_", year, raster_format))
    df[nrow(df)+1,1] <- park
    df[nrow(df),2] <- parks_index$id[parks_index$protected_area == park]
    df[nrow(df),3] <- year
    df2 <- as.data.frame(table(values(raster)))
  
    for (class in df2$Var1) {
      df[nrow(df), as.integer(class)+3] <- df2$Freq[df2$Var1 == as.integer(class)]
    }
    
    df[nrow(df), "total"] <- sum(table(values(raster)))
    print(paste(park, "Ha sido procesado!"))
  }
}

df[is.na(df)] <- 0
saveRDS(df, paste0("frequency_statistics/", "statistics_pixels.RDS"))
write_xlsx(df, paste0("frequency_statistics/", "statistics_pixels.xlsx"))


agg_df <- aggregate(df[4:9], list(df$year), FUN = sum)
agg_df[2:6] <- (agg_df[2:7]/agg_df$total)*100
saveRDS(agg_df, paste0("frequency_statistics/", "statistics_by_year.RDS"))
write_xlsx(agg_df, paste0("frequency_statistics/", "statistics_by_year.xlsx"))

df[4:9] <- (df[4:9]/unlist(rep(df[9], 5)))*100
saveRDS(df, paste0("frequency_statistics/", "statistics_perc.RDS"))
write_xlsx(df, paste0("frequency_statistics/", "statistics_perc.xlsx"))


#agreement among ML algorithms by land cover/ land use
parks <- sort(list.files(path = "lcc", pattern = ".tif$"))
parks <- str_remove(parks, ".tif")

parks_index <- readOGR("boundaries_shapes/SPNN_SEL.shp")
parks_index <- as.data.frame(parks_index[, c(1, 4)])
names(parks_index) <- (c("id", "protected_area"))
parks_index[2] <- str_replace_all(parks_index$protected_area, " ", "_")
parks_index[2] <- str_replace_all(parks_index$protected_area, "__", "_")
parks_index[2] <- toupper(parks_index$protected_area)

land_cover <- data.frame(id = c(1:13), lc = c("Water Bodies",
                                      "Forest",
                                      "Shrublands",
                                      "Grasslands",
                                      "Secondary Vegetation",
                                      "Croplands",
                                      "Pastures",
                                      "Artificial Surfaces",
                                      "Bare Soils",
                                      "Clouds",
                                      "Shadows",
                                      "Glaciers",
                                      "Planted Forests"))
years <- c(2000, 2018)
raster_format <- ".tif"

df <- data.frame(matrix(ncol = 11, nrow = 0))
names(df) <- c("protected_area", "id", "year", "land_cover", "id_lc", paste0(rep("X",5), seq(1:5)), "total")

for (year in years) {
  
  for (park in parks) {
    
    raster <- raster(paste0("frequency_raster/", park, "_", year, raster_format))
    raster_lc <- raster(paste0("majority_classifications/", park, "_", year, raster_format))
    df2 <- as.data.frame(table(values(raster_lc)))
    
    for (lc in land_cover$id) {
      
      df[nrow(df)+1,1] <- park
      df[nrow(df),2] <- parks_index$id[parks_index$protected_area == park]
      df[nrow(df),3] <- year
      df[nrow(df),4] <- as.character(land_cover$lc[lc])
      df[nrow(df),5] <- land_cover$id[lc]
      
      if (lc %in% df2$Var1) {
        
        filter_raster <- raster_lc
        filter_raster[raster_lc[] != lc ] = NA
        lc_agreement <- mask(raster, filter_raster)
        df3 <- as.data.frame(table(values(lc_agreement)))
        
        for (class in df3$Var1) {
          df[nrow(df), as.integer(class)+5] <- df3$Freq[df3$Var1 == as.integer(class)]
        }
        
        df[nrow(df), "total"] <- sum(table(values(lc_agreement)))
        
      } else {
        
        df[nrow(df),6:ncol(df)] <- NA
      }
      print(paste(park, year, as.character(land_cover$lc[lc]),  "Ha sido procesado!"))
    }
  }
}

df[is.na(df)] <- 0
saveRDS(df, paste0("frequency_statistics/", "statistics_class_pixels.RDS"))
write_xlsx(df, paste0("frequency_statistics/", "statistics_class_pixels.xlsx"))


agg_df <- aggregate(df[6:11], list(df$year, df$land_cover, df$id_lc), FUN = sum)
names(agg_df)[1:3] <- c("year", "lc", "lc_id")
saveRDS(agg_df, paste0("frequency_statistics/", "statistics_class_year_pixels.RDS"))
write_xlsx(agg_df, paste0("frequency_statistics/", "statistics_class_year_pixels.xlsx"))

agg_df[4:9] <- (agg_df[4:9]/agg_df$total)*100
agg_df[is.na(agg_df)] <- as.integer(0)
saveRDS(agg_df, paste0("frequency_statistics/", "statistics_class_year_perc.RDS"))
write_xlsx(agg_df, paste0("frequency_statistics/", "statistics_class_year_perc.xlsx"))

