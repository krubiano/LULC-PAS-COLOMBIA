parks <- sort(tools::file_path_sans_ext(list.files(path = "training_samples", pattern = "*.shp")))
years <- c("2000", "2018")
for (park in parks){
  for (year in years){
    if (paste0(park, "_", year) %in% tools::file_path_sans_ext(list.files(path = "reports", pattern = "*.html"))) {
      #do nothing
      } else {
  rmarkdown::render(input = "classification.RMD",
                    output_file = paste0("reports/", park, "_", year, ".html"),
                    params = list(park = park, year = year))
    rm(list = setdiff(ls(), c("parks", "years", "park", "year")))
    gc()
      }
  }
}

# progress_df <- readRDS("progress_df/progress_df")
# progress_df <- progress_df[-c(1:nrow(progress_df)),]
# saveRDS(progress_df, file = "progress_df/progress_df")
