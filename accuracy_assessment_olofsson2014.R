# Source: https://blogs.fu-berlin.de/reseda/area-adjusted-accuracies/
# area adjusted accuracy assessment 
# calculated as in Olofsson et al. 2014

# # run if you have not created an accuracy matrix yet
# library(raster)
library(writexl)

# # import classification image and train and validation shapefiles
# setwd("/media/sf_exchange/landsatdata/")
# img.class <- raster("classification_RF.tif")
# shp.train <- shapefile("training_data.shp")
# shp.valid <- shapefile("validation_RF.shp")
# 
# # create regular accuracy matrix 
# confmat <- table(as.factor(extract(img.class, shp.valid)), as.factor(shp.valid$validclass))
# # get number of pixels per class and convert in km²
# imgVal <- as.factor(getValues(img.class))
# nclass <- length(unique(shp.train$classes))
# maparea <- sapply(1:nclass, function(x) sum(imgVal == x))
# maparea <- maparea * res(img.class)[1] ^ 2 / 1000000

# run if you already have an accuracy matrix in csv format
# load accuracy matrix
confmat <- read.csv("validation/SALIDAS COLLECT EARTH/accuracy_matrix.csv")

#load dataframes to estimate areas (based on the individual PA areas extraction)
areap <- readRDS("lcc_statistics_summary_end/LCC_STATISTICS_0_SUMMARY.RDS")
area10 <- readRDS("lcc_statistics_summary_end/LCC_STATISTICS_0_TO_10_KM_SUMMARY.RDS")
area20 <- readRDS("lcc_statistics_summary_end/LCC_STATISTICS_10_TO_20_KM_SUMMARY.RDS")

# number of classes in the matrix
nclass <- nrow(confmat)

# get class map areas in hectares using the number of pixels per class
maparea <- areap$non_adjusted + area10$non_adjusted + area20$non_adjusted

# set confidence interval (95%)
conf <- 1.96

# total  map area
A <- sum(maparea)

# proportion of area mapped as class i
w_i <- maparea / A

# number of reference points per class
n_i <- rowSums(confmat)

# population error matrix (Eq.4)
p <- w_i * confmat/n_i
p[is.na(p)] <- 0

# area estimation
p_area <- colSums(p) * A

# area estimation confidence interval (Eq.10)
p_area_CI <- conf * A * sqrt(colSums((w_i * p - p ^ 2) / (n_i - 1)))

# overall accuracy (Eq.1)
OA <- sum(diag(as.matrix(p)))

# producers accuracy (Eq.2)
PA <- diag(as.matrix(p)) / colSums(p)

# users accuracy (Eq.3)
UA <- diag(as.matrix(p)) / rowSums(p)

# overall accuracy confidence interval (Eq.5)
OA_V <- sum(w_i ^ 2 * UA * (1 - UA) / (n_i - 1))
OA_SE <- sqrt(OA_V)
OA_CI <- conf * OA_SE

# user accuracy confidence interval (Eq.6)
UA_V <- UA * (1 - UA) / (n_i - 1)
UA_SE <- sqrt(UA_V)
UA_CI <- conf * UA_SE

# producer accuracy confidence interval (Eq.7)
N_j <- sapply(1:nclass, function(x) sum(maparea / n_i * confmat[ , x]) )
tmp <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat[-x, x] / n_i[-x] * ( 1 - confmat[-x, x] / n_i[-x]) / (n_i[-x] - 1)) )
PA_V <- 1 / N_j ^ 2 * (maparea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp)
PA_SE <- sqrt(PA_V)
PA_CI <- conf * PA_SE

# # copy vectors to excel
# writeClipboard(as.character(PA_V))
# writeClipboard(as.character(PA_SE))
# writeClipboard(as.character(PA_CI))

# gather results
result <- matrix(c(names(confmat),p_area, p_area_CI, PA, PA_V, PA_SE, PA_CI, UA, UA_V, UA_SE, UA_CI, c(OA, rep(NA, nclass-1)), c(OA_V, rep(NA, nclass-1)), c(OA_SE, rep(NA, nclass-1)), c(OA_CI, rep(NA, nclass-1))), nrow = nclass)
colnames(result) <- c("class", "hectares", "hectares_CI", "PA", "PA_V", "PA_SE", "PA_CI", "UA", "UA_V", "UA_SE", "UA_CI", "OA", "OA_v", "OA_SE", "OA_CI")
result <- as.data.frame(result)
as.numeric(result[2:ncol(result)])


# export results
write_xlsx(result, paste0("validation/SALIDAS COLLECT EARTH/", "accuracy_assessment_r.xlsx"))
