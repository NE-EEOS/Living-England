#############################################################################################
############### Automated object-based classification using RF ##############################
#
# Author: Natural England, Evidence Earth Observation Service
# 
# Note: Work in progress


library(rgdal)
library(raster)
library(rgeos)
library(randomForest)
library(impute) # install zip file from: www.bioconductor.org/packages/release/bioc/html/impute.html
library(sp)

setwd("D:/LivingEngland/DATA")

source("C:/Users/ne.user/Documents/GitHub/Living-England/zonal_stats_velox.r")
source("C:/Users/ne.user/Documents/GitHub/Living-England/user_producer_accuracy.r")

#training data
#training.data.habitat.shp <- st_read("D:/JNCC_Training_Data/Training_Data/TrainingPoints_subset.shp", "TrainingPoints_subset")

#LE subset segmentation shapefile
#segmentation <- st_read("Segmentation/Subset_National_Seg_2017_18.gpkg", "Subset_National_Seg_2017_18")

#LE segmentation shapefile
#start <- proc.time()
segmentation <- st_read("Segmentation/National_Segmentation_2017_18.gpkg","National_Segmentation_2017_18")
#proc.time()-start


### Zonal statistics layers ###########################################################################
#
#

#LE Data
S2_spring <- "S2/Spring2017_cloudMaskedMosaic.img"
S2_summer <- "S2/Summer2017_cloudMaskedMosaic.img"
#S2_autumn <- "S2/Autumn2017_cloudMaskedMosaic.img"
S2_winter <- "S2/Winter2018_cloudMaskedMosaic.img"

#S1_spring <- "S1/X"
#S1_summer <- "S1/X"
#S1_autumn <- "S1/X"
#S1_winter <- "S1/X"

height <- "AncillaryData/APGBDTM10m.tif"
slope <- "AncillaryData/APGBSlope10m.tif"
aspect <- "AncillaryData/APGBAspect10m.tif"

buildingsprox <- "AncillaryData/Buildingsprox.tif"
foreshoreprox <- "AncillaryData/Foreshoreprox.tif"
moorlandprox <- "AncillaryData/Moorlandprox.tif"
roadprox <- "AncillaryData/Roadprox.tif"
surfaceWaterprox <- "AncillaryData/SurfaceWaterprox.tif"
tidalWaterprox <- "AncillaryData/TidalWaterprox.tif"
woodlandprox <- "AncillaryData/Woodlandprox.tif"

bioclim_max_temp <- "BioClim/wc2_bio_BNG_05.tif"
bioclim_min_temp <- "BioClim/wc2_bio_BNG_06.tif"
bioclim_annual_rainfall <- "BioClim/wc2_bio_BNG_12.tif"



# list(layer_name=c(path_to_raster, stat, band1[, band2, ...]), ...)

list.rasters <- list(
                     
                     #mean
                     S1_spring=c(S1_spring, "mean", 1, 2),
                     S1_summer=c(S1_summer, "mean", 1, 2),
                     S1_autumn=c(S1_autumn, "mean", 1, 2),
                     S1_winter=c(S1_winter, "mean", 1, 2),
                     
                     S2_spring=c(S2_spring, "mean", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_summer=c(S2_summer, "mean", 1, 2, 3, 4, 5, 6, 7, 8),
                     #S2_autumn=c(S2_autumn, "mean", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_winter=c(S2_winter, "mean", 1, 2, 3, 4, 5, 6, 7, 8),
                     
                     height=c(height, "mean", 1),
                     slope=c(slope, "mean", 1),
                     aspect=c(aspect, "mean", 1),

                     buildingsprox=c(buildingsprox, "mean", 1),
                     foreshoreprox=c(foreshoreprox, "mean", 1),
                     moorlandprox=c(moorlandprox, "mean", 1),
                     surfaceWaterprox=c(surfaceWaterprox, "mean", 1),
                     tidalWaterprox=c(tidalWaterprox, "mean", 1),
                     woodlandprox=c(woodlandprox, "mean", 1),

                     bioclim_max_temp=c(bioclim_max_temp, "mean", 1),
                     bioclim_min_temp=c(bioclim_min_temp, "mean", 1),
                     bioclim_annual_rainfall=c(bioclim_annual_rainfall, "mean", 1),
                     
                     #min
                     S1_spring=c(S1_spring, "min", 1, 2),
                     S1_summer=c(S1_summer, "min", 1, 2),
                     S1_autumn=c(S1_autumn, "min", 1, 2),
                     S1_winter=c(S1_winter, "min", 1, 2),
                     
                     S2_spring=c(S2_spring, "min", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_summer=c(S2_summer, "min", 1, 2, 3, 4, 5, 6, 7, 8),
                     #S2_autumn=c(S2_autumn, "min", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_winter=c(S2_winter, "min", 1, 2, 3, 4, 5, 6, 7, 8),
                     
                     height=c(height, "min", 1),
                     slope=c(slope, "min", 1),
                     aspect=c(aspect, "min", 1),
                     
                     buildingsprox=c(buildingsprox, "min", 1),
                     foreshoreprox=c(foreshoreprox, "min", 1),
                     moorlandprox=c(moorlandprox, "min", 1),
                     surfaceWaterprox=c(surfaceWaterprox, "min", 1),
                     tidalWaterprox=c(tidalWaterprox, "min", 1),
                     woodlandprox=c(woodlandprox, "min", 1),
                     
                     bioclim_max_temp=c(bioclim_max_temp, "min", 1),
                     bioclim_min_temp=c(bioclim_min_temp, "min", 1),
                     bioclim_annual_rainfall=c(bioclim_annual_rainfall, "min", 1),
                     
                     #max
                     S1_spring=c(S1_spring, "max", 1, 2),
                     S1_summer=c(S1_summer, "max", 1, 2),
                     S1_autumn=c(S1_autumn, "max", 1, 2),
                     S1_winter=c(S1_winter, "max", 1, 2),
                     
                     S2_spring=c(S2_spring, "max", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_summer=c(S2_summer, "max", 1, 2, 3, 4, 5, 6, 7, 8),
                     #S2_autumn=c(S2_autumn, "max", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_winter=c(S2_winter, "max", 1, 2, 3, 4, 5, 6, 7, 8),
                     
                     height=c(height, "max", 1),
                     slope=c(slope, "max", 1),
                     aspect=c(aspect, "max", 1),
                     
                     buildingsprox=c(buildingsprox, "max", 1),
                     foreshoreprox=c(foreshoreprox, "max", 1),
                     moorlandprox=c(moorlandprox, "max", 1),
                     surfaceWaterprox=c(surfaceWaterprox, "max", 1),
                     tidalWaterprox=c(tidalWaterprox, "max", 1),
                     woodlandprox=c(woodlandprox, "max", 1),
                     
                     bioclim_max_temp=c(bioclim_max_temp, "max", 1),
                     bioclim_min_temp=c(bioclim_min_temp, "max", 1),
                     bioclim_annual_rainfall=c(bioclim_annual_rainfall, "max", 1),
                     
                     #standard deviation (sd)
                     S1_spring=c(S1_spring, "sd", 1, 2),
                     S1_summer=c(S1_summer, "sd", 1, 2),
                     S1_autumn=c(S1_autumn, "sd", 1, 2),
                     S1_winter=c(S1_winter, "sd", 1, 2),
                     
                     S2_spring=c(S2_spring, "sd", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_summer=c(S2_summer, "sd", 1, 2, 3, 4, 5, 6, 7, 8),
                     #S2_autumn=c(S2_autumn, "sd", 1, 2, 3, 4, 5, 6, 7, 8),
                     S2_winter=c(S2_winter, "sd", 1, 2, 3, 4, 5, 6, 7, 8),
                     
                     height=c(height, "sd", 1),
                     slope=c(slope, "sd", 1),
                     aspect=c(aspect, "sd", 1),
                     
                     buildingsprox=c(buildingsprox, "sd", 1),
                     foreshoreprox=c(foreshoreprox, "sd", 1),
                     moorlandprox=c(moorlandprox, "sd", 1),
                     surfaceWaterprox=c(surfaceWaterprox, "sd", 1),
                     tidalWaterprox=c(tidalWaterprox, "sd", 1),
                     woodlandprox=c(woodlandprox, "sd", 1),
                     
                     bioclim_max_temp=c(bioclim_max_temp, "sd", 1),
                     bioclim_min_temp=c(bioclim_min_temp, "sd", 1),
                     bioclim_annual_rainfall=c(bioclim_annual_rainfall, "sd", 1))
                     


### Zonal Stats for Segmented Polygons ################################################################
#
#

# Calculate the zonal stats for each segmented polygon.

start <- proc.time()
zonal_stats_seg <- zonal.stats.velox(segmentation, list.rasters, tiles=25)
proc.time()-start

# # Save the results as an intermediate file
zonal_stats_seg <- write.table(zonal_stats_seg, "ZS/zonal_stats_seg.txt", sep="\t")
zonal_stats_seg <- read.table("ZS/zonal_stats_seg.txt", sep="\t", header=T)

# Append area and perimeter from shapefile if not already calculated
#if (!"area_ratio1" %in% names(zonal_stats_seg))
#{
#  zonal_stats_seg <- merge(zonal_stats_seg, segmentation, by="ID")
#  zonal_stats_seg$area_ratio1 <- with(zonal_stats_seg, Shape_Area/Shape_Leng)
#  zonal_stats_seg$area_ratio2 <- with(zonal_stats_seg, Shape_Leng/sqrt(Shape_Area))
#}

# Ensure that catagorical data doesn't having any missing or inf values
#zonal_stats_seg[is.na(zonal_stats_seg)] <- 0
#zonal_stats_seg[sapply(zonal_stats_seg, is.infinite)] <- 0

# Impute missing values for all S1 and S2 columns, excluding max and min statistics
impute.cols <- grepl("S2|S1",colnames(zonal_stats_seg)) & !grepl("max|min",colnames(zonal_stats_seg))
zonal_stats.imputed <- impute.knn(as.matrix(zonal_stats_seg[,impute.cols]))
zonal_stats_seg <- cbind(zonal_stats_seg[,!impute.cols], zonal_stats.imputed$data[,colnames(zonal_stats_seg)[impute.cols]])

# for OS segments, remove FID columns that have crept in
fid.cols <- grepl('FID', colnames(zonal_stats_seg))
zonal_stats_seg <- zonal_stats_seg[,!fid.cols]

## Indices ###############

# Calculate NDVI and NDWI
#zonal_stats_seg$S2_summer_ndvi <- with(zonal_stats_seg, (S2_summer_nir - S2_summer_red)/(S2_summer_nir + S2_summer_red))
#zonal_stats_seg$S2_summer_ndwi <- with(zonal_stats_seg, (S2_summer_nir - S2_summer_swir1)/(S2_summer_nir + S2_summer_swir1))
#zonal_stats_seg$S2_winter_ndvi <- with(zonal_stats_seg, (S2_winter_nir - S2_winter_red)/(S2_winter_nir + S2_winter_red))
#zonal_stats_seg$S2_winter_ndwi <- with(zonal_stats_seg, (S2_winter_nir - S2_winter_swir1)/(S2_winter_nir + S2_winter_swir1))

# Top indices identified as important
#zonal_stats_seg$sar_winter_ndvhvvi <- with(zonal_stats_seg, (sar_winter_vh_median - sar_winter_vv_median)/(sar_winter_vh_median + sar_winter_vv_median))

#zonal_stats_seg$s2_summer_greenmrededge5m_di <- with(zonal_stats_seg, (S2_summer_green_median - S2_summer_rededge5_median))

# Seasonal difference indices
#zonal_stats_seg$S2_summer_swir2m_S2_winter_rededge5m_di <- with(zonal_stats_seg, (S2_summer_swir2_median - S2_winter_rededge5_median))

# Ensure that catagorical data doesn't having any missing or inf values
zonal_stats_seg[is.na(zonal_stats_seg)] <- 0
zonal_stats_seg[sapply(zonal_stats_seg, is.infinite)] <- 0

colnames(zonal_stats_seg)[colnames(zonal_stats_seg)==""] <- "ID"

write.table(zonal_stats_seg, "ZS/zonal_stats_seg_impute.txt", sep="\t")

# RUN UP TO HERE :D

### Training data (Zonal Stats) #######################################################################
#
#

nmax <- 30 #### Number of training points per class

if (!exists("zonal_stats_seg"))
{
  zonal_stats_seg <- read.table("ZS/zonal_stats_seg_impute.txt", sep="\t", header=T, as.is=T)
}

training.data.shp <- training.data.habitat.shp[c("Detailed", "Broad", "Tier")]

# Identify the segmented polygons the training points fall within and extract the zonal statistics from these
training.data.ids <- as.numeric(st_within(training.data.shp, segmentation))

training.data <- cbind(as.data.frame(training.data.shp),zonal_stats_seg[training.data.ids,])

training.data <- training.data[names(training.data)!="geometry"]

training.data.all <- training.data[c(1:ncol(training.data))]

# Remove rows with mising values
training.data.all <- training.data.all[complete.cases(training.data.all),]
training.data.all$Tier <- as.numeric(training.data.all$Tier)

# Select the training data for points with accurate spatial mapping and for mappable habitat classes
training.data.all <- subset(training.data.all, Tier<=3)

# Split into stratified training and test datasets
training.data <- NULL
training.data.test <- NULL

# Loop through all the classes
set.seed(1) # Set a random seed to ensure consistent training and test datasets
for(c in unique(training.data.all$Detailed))
{
  # Select the subset of rows for the current class
  training.data.sub <- subset(training.data.all, Detailed==c)
  
  # Select a sample prioritising the training points from the highest tier
  n <- nrow(training.data.sub)
  prb <- ifelse(training.data.sub$Tier == 1,0.60, ifelse(training.data.sub$Tier == 2, 0.30, 0.10))
  training.data.sub <- training.data.sub[sample(n, min(n,nmax), prob=prb, replace=F),]
  
  # Split the data using a random sample
  subset <- random.subset(training.data.sub, 0.8)
  training.data <- rbind(training.data, training.data.sub[subset,])
  training.data.test <- rbind(training.data.test, training.data.sub[-subset,])
  rownames(training.data.test) <- NULL
}

# Remove duplicates from test dataset
training.data.test <- training.data.test[!duplicated(training.data.test),]

## Write training_data to text file
write.table(training.data, "Training_Data/training_data_subset.txt", sep="\t")
write.table(training.data.test, "Training_Data/training_data_test_subset.txt", sep="\t")

### Classify training points using random forest ######################################################
#
#

if (!exists("zonal_stats_seg"))
{
  zonal_stats_seg <- read.table("ZS/zonal_stats_seg_impute.txt", sep="\t", header=T, as.is=T)
}

# Read in training and test datasets
training.data <- read.table("Training_Data/training_data_subset.txt", sep="\t", header=T)
training.data.test <- read.table("Training_Data/training_data_test_subset.txt", sep="\t", header=T)

# Ensure that any catagorical data are converted to factors
#training.data$vectormap <- as.factor(training.data$vectormap)
#training.data.test$vectormap <- as.factor(training.data.test$vectormap)
#zonal_stats_seg$vectormap <- as.factor(zonal_stats_seg$vectormap)

#training.data$Detailed <- as.factor(as.character(training.data$Detailed))
#training.data$Broad <- as.factor(as.character(training.data$Broad))

# Predict habitats using random forest
#Run for only the top 42 most important variables
#Note - (2,5:ncol) is there to select just the detailed column and the other data cols (ie cuts out the ID, Broad and Tier cols) from training.data
M.rf.detailed.all <- randomForest(Detailed ~ ., data=training.data[c(1,5:ncol(training.data))], na.action=na.omit)
i <- colnames(training.data) %in% c(rownames(M.rf.detailed.all$importance)[order(M.rf.detailed.all$importance, decreasing=T)][1:42],"Detailed")
M.rf.detailed <- randomForest(Detailed ~ ., data=training.data[i], na.action=na.omit)

# Calculate confusion matrix
p <- predict(M.rf.detailed, training.data.test, type="response")
confusion.matrix(training.data.test$Detailed, p)


# Predict classes for all polygons
results.detailed.probs <- predict(M.rf.detailed, zonal_stats_seg,
                                  type="vote", norm.votes=TRUE,
                                  progress="text")

responseNFromProbs <- function(df, n=1) {
  columns <- colnames(df)
  response <- apply(df, MARGIN=1, FUN=function(x) {columns[order(x, decreasing=TRUE)[n]]})
  return (response)
}

probNFromProbs <- function(df, n=1) {
  response <- apply(df, MARGIN=1, FUN=function(x) {sort(x, decreasing=TRUE)[n]})
  return (response)
}

results.detailed.response1 <- responseNFromProbs(results.detailed.probs, n=1)
results.detailed.prob1 <- probNFromProbs(results.detailed.probs, n=1)*100
results.detailed.response2 <- responseNFromProbs(results.detailed.probs, n=2)
results.detailed.prob2 <- probNFromProbs(results.detailed.probs, n=2)*100

#*new*
#zonal_stats_seg <- as.data.frame(zonal_stats_seg)
zonal_stats_seg <- read.table("ZS/zonal_stats_seg_impute.txt", sep="\t", header=T, as.is=T)

# Combine results with segmentation polygons and save to new shapefile
results.rf <- data.frame(ID=zonal_stats_seg$ID,
                         A_pred=results.detailed.response1,
                         A_prob=results.detailed.prob1,
                         B_pred=results.detailed.response2,
                         B_prob=results.detailed.prob2)

segmentation.p <- merge(segmentation, results.rf, by="ID")

st_write(segmentation.p, 
         "Outputs/Living_Maps_Subset_RF_Detailed_test.shp",
         "Living_Maps_Subset_RF_Detailed_test",
         driver="ESRI Shapefile",
         delete_layer = TRUE)

rm(segmentation.p)
