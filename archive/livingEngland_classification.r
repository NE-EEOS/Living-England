#############################################################################################
############### Automated object-based classification using RF ##############################
# Duplicated from livingEngland_classification.r and updated to run on a single bio-geographic zone
# Biogeographic Zone Number: 1
# Author: Natural England, Evidence Earth Observation Service
# 
# Note: work in progress
# For 'impute', install zip file from: www.bioconductor.org/packages/release/bioc/html/impute.html
#

library(rgdal)
library(raster)
library(rgeos)
library(randomForest)
library(impute) 
library(sp)
library(sf)

setwd("D:/LivingEngland_PhaseII/DATA")

source("C:/Users/ne.user/Documents/GitHub/Living-England/zonal_stats_velox.r")
source("C:/Users/ne.user/Documents/GitHub/Living-England/user_producer_accuracy.r")

#training data
training.data.habitat.gpkg <- st_read("TRAININGDATA/GeoPackages/BioZone1_TrainingData_outliersRemoved.gpkg", "BioZone1_TrainingData_outliersRemoved")

#LE segmentation shapefile
segmentation <- st_read("SEGMENTATION/GeoPackages/BGZ_01_Seg18.gpkg", "BGZ_01_Seg18")

### Zonal statistics layers ###########################################################################
#
#

# LE Data
S2_summer <- "S2/10band_masked_summer_2018_mosaic.img"
S2_winter <- "S2/10band_masked_winter_2018_19_mosaic.img"

S1_summer <- "S1/s1_Summer2018_mosaic.tif"
S1_winter <- "S1/s1_Winter2018_19_mosaic.tif"

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

bioclim_max_temp <- "AncillaryData/wc2_bio_BNG_05.tif"
bioclim_min_temp <- "AncillaryData/wc2_bio_BNG_06.tif"
bioclim_annual_rainfall <- "AncillaryData/wc2_bio_BNG_12.tif"


# list(layer_name=c(path_to_raster, stat, band1[, band2, ...]), ...)

list.rasters <- list(
  
  #mean
  S1_summer=c(S1_summer, "mean", 1, 2),
  S1_winter=c(S1_winter, "mean", 1, 2),
  
  S2_summer=c(S2_summer, "mean", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  S2_winter=c(S2_winter, "mean", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  
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
  S1_summer=c(S1_summer, "min", 1, 2),
  S1_winter=c(S1_winter, "min", 1, 2),
  
  S2_summer=c(S2_summer, "min", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  S2_winter=c(S2_winter, "min", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  
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
  S1_summer=c(S1_summer, "max", 1, 2),
  S1_winter=c(S1_winter, "max", 1, 2),
  
  S2_summer=c(S2_summer, "max", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  S2_winter=c(S2_winter, "max", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  
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
  S1_summer=c(S1_summer, "sd", 1, 2),
  S1_winter=c(S1_winter, "sd", 1, 2),
  
  S2_summer=c(S2_summer, "sd", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  S2_winter=c(S2_winter, "sd", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  
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
# Est. 3hrs
#start <- proc.time()
#zonal_stats_seg <- zonal.stats.velox(segmentation, list.rasters, tiles=1)
#proc.time()-start

# Save the results as an intermediate file
#zonal_stats_seg <- write.table(zonal_stats_seg, "ZS/BGZ01_zonal_stats_seg.txt", sep="\t")
zonal_stats_seg <- read.table("ZS/BGZ01_zonal_stats_seg.txt", sep="\t", header=T)

# Append area and perimeter from shapefile if not already calculated
#if (!"area_ratio1" %in% names(zonal_stats_seg))
#{
#  zonal_stats_seg <- merge(zonal_stats_seg, segmentation, by="ID")
#  zonal_stats_seg$area_ratio1 <- with(zonal_stats_seg, Shape_Area/Shape_Leng)
#  zonal_stats_seg$area_ratio2 <- with(zonal_stats_seg, Shape_Leng/sqrt(Shape_Area))
#}

# Not currently needed
# Ensure that catagorical data doesn't having any missing or inf values
#zonal_stats_seg[is.na(zonal_stats_seg)] <- 0
#zonal_stats_seg[sapply(zonal_stats_seg, is.infinite)] <- 0

# Impute missing values for all S1 and S2 columns, excluding max and min statistics
impute.cols <- grepl("S2|S1",colnames(zonal_stats_seg)) & !grepl("max|min",colnames(zonal_stats_seg))
zonal_stats.imputed <- impute.knn(as.matrix(zonal_stats_seg[,impute.cols]))
zonal_stats_seg <- cbind(zonal_stats_seg[,!impute.cols], zonal_stats.imputed$data[,colnames(zonal_stats_seg)[impute.cols]])

# for OS segments, remove FID columns that have crept in
#fid.cols <- grepl('FID', colnames(zonal_stats_seg))
#zonal_stats_seg <- zonal_stats_seg[,!fid.cols]

## Indices ###############

# Calculate NDVI and NDWI
zonal_stats_seg$S2_summer_ndvi <- with(zonal_stats_seg, (S2_summer_mean_band7 - S2_summer_mean_band3)/(S2_summer_mean_band7 + S2_summer_mean_band3))
zonal_stats_seg$S2_summer_ndwi <- with(zonal_stats_seg, (S2_summer_mean_band7 - S2_summer_mean_band9)/(S2_summer_mean_band7 + S2_summer_mean_band9))
zonal_stats_seg$S2_winter_ndvi <- with(zonal_stats_seg, (S2_winter_mean_band7 - S2_winter_mean_band3)/(S2_winter_mean_band7 + S2_winter_mean_band3))
zonal_stats_seg$S2_winter_ndwi <- with(zonal_stats_seg, (S2_winter_mean_band7 - S2_winter_mean_band9)/(S2_winter_mean_band7 + S2_winter_mean_band9))

# Ensure that catagorical data doesn't having any missing or inf values
zonal_stats_seg[is.na(zonal_stats_seg)] <- 0
zonal_stats_seg[sapply(zonal_stats_seg, is.infinite)] <- 0

colnames(zonal_stats_seg)[colnames(zonal_stats_seg)==""] <- "ID"

write.table(zonal_stats_seg, "ZS/BGZ01_zonal_stats_seg_indices.txt", sep="\t")


### Training data (Zonal Stats) #######################################################################
#
#

nmax <- 500 #### Number of training points per class


if (!exists("zonal_stats_seg"))
{
  zonal_stats_seg <- read.table("ZS/BGZ01_zonal_stats_seg_indices.txt", sep="\t", header=T, as.is=T)
}

# Only include training data from 200X onwards
training.data.gpkg <- training.data.habitat.gpkg[c("Training_P", "UKHab_Deta", "Survey_Dat", "UKHab_Broa")]
#training.data.gpkg <- subset(training.data.gpkg, Survey_Dat>=2009)

# Identify the segmented polygons the training points fall within and extract the zonal statistics from these
training.data.ids <- as.numeric(st_within(training.data.gpkg, segmentation))

training.data <- cbind(as.data.frame(training.data.gpkg),zonal_stats_seg[training.data.ids,])

# Remove the column "geom" and "Survey_Date"
training.data <- training.data[names(training.data)!="geom"]
#training.data <- training.data[names(training.data)!="Survey_Dat"]

# Write the training data zonal stats to a txt file 
write.table(training.data, "TrainingData/BGZ01_training_data_zs.txt", sep="\t")

##GPPRunToHere - 12-02-2020

training.data.all <- training.data[c(1:ncol(training.data))]

# Remove rows with mising values
training.data.all <- training.data.all[complete.cases(training.data.all),]
#training.data.all$Tier <- as.numeric(training.data.all$Tier)

# Split into stratified training and test datasets
training.data <- NULL
training.data.test <- NULL

# Loop through all the classes
set.seed(12) # Set a random seed to ensure consistent training and test datasets
for(c in unique(training.data.all$UKHab_Deta))
{
  # Select the subset of rows for the current class
  training.data.sub <- subset(training.data.all, UKHab_Deta==c)
  
  # Select a sample prioritising the training points from the highest tier
  n <- nrow(training.data.sub)
  
  training.data.sub <- training.data.sub[sample(n, min(n,nmax), replace=F),]
  
  # Split the data using a random sample
  subset <- random.subset(training.data.sub, 0.8)
  training.data <- rbind(training.data, training.data.sub[subset,])
  training.data.test <- rbind(training.data.test, training.data.sub[-subset,])
  rownames(training.data.test) <- NULL
}

# Remove duplicates from test dataset
#training.data.test <- training.data.test[!duplicated(training.data.test),]

## Write training_data to text file
write.table(training.data, "TrainingData/BGZ01_training_data_subset.txt", sep="\t")
write.table(training.data.test, "TrainingData/BGZ01_training_data_test_subset.txt", sep="\t")

### Classify training points using random forest ######################################################
#
#

if (!exists("zonal_stats_seg"))
{
  zonal_stats_seg <- read.table("ZS/BGZ01_zonal_stats_seg_indices.txt", sep="\t", header=T, as.is=T)
}

# Read in training and test datasets
training.data <- read.table("TrainingData/BGZ01_training_data_subset.txt", sep="\t", header=T)
training.data.test <- read.table("TrainingData/BGZ01_training_data_test_subset.txt", sep="\t", header=T)

# RF needs factors
training.data$Detailed <- as.factor(as.character(training.data$UKHab_Deta))
#training.data$Broad <- as.factor(as.character(training.data$Broad))

# Predict habitats using random forest
#Run for only the top 42 most important variables
#Note - (6:ncol) is there to select just the detailed column and the other data cols (ie cuts out the ID, Broad and Tier cols) from training.data
M.rf.detailed.all <- randomForest(Detailed ~ ., data=training.data[c(6:ncol(training.data))], na.action=na.omit)
i <- colnames(training.data) %in% c(rownames(M.rf.detailed.all$importance)[order(M.rf.detailed.all$importance, decreasing=T)][1:100],"Detailed")
M.rf.detailed <- randomForest(Detailed ~ ., data=training.data[i], na.action=na.omit)

# Calculate confusion matrix
p <- predict(M.rf.detailed, training.data.test, type="response")
confusion.matrix(training.data.test$UKHab_Deta, p)


# RUN TO HERE TO GET ACCURACY OUTPUT
#######################################

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

if (!exists("zonal_stats_seg"))
{
  zonal_stats_seg <- read.table("ZS/BGZ01_zonal_stats_seg_indices.txt", sep="\t", header=T, as.is=T)
}

# Combine results with segmentation polygons and save to new shapefile
results.rf <- data.frame(ID=zonal_stats_seg$ID,
                         A_pred=results.detailed.response1,
                         A_prob=results.detailed.prob1,
                         B_pred=results.detailed.response2,
                         B_prob=results.detailed.prob2)

segmentation.p <- merge(segmentation, results.rf, by="ID")

# Write the file to a GeoPackage
st_write(segmentation.p,
         "Outputs/BGZ1-living-england.gpkg",
         "BGZ1-living-england",
         driver="GPKG",
         delete_layer = TRUE)

rm(segmentation.p)

### Graphs ###########################################################################################
#
#

#Create a variable called confusion matrix - broad classes
cm1 <- broadclass.confusion.matrix(training.data.test$UKHab_Deta, training.data.test$UKHab_Broa, p)

C_graph1 <- barplot.confusion.matrix(cm1)

#Adjust margins
C_graph1 <- C_graph1 + theme(plot.margin = unit(c(1,1,1,1), "cm"))

C_graph1

#Create a variable called confusion matrix 2 - detailed classes
cm2 <- broadclass.confusion.matrix(training.data.test$UKHab_Deta, training.data.test$UKHab_Deta, p)

C_graph2 <- barplot.confusion.matrix(cm2)

#Adjust margins
C_graph2 <- C_graph2 + theme(plot.margin = unit(c(1,1,1,3), "cm"))

C_graph2

## Percentage plots ###############


# Create a simple percentage plot - broad classes
C_graph3 <- barplot.percent(cm1)

#Adjust margins
C_graph3 <- C_graph3 + theme(plot.margin = unit(c(1,1,1,1), "cm"))

C_graph3


# Create a simple percentage plot - detailed classes
C_graph4 <- barplot.percent(cm2)

#Adjust margins
C_graph4 <- C_graph4 + theme(plot.margin = unit(c(1,1,1,1), "cm"))

C_graph4

# Plot
#output.shapefile <- st_read("Outputs/BGZ01-living-england.gpkg", "BGZ01-living-england")
# 
# output.shapefile$A_prob<-as.factor(output.shapefile$A_prob)
# output.shapefile$Colour <- "#FFFFFF" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 0 ] <- "#FA0000" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 10 ] <- "#FA3200" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 20 ] <- "#FA7D00" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 30 ] <- "#FAAF00" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 40 ] <- "#FAD100" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 50 ] <- "#AACA09" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 60 ] <- "#94A637" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 70 ] <- "#67A637" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 80 ] <- "#70CD6A" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) > 90 ] <- "#5CFA00" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) == 100 ] <- "#11FF00" 
# output.shapefile$Colour[(as.numeric(as.character(output.shapefile$A_prob))) == 0] <- "grey"
# plot(output.shapefile, border=NA, main="Output coloured by probability per segment", col=output.shapefile$Colour)






