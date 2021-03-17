############### Automated object-based classification using RF ##############################
#
# Updated to removed "impute" function and create seasonal outputs in cloud afflicted regions
#
# Example below for Living England BioGeographic Zone 10
#
# Author: Natural England, Evidence Earth Observation Service
# 
# Note: Work in Progress. Further improvements in automation will be implemented shortly.
#
# Adjustments likely required for application on other projects
#
# Last modified 16/03/2021
#

library(rgdal)
library(raster)
library(rgeos)
library(randomForest)
library(sp)
library(sf)

# Set working directory to location of data inputs
setwd()

# Load in script containing accuracy functions
source("CODE/user_producer_accuracy.r")

# Define BioGeographic Zone of interest
bgz = "10"

# Load in training data
training.data.habitat.gpkg <- st_read(paste0("INPUT/TrainingData/BGZ",bgz,"_trainingData_validated.gpkg"),
                                     paste0("BGZ",bgz,"_trainingData_validated"))

# Load in LE segmentation shapefile
segmentation <- st_read(paste0("INPUT/Segmentation/LE_BGZ",bgz,"_RFInput.gpkg"), paste0("LE_BGZ",bgz,"_RFInput"))

# Load in zonal stats txt file as master dataset
zonal_stats_seg <- read.table(paste0("INPUT/ZonalStats/BGZ",bgz,"_zonal_stats_seg.txt"), sep="\t", header=T)

# Calculate NDVI and NDWI columns for summer and winter
zonal_stats_seg.master$S2_summer_ndvi <- with(zonal_stats_seg.master, (S2_summer_mean_band7 - S2_summer_mean_band3)/(S2_summer_mean_band7 + S2_summer_mean_band3))
zonal_stats_seg.master$S2_summer_ndwi <- with(zonal_stats_seg.master, (S2_summer_mean_band7 - S2_summer_mean_band9)/(S2_summer_mean_band7 + S2_summer_mean_band9))
zonal_stats_seg.master$S2_winter_ndvi <- with(zonal_stats_seg.master, (S2_winter_mean_band7 - S2_winter_mean_band3)/(S2_winter_mean_band7 + S2_winter_mean_band3))
zonal_stats_seg.master$S2_winter_ndwi <- with(zonal_stats_seg.master, (S2_winter_mean_band7 - S2_winter_mean_band9)/(S2_winter_mean_band7 + S2_winter_mean_band9))

# Ensure that categorical data does not have any missing or inf values
zonal_stats_seg.master[is.na(zonal_stats_seg.master)] <- 0
zonal_stats_seg.master[sapply(zonal_stats_seg.master, is.infinite)] <- 0

# Create separate summer and winter datasets for classifying cloud affected locations
zonal_stats_seg.summer = !grepl("winter",colnames(zonal_stats_seg.master))
zonal_stats_seg.summer = zonal_stats_seg.master[,zonal_stats_seg.summer]
zonal_stats_seg.winter = !grepl("summer",colnames(zonal_stats_seg.master))
zonal_stats_seg.winter = zonal_stats_seg.master[,zonal_stats_seg.winter]

# Output updated tables containing calculated indices
write.table(zonal_stats_seg.master, paste0("OUTPUT/Zonal_Indices/BGZ",bgz,"_zonal_stats_seg_indices_master.txt"), sep="\t", row.names=FALSE)
write.table(zonal_stats_seg.summer, paste0("OUTPUT/Zonal_Indices/BGZ",bgz,"_zonal_stats_seg_indices_summer.txt"), sep="\t", row.names=FALSE)
write.table(zonal_stats_seg.winter, paste0("OUTPUT/Zonal_Indices/BGZ",bgz,"_zonal_stats_seg_indices_winter.txt"), sep="\t", row.names=FALSE)

# Remove unneccesary columns 
training.data.gpkg <- training.data.habitat.gpkg[c("Training_P", "Survey_Dat", paste0("BGZ",bgz,"_trainingdata_final_IDs_UKHab_Broa"), 
                                                                                      paste0("BGZ",bgz,"_trainingdata_final_IDs_UKHab_Deta"))]
colnames(training.data.gpkg)=c("Training_Point","Survey_Date","Broad_Habitat","Detailed_Habitat","geom")
# Identify the segmented polygons the training points fall within and extract the zonal statistics from these
training.data.ids <- as.numeric(st_within(training.data.gpkg, segmentation))

# Functions required in main RF function
responseNFromProbs <- function(df, n=1) {
  columns <- colnames(df)
  response <- apply(df, MARGIN=1, FUN=function(x) {columns[order(x, decreasing=TRUE)[n]]})
  return (response)
}

probNFromProbs <- function(df, n=1) {
  response <- apply(df, MARGIN=1, FUN=function(x) {sort(x, decreasing=TRUE)[n]})
  return (response)
}

### Function for running LE Random Forest algorithm ###
# zonal_stats_seg = relevant zonal_stats_seg dataset, either .master, .summer or .winter 
# input.type = either "master", "summer" or "winter" (for input/output tasks)
# nmax = Maximum number of training points used per class (default = 500)
# seed = numerical value used to divide training data between training and test
# num.variables = number variables to use for random forest calculation (default = 50)

## Function also uses bgz, training.data.gpkg, training.data.ids and segmentation from local environment

LE.random.forest <- function(zonal_stats_seg,
                             input.type,
                             nmax=500,
                             seed,
                             num.variables=50){
  nmax <- nmax
  
  zonal_stats_seg = zonal_stats_seg
  
  if (!exists("zonal_stats_seg"))
  {
    zonal_stats_seg <- read.table(paste0("OUTPUT/Zonal_Indices/BGZ",bgz,"_zonal_stats_seg_indices_",input.type,".txt"), sep="\t", header=T, as.is=T)
  }
  
  training.data <- cbind(as.data.frame(training.data.gpkg),zonal_stats_seg[training.data.ids,])
  
  # Remove the column "geom" and "Survey_Date"
  training.data <- training.data[names(training.data)!="geom"]
  
  # Write the training data zonal stats to a txt file 
  write.table(training.data, paste0("OUTPUT/TrainingZonalStats/BGZ",bgz,"_zonal_stats_seg_training_",input.type,".txt"), sep="\t", row.names=FALSE)
  
  training.data.all <- training.data
  
  # Remove rows with mising values
  training.data.all <- training.data.all[complete.cases(training.data.all),]
  
  # Split into stratified training and test datasets
  training.data <- NULL
  training.data.test <- NULL
  
  set.seed(seed) 
  
  for(c in unique(training.data.all$Detailed_Habitat)){
    # Select the subset of rows for the current class
    training.data.sub <- subset(training.data.all, Detailed_Habitat==c)
    
    # Select a sample prioritising the training points from the highest tier
    n <- nrow(training.data.sub)
    
    training.data.sub <- training.data.sub[sample(n, min(n,nmax), replace=F),]
    
    # Split the data using a random sample
    subset <- random.subset(training.data.sub, 0.8)
    training.data <- rbind(training.data, training.data.sub[subset,])
    training.data.test <- rbind(training.data.test, training.data.sub[-subset,])
    rownames(training.data.test) <- NULL
  }
  
  ## Write training_data to text file
  write.table(training.data, paste0("OUTPUT/TrainingDataSplit/BGZ",bgz,"_training_data_subset_",input.type,".txt"), sep="\t", row.names=FALSE)
  write.table(training.data.test, paste0("OUTPUT/TrainingDataSplit/BGZ",bgz,"_training_data_test_subset",input.type,".txt"), sep="\t", row.names=FALSE)
  
  if (!exists("zonal_stats_seg"))
  {
    zonal_stats_seg <- read.table(paste0("OUTPUT/Zonal_Indices/BGZ",bgz,"_zonal_stats_seg_indices_",input.type,".txt"), sep="\t", header=T, as.is=T)
  }
  
  training.data <- read.table(paste0("OUTPUT/TrainingDataSplit/BGZ",bgz,"_training_data_subset_",input.type,".txt"), sep="\t", header=T)
  training.data.test <- read.table(paste0("OUTPUT/TrainingDataSplit/BGZ",bgz,"_training_data_test_subset",input.type,".txt"), sep="\t", header=T)
  
  # RF needs factors
  training.data$Detailed <- as.factor(as.character(training.data$Detailed_Habitat))
  
  # Predict habitats using random forest
  # Note - (6:ncol) is there to select just the 6 data cols (ie cuts out the ID, Broad and Tier cols) from training.data
  M.rf.detailed.all <- randomForest(Detailed ~ ., data=training.data[c(6:ncol(training.data))], na.action=na.omit)
  i <- colnames(training.data) %in% c(rownames(M.rf.detailed.all$importance)[order(M.rf.detailed.all$importance, decreasing=T)][1:num.variables],"Detailed")
  M.rf.detailed <- randomForest(Detailed ~ ., data=training.data[i], na.action=na.omit)
  
  # Calculate confusion matrix
  p <- predict(M.rf.detailed, training.data.test, type="response")

  # Predict classes for all polygons
  results.detailed.probs <- predict(M.rf.detailed, zonal_stats_seg,
                                    type="vote", norm.votes=TRUE,
                                    progress="text")
  
  # Extract preditions and probabilities
  results.detailed.response1 <- responseNFromProbs(results.detailed.probs, n=1)
  results.detailed.prob1 <- probNFromProbs(results.detailed.probs, n=1)*100
  results.detailed.response2 <- responseNFromProbs(results.detailed.probs, n=2)
  results.detailed.prob2 <- probNFromProbs(results.detailed.probs, n=2)*100
  
  if (!exists("zonal_stats_seg"))
  {
    zonal_stats_seg <- read.table(paste0("OUTPUT/Zonal_Indices/BGZ",bgz,"_zonal_stats_seg_indices_",input.type,".txt"), sep="\t", header=T, as.is=T)
  }
  
  # Creat results df
  results.rf <- data.frame(ID=zonal_stats_seg$ID,
                           A_pred=results.detailed.response1,
                           A_prob=results.detailed.prob1,
                           B_pred=results.detailed.response2,
                           B_prob=results.detailed.prob2)
  
  # Remove possible duplicated colnames
  dup.cols = list("A_pred","A_prob","B_pred","B_prob")
  segmentation = segmentation[,!(names(segmentation) %in% dup.cols)]
  
  # Merge RF and segmentation
  segmentation.p <- merge(segmentation, results.rf, by="ID")
  
  # Write the file to a GeoPackage
  st_write(segmentation.p,
           paste0("OUTPUT/Classification/BGZ",bgz,"-living-england-",input.type,".gpkg"),
           paste0("BGZ",bgz,"-living-england-",input.type),
           driver="GPKG",
           delete_layer = TRUE)
  
  rm(segmentation.p)
  
  return(list("test" = training.data.test, "p" = p))
}

### Run for master dataset
outputs.master = LE.random.forest(zonal_stats_seg = zonal_stats_seg.master,
                                  input.type = "master",
                                  nmax = 500,
                                  seed = 3,
                                  num.variables = 50)

# Extract Variables for confusion matrix
test.data.master = outputs.master$test
p.data.master = outputs.master$p

confusion.matrix(test.data.master$Detailed_Habitat, p.data.master)
###########################

### Run for summer dataset
outputs.summer = LE.random.forest(zonal_stats_seg = zonal_stats_seg.summer,
                                  input.type = "summer",
                                  nmax = 500,
                                  seed = 3,
                                  num.variables = 50)


test.data.summer = outputs.summer$test
p.data.summer = outputs.summer$p

confusion.matrix(test.data.summer$Detailed_Habitat, p.data.summer)
###########################

### Run for summer dataset
outputs.winter = LE.random.forest(zonal_stats_seg = zonal_stats_seg.winter,
                                  input.type = "winter",
                                  nmax = 500,
                                  seed = 3,
                                  num.variables = 50)

# Extract Variables for confusion matrix
test.data.winter = outputs.winter$test
p.data.winter = outputs.winter$p

confusion.matrix(test.data.winter$Detailed_Habitat, p.data.winter)
