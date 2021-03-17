############### Zonal Statistics for RF Input ##############################
#
# Updated to replace velox with exactextraxtr 
#
# Example below for Living England BioGeographic Zone 10
#
# Author: Natural England, Evidence Earth Observation Service
# 
# Note: Work in Progress. Further improvements in automation will be implemented shortly.
#
# Adjustments likely required for application on other projects
#
# Last modified 17/03/2021
#

library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(sf)

# Set working directory to location of data inputs
setwd("")

# Load in script containing zonal stats functions
source("CODE/zonal_stats_exactextract.r")

# Define BioGeographic Zone of interest (Between 1-14)
bgz = "10"

# Load in segmentation 
segmentation <- st_read(paste0("INPUT/Segmentation/LE_BGZ",bgz,"_RFInput.gpkg"), paste0("LE_BGZ",bgz,"_RFInput"))

# Remove unwanted columns, leaving just segment ID
segment.id = segmentation.shp[,names(segmentation.shp) %in% c("ID")]

# LE Data
S2_summer <- paste0("INPUT/layers/s2_ard_Sum2020_BGZ",bgz,"_masked.img")
S2_winter <- paste0("INPUT/layers/s2_ard_Win2019-20_BGZ",bgz,"_masked.img")

S1_summer <- paste0("INPUT/layers/s1_grd_Sum2020_BGZ",bgz,".tif")
S1_winter <- paste0("INPUT/layers/s1_grd_Win2019_20_BGZ",bgz,".tif")

height <- "INPUT/layers/IHM19_DTM_10m.tif"
slope <- "INPUT/layers/IHM19_Slope_10m.tif"
aspect <- "INPUT/layers/IHM19_Aspect_10m.tif"
hand <- "INPUT/layers/HM19_HAND_10m.tif"

buildingsprox <- "INPUT/layers/Buildings_prox.tif"
foreshoreprox <- "INPUT/layers/Foreshore_prox.tif"
moorlandprox <- "INPUT/layers/Moorland_prox.tif"
roadprox <- "INPUT/layers/Road_prox.tif"
surfaceWaterprox <- "INPUT/layers/SurfaceWater_prox.tif"
tidalWaterprox <- "INPUT/layers/TidalWater_prox.tif"
woodlandprox <- "INPUT/layers/Woodland_prox.tif"

bioclim_max_temp <- "INPUT/layers/wc2_bio_BNG_05.tif"
bioclim_min_temp <- "INPUT/layers/wc2_bio_BNG_06.tif"
bioclim_annual_rainfall <- "INPUT/layers/wc2_bio_BNG_12.tif"

# list(layer_name=c(path_to_raster, stat, band1[, band2, ...]), ...)

list.rasters <- list(
  S1_summer=c(S1_summer, "mean", 1, 2),
  S1_winter=c(S1_winter, "mean", 1, 2),
  
  S2_summer=c(S2_summer, "mean", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  S2_winter=c(S2_winter, "mean", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),

  height=c(height, "mean", 1),
  slope=c(slope, "mean", 1),
  aspect=c(aspect, "mean", 1),
  hand=c(hand, "mean", 1),

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
  hand=c(hand, "min", 1),

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
  hand=c(hand, "max", 1),

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
  S1_summer=c(S1_summer, "stdev", 1, 2),
  S1_winter=c(S1_winter, "stdev", 1, 2),

  S2_summer=c(S2_summer, "stdev", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  S2_winter=c(S2_winter, "stdev", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),

  height=c(height, "stdev", 1),
  slope=c(slope, "stdev", 1),
  aspect=c(aspect, "stdev", 1),
  hand=c(hand, "stdev", 1),

  buildingsprox=c(buildingsprox, "stdev", 1),
  foreshoreprox=c(foreshoreprox, "stdev", 1),
  moorlandprox=c(moorlandprox, "stdev", 1),
  surfaceWaterprox=c(surfaceWaterprox, "stdev", 1),
  tidalWaterprox=c(tidalWaterprox, "stdev", 1),
  woodlandprox=c(woodlandprox, "stdev", 1),

  bioclim_max_temp=c(bioclim_max_temp, "stdev", 1),
  bioclim_min_temp=c(bioclim_min_temp, "stdev", 1),
  bioclim_annual_rainfall=c(bioclim_annual_rainfall, "stdev", 1))


### Zonal Stats for Segmented Polygons ################################################################
# Calculate the zonal stats for each segmented polygon.
start <- proc.time()
zonal_stats_seg <- zonal.stats.exactextract(segment.id , list.rasters, tiles=1)
proc.time()-start

# Save the results as an intermediate file
write.table(zonal_stats_seg, paste0("OUTPUT/ZonalStats/BGZ",bgz,"_zonal_stats_seg.txt"), sep="\t",row.names = FALSE)
