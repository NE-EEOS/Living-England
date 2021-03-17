# Living-England
# Satellite-based habitat mapping of England in R.

<span>&#9888;</span>  **IMPORTANT** <span>&#9888;</span> 

This is an active project. The code within this repository will potentially be updated without prior warning. Further details on the deployment of the scripts will be provided in due course.

The scripts are designed specifically for the Living England project, and are likely to need adjusting for application elsewhere.

- LivingEngland-ZonalStats-Master.r calculates the zonal statistics (mean, min, max, stdev) for numerous raster layers for each polygon in a segmentation. Script uses functions from zonal_stats_exactextract.R

- LivingEngland-RandomForest-Master.r runs a Random Forest algorithm using in-situ training data and the zonal stats calculated previously. Script uses functions from user_producer_accuracy.R

Please let us know if you are using the scripts - drop us a line at earth.observation@naturalengland.org.uk
