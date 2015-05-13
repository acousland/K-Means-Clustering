#########################################################
# K-Means Optimisation 
#
# Run a KMeans clusering algorithm on the fault dataset 
# to identify the fault characteristics
# 
# 503 is current best result using
# 2 Clusters on only RMSI1
#
# - Aaron Cousland 14/05/2015
########################################################

require (RODBC)        # Load RODBC package
require (lubridate)    # Required to manipulate dates
require (dplyr)       # Required for performance measurement

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection,"SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/Mar/15 08:00:00 AM' and '17/Mar/15 03:00:00 PM';")
odbcCloseAll()

logger.results.backup <- logger.results

ptm <- proc.time()

for (i in seq(1,5,by=1))
{
  # Run kmeans on dataset. Rounding is required to stop non-convergance error.
  logger.results <- logger.results.backup
  logger.results[,c(2,3,5,6)] <- round(logger.results[,c(2,3,5,6)], digits=2)
  kmeans.results <- kmeans(logger.results[,c(2)],i)
  
  # Display centroid centers
  kmeans.results$centers
  
  # Write clusters back to results
  logger.results <- data.frame(logger.results, kmeans.results$cluster)
  
  # Pick out cluster with largest value as it is likley to be the fault cluster
  target.cluster <- which.max(t(kmeans.results$centers[,1]))
  
  # Show how many records are in that cluster
  nrow(subset(logger.results, logger.results$kmeans.results.cluster == target.cluster))  
  
  # Mark target cluster as fault data
  logger.results$PrFault <- ifelse(logger.results$kmeans.results.cluster == target.cluster,1,0)
  
  # Measure performance
  performance <- logger.results %>%
    group_by(FAULT) %>%
    summarise (Score = sum(PrFault))
  
  write.table(t(c(i,performance$Score,performance$Score[2]-performance$Score[1])),file="KMeans_Optimisation_Results.csv", sep=",",append=T, row.names=F, col.names = F)
}
proc.time() - ptm

results <- read.csv ("KMeans_Optimisation_Results.csv")
plot(results[,1],results[,4],ylim=c(0,max(results[,4])))

