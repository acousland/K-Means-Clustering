#########################################################
# K-Means Implementation 
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


logger.results <- logger.results.backup

# Order by timestamp and force local timestamp
logger.results <- logger.results[with(logger.results, order(logger.results$TS)),]
logger.results$TS <- force_tz(logger.results$TS,"UTC")

# Superimpose load current on fault current
logger.results$RMSI1 <- (logger.results$RMSI1 + logger.results$RMSI2)

# Run kmeans on dataset. Rounding is required to stop non-convergance error.
logger.results[,c(2,3,5,6)] <- round(logger.results[,c(2,3,5,6)], digits=2)
kmeans.results <- kmeans(logger.results[,c(2)],5)

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

print(paste("Score =",performance$Score[2]-performance$Score[1],"/",sum(logger.results$FAULT==TRUE)))
performance$Score

# Interrogate results
StartTime <- force_tz(as.POSIXct("2015-03-17 13:18:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
EndTime <- force_tz(as.POSIXct("2015-03-17 13:19:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
logger.results.display <- subset(logger.results, logger.results$TS >= StartTime & logger.results$TS <= EndTime)

plot(logger.results.display$TS,logger.results.display$RMSI1, type="l")
#plot(logger.results.validation.subset$TS,logger.results.validation.subset$FtDetected*max(logger.results.validation.subset$RMSI1))
polygon(logger.results.display$TS,logger.results.display$PrFault*max(logger.results.display$RMSI1), col =rgb(1,0,0,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
#polygon(logger.results.validation.subset$TS,logger.results.validation.subset$FAULT*max(logger.results.validation.subset$RMSI1), col =rgb(0,0,1,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
axis(4)


