require (RODBC)        # Load RODBC package
require (lubridate)   # Required to manipulate dates

#Close any existing connections
odbcCloseAll()

# Create a connection to the database called
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

ptm <- proc.time()
# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection, 
                                    "SELECT * FROM ELSPEC.RMS_TRAINING where ts between '01/Feb/15 09:00:00 AM' and '28/Mar/15 09:00:00 AM';")
proc.time() - ptm


# Run kmeans on dataset and record run time
ptm <- proc.time()
kmeans.results <- kmeans(logger.results[,c(2,3,5,6)],10)
proc.time() - ptm

# Display centroid centers
kmeans.results$centers

# Write clusters back to results
logger.results <- data.frame(logger.results, kmeans.results$cluster)

# Pick out cluster with largest value as it is likley to be the fault cluster
target.cluster <- which.max(t(kmeans.results$centers[,1]))

nrow(subset(logger.results, logger.results$kmeans.results.cluster == target.cluster))	

# Create a subset of the results with only the fault identified data
logger.results.faults <- subset(logger.results, logger.results$kmeans.results.cluster == target.cluster)

# Set the grouping times to remove multiple readings per cluster
test.interval <- 1
logger.results.faults$test.marker <- 0

# Will break if too many rows are put through. Need a better way.
for (i in 1:(nrow(logger.results.faults)-1))
{
 if(logger.results.faults$TS[i]+ minutes(test.interval) < logger.results.faults$TS[i+1])
  {
   logger.results.faults$test.marker[i] <- 1
  }
}

nrow(subset(logger.results.faults, logger.results.faults$test.marker == 1))







# End of plan - Crap code below

plot(logger.results.faults$TS,logger.results.faults$RMSI1)

kmeans.results.2 <- kmeans(logger.results.faults$TS,1000)




test.times <- as.character(as.POSIXct(kmeans.results.2$centers,origin = "1970-01-01",tz = "GMT"))

write.csv(test.times, file = "TestTimes2.csv")

