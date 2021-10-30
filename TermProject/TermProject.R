# ECS 132 Term Project Rohan Skariah, Nathan Krieger, Raymond Laughrey, Geoffrey Cook.

#Calculates the trip distance in meters
HaversineDistance <- function(lat1, lon1, lat2, lon2) {
  # This function was taken from online
  # https://github.com/santiagomota/kaggle-taxi-II/blob/master/taxi.R
  # returns the distance in m
  REarth <- 6378137
  lat  <- abs(lat1-lat2)*pi/180
  lon  <- abs(lon1-lon2)*pi/180
  lat1 <- lat1*pi/180
  lat2 <- lat2*pi/180
  a    <- sin(lat/2)*sin(lat/2)+cos(lat1)*cos(lat2)*sin(lon/2)*sin(lon/2)
  d    <- 2*atan2(sqrt(a), sqrt(1-a))
  d    <- REarth*d
  return(d)
}

AnalyzeInitialData <- function(chunk1) {
  trip_durations <- c()
  trip_distance <- c()

  chunk1 <- chunk1[as.logical(chunk1$MISSING_DATA) == FALSE, ]
  
  # Get TRIP DISTANCE and TRIP DURATION from POLYLINE chunk1#
  for (i in 1:nrow(chunk1)) {
    distance_sum <- 0
    lonlat <- fromJSON(chunk1$POLYLINE[i])
    
    len <- length(lonlat)
    if (len > 1) {
      for (j in 1:(len - 1)) {
        distance_sum <- distance_sum + HaversineDistance(lonlat[[j]][2], lonlat[[j]][1], lonlat[[j+1]][2], lonlat[[j+1]][1])
      }
    } else distance_sum <- 0
    trip_durations <- append(trip_durations, (len - 1) * 15) # Calculated in seconds
    trip_distance <- append(trip_distance, distance_sum) # Calculated in meters
  }
  chunk1 <- cbind(chunk1, TRIP_DURATION=trip_durations)
  chunk1 <- cbind(chunk1, TRIP_DISTANCE=trip_distance)
  
  # Plot the histogram if you see fit
  # hist(chunk1$TRIP_DISTANCE, breaks = 100, freq = FALSE, xlim = c(0, 50000))
  # hist(chunk1$TRIP_DURATION, breaks = 100, freq = FALSE, xlim = c(0, 5000))
  
  # Seperate CALL TYPES into indicator variables #
  chunk1$CALL_TYPE <- as.factor(chunk1$CALL_TYPE)
  chunk1$CALL_A <- as.integer(as.integer(chunk1$CALL_TYPE) == 1)
  chunk1$CALL_B <- as.integer(as.integer(chunk1$CALL_TYPE) == 2)
  chunk1$CALL_C <- as.integer(as.integer(chunk1$CALL_TYPE) == 3)
  
  # Seperate DAY TYPES into indicator variables #
  ## NOTE: with correct data, this will be useful, not in this case because this column was calculated incorrectly ##
  chunk1$DAY_TYPE <- as.factor(chunk1$DAY_TYPE)
  chunk1$DAY_A <- as.integer(as.integer(chunk1$DAY_TYPE) == 1)
  chunk1$DAY_B <- as.integer(as.integer(chunk1$DAY_TYPE) == 2)
  chunk1$DAY_C <- as.integer(as.integer(chunk1$DAY_TYPE) == 3)
  
  print("TRIP DISTANCE")
  print(summary(chunk1$TRIP_DISTANCE))
  print("TRIP DURATION")
  print(summary(chunk1$TRIP_DURATION))
  print("CALL A")
  print(summary(chunk1$CALL_A))
  print("CALL B")
  print(summary(chunk1$CALL_B))
  print("CALL C")
  print(summary(chunk1$CALL_C))
  print("DAY A")
  print(summary(chunk1$DAY_A))
  print("DAY B")
  print(summary(chunk1$DAY_B))
  print("DAY C")
  print(summary(chunk1$DAY_C))
  return (chunk1)
}

plotDurationByCallType <- function() {
  a <- density(chunk1[chunk1$CALL_A == 1, c("TRIP_DURATION")]) # returns the density data
  b <- density(chunk1[chunk1$CALL_B == 1, c("TRIP_DURATION")]) # returns the density data
  c <- density(chunk1[chunk1$CALL_C == 1, c("TRIP_DURATION")]) # returns the density data
  
  max_val <- max(max(a$y), max(b$y), max(c$y))
  plot(a, col = "red", ylim=c(0,max_val), xlab="Trip Duration", main="Density of Trip Duration vs Call Type") # plots the results
  lines(c, col = "green") 
  lines(b, col = "yellow") 
  legend("topright", lty=c(1,2,1,2), legend = c("Call Type A", "Call Type B", "Call Type C"), col = c("red", "yellow", "green"))
}

confidenceIntervalCallTypes <- function() { 
  chunk1$CALL_TYPE <- as.factor(chunk1$CALL_TYPE)
  
  call_A_Duration <- chunk1[(as.integer(chunk1$CALL_TYPE) == 1), c("TRIP_DURATION")]
  call_B_Duration <- chunk1[(as.integer(chunk1$CALL_TYPE) == 2), c("TRIP_DURATION")]
  call_C_Duration <- chunk1[(as.integer(chunk1$CALL_TYPE) == 3), c("TRIP_DURATION")]
  
  conf_95_A <- t.test(call_A_Duration)$conf.int
  conf_95_B <- t.test(call_B_Duration)$conf.int
  conf_95_C <- t.test(call_C_Duration)$conf.int

  xlimVal <- min(length(call_A_Duration), length(call_B_Duration), length(call_C_Duration))
  plot(1, main = "95% Condifence Intervals for Call Types and Linear Fit Models", xlab = "TRIPS", ylab = "TRIP_DURATION", xlim = c(50,xlimVal), ylim = c(600, 850))
  
  abline(lm(call_A_Duration~c(1:length(call_A_Duration))), col="red")
  abline(lm(call_B_Duration~c(1:length(call_B_Duration))), col="orange")
  abline(lm(call_C_Duration~c(1:length(call_C_Duration))), col="darkgreen")
  
  densVal <- 10
  rect(-100, conf_95_A[1], xlimVal + 100, conf_95_A[2], density = densVal, col = "red")
  rect(-100, conf_95_B[1], xlimVal + 100, conf_95_B[2], density = densVal, col = "yellow")
  rect(-100, conf_95_C[1], xlimVal + 100, conf_95_C[2], density = densVal, col = "green")
  legend("bottomright", lty=c(1,2,1,2), legend = c("Call A", "Call B", "Call C"), col = c("red", "yellow", "green"))
}

CalculateBusyTime <- function() {
  #Calculates the time a driver is busy
  chunk2 <- train[1:numRowsToCalculate,5:9]
  
  trip_durations_chunk2 <- c()
  finish_time <- c()
  for (i in 1:nrow(chunk2)) {
    polyline_data <- as.list(strsplit(chunk1[[9]][i], "],")[[1]])
    polyline_data <- gsub("[[]", "", polyline_data)
    trip_durations_chunk2 <- append(trip_durations_chunk2, (length(polyline_data) - 1) * 15) # 
  }
  finish_time <- append(finish_time, chunk2$TIMESTAMP + trip_durations_chunk2)
  
  #Add columns for trip duration and trip finish time
  chunk2 <- cbind(chunk2, TRIP_DURATION=trip_durations_chunk2)
  chunk2 <- cbind(chunk2, FINISH_TIME=finish_time)
  #Was used to convert UNIX timestamp but realized we don't need to do that
  # chunk2$TIMESTAMP <- as.POSIXct(chunk2$TIMESTAMP, origin="1970-01-01")
  # FINISH_TIME <- as.POSIXct(chunk2$FINISH_TIME, origin="1970-01-01")
  
  #Removes trips that were not greater than 15 seconds and splits them by taxi id
  chunk2 <- chunk2[!(chunk2$TRIP_DURATION <= "15"), ]
  is_busy <- chunk2[,c("TAXI_ID", "TIMESTAMP", "TRIP_DURATION" , "FINISH_TIME" )]
  split_by_id <- split(is_busy, is_busy$TAXI_ID)
  
  driverWaiting <- c()
  driverBusy <- c()
  
  #Calculates the time a driver is busy as well as the time a driver is not busy
  for (i in 1:length(split_by_id)) {
    sumDuration <- 0
    for (j in 1:length(split_by_id[[i]]$TRIP_DURATION))
      sumDuration <- sumDuration + split_by_id[[i]]$TRIP_DURATION[j]
    timeDifference <- (split_by_id[[i]]$TIMESTAMP[length(split_by_id[[i]]$TIMESTAMP)] - split_by_id[[i]]$TIMESTAMP[1])
    driverWaiting <- append(driverWaiting, (timeDifference - sumDuration))
    driverBusy <- append(driverBusy, sumDuration)
    
  }
  #Histogram for time a driver is not busy
  hist(driverWaiting, breaks = 100, freq = FALSE)
  
  #Histogram for time a driver is busy
  hist(driverBusy, breaks = 50, freq = FALSE, main = "Normal Distribution of Time Driver is Busy WITHOUT Filter", xlab = "Time Driver is Busy in Seconds")
  curve(dnorm(x, mean = mean(driverBusy), sd = sd(driverBusy)), col = "red", add = TRUE)
  
  #Cleaning the data by removing anything that is not within two standard deviations
  std2 <- 2 * sd(driverBusy)
  meanBusy <- mean(driverBusy)
  driverBusy <- driverBusy[driverBusy > (meanBusy - std2)]
  driverBusy <- driverBusy[driverBusy < (meanBusy + std2)]
  
  #Histogram for time a driver is busy with data cleaned
  hist(driverBusy, breaks = 50, freq = FALSE, main = "Normal Distribution of Time Driver is Busy WITH Filter", xlab = "Time Driver is Busy in Seconds")
  curve(dnorm(x, mean = mean(driverBusy), sd = sd(driverBusy)), col = "red", add = TRUE)
  
  print(mean(driverBusy))
  print(sd(driverBusy))
  print(mean(driverWaiting))
  print(sd(driverWaiting))
}

plotMeanDurationByCallType <- function() {
  library(ggplot2)
  chunk1$CALL_TYPE <- as.factor(chunk1$CALL_TYPE)
  Sum = groupwiseMean(TRIP_DURATION ~ CALL_TYPE,
                      data   = chunk1,
                      conf   = 0.95,
                      digits = 3)
  print(Sum)
  
  qplot(x = CALL_TYPE, y = Mean, data = Sum) +
  geom_errorbar(aes(ymin = Trad.lower, ymax  = Trad.upper, width = 0.15))
}

linearModel <- function() {
  data <- chunk1[, c("TRIP_DISTANCE", "TRIP_DURATION")]
  # Using this linear fit, we can predict the TRIP_DURATION given the TRIP_DISTANCE of 1000 to 6000 in 1000 increments.
  fit <- lm(TRIP_DURATION ~ TRIP_DISTANCE, data)
  print(predict(fit, data.frame(TRIP_DISTANCE = c(1000, 2000, 3000, 4000, 5000, 6000)), interval = "confidence"))
  
  data <- chunk1[, c("TRIP_DURATION", "CALL_A", "CALL_B", "CALL_C")]
  fit <- lm(TRIP_DURATION ~ CALL_B + CALL_C, data)
  print(predict(fit, c(data.frame(CALL_B  = c(0,1,0)), data.frame(CALL_C  = c(0,0,1))), interval = "confidence"))
  
  data <- chunk1[, c("TRIP_DURATION", "CALL_A", "CALL_B", "CALL_C", "TRIP_DISTANCE")]
  fit <- lm(TRIP_DURATION ~ CALL_B + CALL_C + TRIP_DISTANCE, data)
  print(predict(fit, c(data.frame(CALL_B  = c(0,1,0)), data.frame(CALL_C  = c(0,0,1)), data.frame(TRIP_DISTANCE  = c(3000, 3000, 3000))), interval = "confidence"))
  
  ogCall <- chunk1[!is.na(chunk1$ORIGIN_CALL),  c("TRIP_DURATION", "ORIGIN_CALL")]
  fit <- lm(TRIP_DURATION ~ ORIGIN_CALL, ogCall)
  print(predict(fit, data.frame(ORIGIN_CALL = c(10000, 30000, 50000, 100000)), interval = "confidence"))
  
  ogStand <- chunk1[!is.na(chunk1$ORIGIN_STAND), c("TRIP_DURATION", "ORIGIN_STAND")]
  fit <- lm(TRIP_DURATION ~ ORIGIN_STAND, ogStand)
  print(predict(fit, data.frame(ORIGIN_STAND = c(5, 10, 15, 100)), interval = "confidence"))
  
  # Predict for DISTANCE = 3000, CALL TYPE = B, ORIGIN STAND = 50 (which already imples call type is B)
  data <- chunk1[!is.na(chunk1$ORIGIN_STAND), c("TRIP_DURATION", "TRIP_DISTANCE", "ORIGIN_STAND", "TAXI_ID")]
  fit <- lm(TRIP_DURATION ~ TRIP_DISTANCE + ORIGIN_STAND + TAXI_ID, data)
  print(summary(fit))
  print(MAPE(fit$fitted.values, data$TRIP_DURATION))
  
  data <- chunk1[!is.na(chunk1$ORIGIN_STAND), c("TRIP_DURATION", "TRIP_DISTANCE", "ORIGIN_STAND")]
  fit <- lm(TRIP_DURATION ~ TRIP_DISTANCE + ORIGIN_STAND, data)
  print(summary(fit))
  print(MAPE(fit$fitted.values, data$TRIP_DURATION))
  
  data <- chunk1[, c("TRIP_DURATION", "TRIP_DISTANCE")]
  data$TRIP_DURATION <- data$TRIP_DURATION
  print("lm")
  fit <- lm(TRIP_DURATION ~ TRIP_DISTANCE, data)
  print(summary(fit))
  print(MAPE(fit$fitted.values, data$TRIP_DURATION))
  
  data <- chunk1[!is.na(chunk1$CALL_A), c("TRIP_DISTANCE", "TRIP_DURATION", "CALL_A")]
  predictionValue <- c(3000,0)
  print("PolyLin")
  qePolyLinOut <- qePolyLin(data,"TRIP_DURATION")
  print(qePolyLinOut$testAcc)
}

machineLearningModel <- function() {
  data  <- chunk1[, c("TRIP_DISTANCE", "TRIP_DURATION")]
  predictingVars  <- c("TRIP_DISTANCE", "TRIP_DURATION")
  TnD  <- chunk1[, predictingVars]
  predictionValue  <- c(2000)
  print("KNN")
  kNNOut  <- qeKNN(TnD , "TRIP_DURATION")
  print(kNNOut$testAcc)
  print(predict(kNNOut , predictionValue))

  data <- chunk1[, c("TRIP_DURATION", "TRIP_DISTANCE", "CALL_A", "CALL_B", "CALL_C")]

  # Predicting from TRIP DISTANCE, ORIGIN STAND, and technically CALL TYPE
  data <- chunk1[!is.na(chunk1$ORIGIN_STAND), c("TRIP_DURATION", "TRIP_DISTANCE", "ORIGIN_STAND")]
  predictionValue <- c(5000, 10)

  mlFit <- qePolyLin(data, "TRIP_DURATION")
  print(mlFit$testAcc)

  mlFit <- qeKNN(data, "TRIP_DURATION")
  print(mlFit$testAcc)
  print(predict(mlFit, predictionValue))

  mlFit <- qeRF(data, "TRIP_DURATION")
  print(mlFit$testAcc)
  print(predict(mlFit, predictionValue))

  mlFit <- qeNeural(data, "TRIP_DURATION")
  print(mlFit$testAcc)
  print(predict(mlFit, predictionValue))

  mlFit <- qeGBoost(data, "TRIP_DURATION")
  print(mlFit$testAcc)
  print(predict(mlFit, predictionValue))

  print(qeCompare(data,'TRIP_DURATION', c('qePolyLin','qeKNN', 'qeNeural', 'qeRF', 'qeGBoost'), 10))
  print(MAPE(lm(TRIP_DURATION ~ TRIP_DISTANCE + ORIGIN_STAND, data)$fitted.values, data$TRIP_DURATION))
  data <- chunk1[!is.na(chunk1$ORIGIN_CALL), c("TRIP_DURATION", "TRIP_DISTANCE", "CALL_A", "CALL_B", "CALL_C", "ORIGIN_CALL")]

  fit <- qePolyLin(data, "TRIP_DURATION")
  print(fit$testAcc)

  data <- chunk1[, c("TRIP_DISTANCE", "TRIP_DURATION")]
  print(qeCompare(data,'TRIP_DURATION', c('qePolyLin','qeKNN', 'qeNeural', 'qeRF'), 10))

  predictionValue <- 5500
  print("KNN")
  kNNOut <- qeKNN(data, "TRIP_DURATION")
  print(kNNOut$testAcc)
  print(predict(kNNOut, predictionValue))

  print("PolyLin")
  polyLinOut <- qePolyLin(data, "TRIP_DURATION")
  print(polyLinOut$testAcc)
  print(predict(polyLinOut, predictionValue))

  print("RF")
  rFOut <- qeRF(data, "TRIP_DURATION")
  print(rFOut$testAcc)
  print(predict(rFOut, predictionValue))

  print("Neural")
  neuralOut <- qeNeural(data, "TRIP_DURATION")
  print(neuralOut$testAcc)
  print(predict(neuralOut, predictionValue))
  
  data <- chunk1[, c("TRIP_DISTANCE", "CALL_A", "CALL_B", "CALL_C", "TRIP_DURATION")]
  predictionValue <- c(3000, 0, 1, 0)
  
  print("gBoost")
  gBoostOut <- qeGBoost(data, "TRIP_DURATION")
  print(gBoostOut$testAcc)
  print(predict(gBoostOut, predictionValue))
  
  print(qeCompare(data,'TRIP_DURATION', c('qePolyLin','qeKNN', 'qeNeural', 'qeRF', 'qeGBoost'), 10))
}

comparingNeural <- function() {
  ### Testing non default hyperparameters for qeNeural ##

  # Trying  to find  which  hyper  parameters  work  best  for  qeNeural.
  data  <- chunk1[!is.na(chunk1$ORIGIN_STAND), c("TRIP_DURATION", "TRIP_DISTANCE", "ORIGIN_STAND")]
  predictionValue <- c(3000, 50)
  
  mlFit1 <- qeNeural(data, "TRIP_DURATION")
  mlFit2 <- qeNeural(data, "TRIP_DURATION",nEpoch=10)
  mlFit3 <- qeNeural(data, "TRIP_DURATION",nEpoch=50)
  mlFit4 <- qeNeural(data, "TRIP_DURATION", hidden = c(50, 50))
  mlFit5 <- qeNeural(data, "TRIP_DURATION", hidden = c(500, 500))
  mlFit6 <- qeNeural(data, "TRIP_DURATION", hidden = c(1000, 1000))
  
  print(mlFit1$testAcc)
  print(predict(mlFit1, predictionValue))
  print(mlFit2$testAcc)
  print(predict(mlFit2, predictionValue))
  print(mlFit3$testAcc)
  print(predict(mlFit3, predictionValue))
  print(mlFit4$testAcc)
  print(predict(mlFit4, predictionValue))
  print(mlFit5$testAcc)
  print(predict(mlFit5, predictionValue))
  print(mlFit6$testAcc)
  print(predict(mlFit6, predictionValue))
}

originCallandStandAndML <- function() {
  
  ogCall <- chunk1[!is.na(chunk1$ORIGIN_CALL),  c("TRIP_DURATION", "ORIGIN_CALL")]
  ogStand <- chunk1[!is.na(chunk1$ORIGIN_STAND), c("TRIP_DURATION", "ORIGIN_STAND")]
  
  hist(ogCall$TRIP_DURATION, breaks = 50, freq = FALSE, main = "Trip Duration with Origin Call Value")
  hist(ogStand$TRIP_DURATION, breaks = 50, freq = FALSE, main = "Trip Duration with Origin Stand Value")
  
  ## PREDICTING FROM ORIGIN CALL AND ORIGIN STAND
  print("KNN")
  kNNOut  <- qeKNN(ogCall , "TRIP_DURATION")
  print(kNNOut$testAcc)
  print(predict(kNNOut , 1000))
  print("KNN")
  kNNOut  <- qeKNN(ogStand , "TRIP_DURATION")
  print(kNNOut$testAcc)
  print(predict(kNNOut , 100))
  
  print("RF")
  rfOut  <- qeRF(ogCall , "TRIP_DURATION")
  print(rfOut$testAcc)
  print(predict(rfOut , 1000))
  print("RF")
  rfOut  <- qeRF(ogStand , "TRIP_DURATION")
  print(rfOut$testAcc)
  print(predict(rfOut , 100))
  
  print("Neural")
  neuralOut  <- qeNeural(ogCall , "TRIP_DURATION")
  print(neuralOut$testAcc)
  print(predict(neuralOut , 1000))
  print("Neural")
  neuralOut  <- qeNeural(ogStand , "TRIP_DURATION")
  print(neuralOut$testAcc)
  print(predict(neuralOut , 100))
}

cleanData <- function() {
  if (clean) {
    chunk1 <- chunk1[chunk1$TRIP_DURATION < maxDur,]
    chunk1 <- chunk1[chunk1$TRIP_DURATION > minDur,]
    chunk1 <- chunk1[as.logical(chunk1$MISSING_DATA) == FALSE, ]
  }
}


###############################EXECUTE CODE######################################################

# Set the working directory and load in the train data as you see fit.
# setwd("D:/UC Davis/Computer Science/ECS 132/Term Project")
# train <- read.csv('../train.csv')

# Libraries used
library(regtools)
library(datetime)
library(rjson)
library(data.table)
library(randomForest)
library(gbm)
library(keras)
library(glmnet)
library(tensorflow)
library(ggplot2)
library(rcompanion)
library(ggmap)

# Sample size for data calculations
numRowsToCalculate <- 10000

cat("Rows Calculated:", numRowsToCalculate, "\n")

# Sample the data or take the fist 'numRowsToCalculate' rows from the csv itself.
chunk1 <- train[sample(nrow(train), numRowsToCalculate), ]
#chunk1 <- read.csv(file = "../train.csv", nrows = numRowsToCalculate)

# Analyze and populate TRIP_DURATION, TRIP_DISTANCE, CALL_A, CALL_B, CALL_C, DAY_A, DAY_B, DAY_C
chunk1 <- AnalyzeInitialData(chunk1)

clean <- TRUE
minDur <- 15
maxDur <- 10000
cleanData()

CalculateBusyTime()
plotMeanDurationByCallType()

plotDurationByCallType()
confidenceIntervalCallTypes()

linearModel()
comparingNeural()
machineLearningModel()
originCallandStandAndML()