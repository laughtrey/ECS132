# ECS 132
# Rohan Skariah, Nathan Krieger, Geoffrey Alexander, Raymond Laughrey
# HW 2 Problem 7

simparkc <- function(mu1, mu2, s, timeLim) {
  simTime <- 0
  parkedCars <- s
  carsTryToPark <- 0
  carsFailToFindSpace <- 0
  numEmptySpaces <- c()
  distanceToEnd <- c()
  
  # Create the matrix with all the parking spots full with exponential departure times

  #Row with <event> and Random var
  eventLog <- matrix(,nrow = s, ncol = 2)
  parkingSpots <- matrix(,nrow = s, ncol = 2)
  for (i in 1:s) {# creates s  rows of departure events 
    eventLog[i, ] <- c(0, runif(1, min = 0, max = mu2 * 2))
    parkingSpots[i, ] <- c(1, eventLog[i, 2])
  }

  for (i in 1:1) {
    eventLog <- rbind(eventLog, c(1, rexp(1, 1/mu1) + simTime)) #Add first new arrival
  }
  
  while(1) {   
    #asserting that eventlog is a matrix and not a vector (case where row = 1)  
    if (is.vector(eventLog)) {
      eventLog <- matrix(as.vector(eventLog), nrow = 1, ncol = 2)
    } else {
      eventLog <- eventLog[order(eventLog[,2]),]
    }
    removedEvent <- eventLog[1,] #row of next event
    simTime <- removedEvent[2] #time value of next event
    # If the next event is the car arrival
    if(removedEvent[1] == 1) { # if next event is arrival
      # in order to calculate LRA of empty spaces the instant after cars arrive
      numEmptySpaces <- append(numEmptySpaces, (s-parkedCars))

      # Schedule arrival event with exponential time with mean mu1
      arrivalTime <- rexp(1, 1/mu1) + simTime
      eventLog <- rbind(eventLog, c(1, rexp(1, 1/mu1) + simTime)) #Add new arrival

      # This car is trying to park 
      carsTryToPark <- carsTryToPark + 1
      if (parkedCars >= s) { #if there are no parking spots 
        carsFailToFindSpace <- carsFailToFindSpace + 1
      }
      else { #there are parking spots 
        parkingSpotFound <- find_index(parkingSpots, simTime, 1)
        parkingSpots[parkingSpotFound, 1] <- 1 #notes where car parked in vector

        distanceToEnd <- append(distanceToEnd, (s-parkingSpotFound))
        parkedCars <- parkedCars + 1
        # Shedule departure event with uniform time with mean mu2
        eventLog <- rbind(eventLog, c(0, runif(1, min = 0, max = mu2 * 2) + simTime))
        parkingSpots[parkingSpotFound, 2] <- eventLog[nrow(eventLog), 2]
      }
    }
    else if(removedEvent[1] == 0) {
      # mark space as available
      parkingSpotFound <- find_index(parkingSpots, simTime, 0)
      parkingSpots[parkingSpotFound, 1] <- 0 #notes where car parked in vector
      parkingSpots[parkingSpotFound, 2] <- 0 
      parkedCars <- parkedCars - 1
    }
 
    eventLog <- eventLog[-1,] # delete the event from event log

    if (simTime > timeLim)
      break
  }

  LR_Prop_Cars_Fail_To_Find_Space <- (carsFailToFindSpace / carsTryToPark)
  LRA_Distance_To_End <- mean(distanceToEnd)
  LRA_Empty_Spots <- mean(numEmptySpaces)

  output <- c(LR_Prop_Cars_Fail_To_Find_Space, LRA_Distance_To_End, LRA_Empty_Spots)

  return (output)
}


#returns index of first occurence of a 0 in the input vector 
find_index <- function(parkingSpotsMatrix, departureTime, isArrival) {
  if (isArrival) {
    for(i in 1:nrow(parkingSpotsMatrix)) {
      if(parkingSpotsMatrix[i, 1] == 0)
        return (i)
    }
  }
  else {
    for(i in 1:nrow(parkingSpotsMatrix)) {
      if(as.double(parkingSpotsMatrix[i, 2]) == departureTime)
        return (i)
    }
  }
  return (-1)
}

#print(simparkc(1, 3, 5, 10000))
