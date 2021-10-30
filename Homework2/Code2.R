# ECS 132
# Rohan Skariah, Nathan Krieger, Geoffrey Alexander, Raymond Laughrey
# HW 2 Problem 2

simpark<-function(p,q,s,nEpochs) {
  # p is probability car leaves
  # q is probability car arrives
  # s is num spaces
  
  parkingSpots <- rep(1, s) #vector of all 1s
  parkedCars <- s
  numFailedToPark <- 0
  numEmptyAtMiddle <- c()
  distanceToEnd <- c()
  carsTryToPark <- 0
  carsFailToFindSpace <- 0

  for (time in 1:nEpochs) {
    #for all the spaces calculate if a car leaves
    for(i in 1:parkedCars)
    {
      if(runif(1) < p) {
        if (parkingSpots[i] == 1) {
          parkedCars <- parkedCars - 1
          parkingSpots[i] <- 0
        }
      }
    }
    
    numEmptyAtMiddle <- append(numEmptyAtMiddle, (s-parkedCars))

    #if a car tries to park 
    if(runif(1) < q) {
      carsTryToPark <- carsTryToPark + 1
      if (parkedCars >= s) { #if there are no parking spots 
        carsFailToFindSpace <- carsFailToFindSpace + 1
      }
      else {#there are parking spots 
        parkingSpotFound <- find_index(parkingSpots)
        parkingSpots[parkingSpotFound] <- 1
        distanceToEnd <- append(distanceToEnd, (s-parkingSpotFound))
        parkedCars <- parkedCars + 1
      }
    }
  }

  LRA_Empty_Spots <- mean(numEmptyAtMiddle)
  LRA_Distance_To_End <- mean(distanceToEnd)
  LR_Prop_Cars_Fail_To_Find_Space <- (carsFailToFindSpace / carsTryToPark)

  output <- c(LR_Prop_Cars_Fail_To_Find_Space, LRA_Distance_To_End, LRA_Empty_Spots)
  
  return (output)
}

#returns index of first occurence of a 0 in the input vector 
find_index <- function(sum_vec)
{
  for(i in 1:length(sum_vec)) {
    if(sum_vec[i] == 0)
      return (i)
  }
  return -1
}

#print(simpark(0.1, 0.5, 5, 10000))