# ECS 132
# Rohan Skariah, Nathan Krieger, Geoffrey Alexander, Raymond Laughrey
# HW 2 Problem 5

simline<-function(u,v,nreps) {
  sumL <- 0
  for(i in 1:nreps) {
    Y = rbeta(1, u, v)
    L = 2 * sqrt(1-Y^2)
    sumL <- sumL + L
  }
  return (sumL / nreps)
}

#print(simline(0.2, 0.2, 1000))