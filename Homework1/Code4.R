# This problem is adapted from CS research on computer viruses. But it will also give you a glimpse into the various epidemiology models of Covid-19.

# We have c computers.
# Time is slotted. Let Xi denote the number of infected computers at the beginning of Slot i.
# In the beginning, Time 0, one of the computers is infected with a virus.
# Halfway through any epoch, each infected computer may infect each of the noninfected computers. If there are currently k infected computers, any given noninfected one will stay noninfected with probability rk, independently.
# Write a function with call form

# simvir(c,r,m,n,nreps) 
# that will return the approximate probability that there are n noninfected computers at the start of epoch m.

simvir <- function(c,r,m,n,nreps) {
  # c is the number of computers
  # r is part of probability (0-1)
  # m is epoch in which to return the probability of
  # n is the number of noninfected computers
  # nreps is the number of times the simulation has to run

  infectedCompTotal <- vector() #empty vector 
  numInfected <- 1
  epoch <- 0
  
  for (i in 1:nreps) {
    # Below is 1 simulation
    epoch <- 0
    numInfected <- 1
    while (1) { #each loop represents 1 epoch. Break when m epochs reached
      nonInfected <- (c - numInfected)
      tempNumInfected <- numInfected
      for (comps in 1:nonInfected) {
        if (infectionChance(r, tempNumInfected)) numInfected <- numInfected + 1
      }      

      epoch <- epoch + 1 # time is incremented

      if (numInfected >= c) {
        infectedCompTotal <- append (infectedCompTotal, c)
        break
      }
      if (epoch >= m) {
        infectedCompTotal <- append (infectedCompTotal, numInfected)
        break
      }
    }
  }
  return (mean((c - (infectedCompTotal)) >= n))
}

infectionChance <- function(r, numInfected) {
  X <- runif(1) # random value from 0-1
  prob <- r ^ numInfected # probability
  if (X > prob) # if the random is above the probability (from 0-1), then noninfected becomes infected
    return (TRUE)
  else return (FALSE) # if less (the actual probability), then noninfected stays the same
}