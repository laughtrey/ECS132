simjj<-function(d, nreps) {
	# Create the list of vectors
	# winner, prize and loserDots
	winner <- c()
	prize <- c()
	loserDots <- c()
	# n <- c()  <-- used to check 1b

	# Repeat nrep times
	for (i in 1:nreps) {
		#Simulation happens here
		
		jillDots <- 0
		jackDots <- 0
		numTurns <- 0 # <-- used to check 1b

		# random dice generator (whole #1-6)
		rand <- sample(1:6, 1)
      
		while(jillDots < d && jackDots < d) {
			jillDots <- jillDots + roll()
			if (jillDots < d) {
				jackDots <- jackDots + roll()
			}
			numTurns <- numTurns + 1
		}
		
		# Assign values here and add to vector
		if (jillDots > jackDots)
			winner <- append(winner, TRUE)
		else
			winner <- append(winner, FALSE)

		prize <- append(prize, max(jillDots, jackDots))
		loserDots <- append(loserDots, min (jillDots, jackDots))
		#n <- append(n, numTurns)  <-- used to check 1b
	}

	output = list (winner, prize, loserDots)
	#output = list (winner, prize, loserDots, n)  <-- used to check 1b
	return (output)
}

roll <- function() return(sample(1:6,1))

checkVal <- function(d, nreps) {
  simulationResults<-simjj(d, nreps)
  # 1a
  cat("1a: ", mean(simulationResults[[1]]), "\n")

  # We are unable to check answer for 1b since that would require the function returning the number of turns it takes for the winner to win. While we did check it on our own, the submission will follow the instructions and not return the number of turns it takes to win.
  # 1b
  #cat("1b: ", mean((simulationResults[[4]] == 2) & simulationResults[[1]]), "\n")
  # Jill winning in 1, 2, 3, 4
  #cat("win in 1 roll: ", mean((simulationResults[[4]] == 1) & simulationResults[[1]]), "\n")
  #cat("win in 2 roll: ", mean((simulationResults[[4]] == 2) & simulationResults[[1]]), "\n")
  #cat("win in 3 roll: ", mean((simulationResults[[4]] == 3) & simulationResults[[1]]), "\n")
  #cat("win in 4 roll: ", mean((simulationResults[[4]] == 4) & simulationResults[[1]]), "\n")

  # 1c
  cat("1c: ", mean((simulationResults[[2]] - simulationResults[[3]]) == 1), "\n")

  # 1d
  cat("1d: ", mean(simulationResults[[2]] == 6 & simulationResults[[1]]), "\n")
}