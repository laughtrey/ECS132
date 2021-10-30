# N is the number of turns to win
# d is the winning threshhold
# s is the number of sides to the die
# k is the predicted number of turns to win

# pd,k = P(N = k)
# pd,k = (1/s) Sr=1,...,s pd-r,k-1
pnk<-function(d,s,k) {
	sumProb <- 0	
	
	for (r in 1:s) {
		if (k == 0) {
			if (d <= k)	return (1)
			else if (d > k) return (0)
		}
		else {
			sumProb <- sumProb + (1/s * pnk(d-r, s, k-1))	
		}
	}
	return (sumProb)	
}

roll <- function(Y) return(sample(1:Y,1))
