# ECS 132
# Rohan Skariah, Nathan Krieger, Geoffrey Alexander, Raymond Laughrey
# HW 2 Problem 3

doNotPassGo<-function(k) {
  # The question asks to print doNotPassGo(0), which is setup as a seperate function, please uncomment the following code to call that function.
  #printDoNotPassGoZero()
  if (k == 3) return (NULL)
  return (calculateDoNotPassGo(k))
}

calculateDoNotPassGo<-function(k) {
  summation <- 1
  if (k == 7) return (1)
  if (k > 7) return (0)

  for(i in (k+1):(k+6)) {
    if (i == 3) { 
      summation <- summation + 1/12 * calculateDoNotPassGo(i)
    }
    else summation <- summation + 1/6 * calculateDoNotPassGo(i)
  }
  return (summation)
}

printDoNotPassGoZero<-function() {
  print(calculateDoNotPassGo(0))
}