#RC BOUNDS
e_bounds<- function(prob1, prob2, amt1, amt2) {
  a <- abs(log(prob2/prob1)/(log(amt1/amt2)))
  return(log(a))
}

r_bounds<- function(prob1, prob2, amt1, amt2){
  ev2 <- amt2*prob2
  var2 <- prob2 * (amt2 - ev2)^ 2 + (1 - prob2) * (-ev2) ^2
  ev1 <- amt1 *prob1
  var1 <- prob1 * (amt1 - ev1)^ 2 + (1 - prob1) * (-ev1) ^2
  b <- (ev1 - ev2)/ (var1 - var2)
  return (b)
}


w_bounds<- function(prob1, prob2, amt1, amt2){
  ev2 <- amount2*prob2
  var2 <- prob2 * (amount2 - ev2)^ 2 + (1 - prob2) * (-ev2) ^2
  ev1 <- amount1 *prob1
  var1 <- prob1 * (amount1 - ev1)^ 2 + (1 - prob1) * (-ev1) ^2
  b <- (ev1 - ev2)/(sqrt(var1)/ev1 - (sqrt(var2)/ev2))
  return (b)
}

h_bounds<- function(prob1, prob2, amt1, amt2){
  theta1 <- (1- prob1)/prob1
  theta2 <- (1-prob2)/prob2
  h <- abs((amt2 - amt1)/(amt1*theta2 - amt2*theta1))
  return (log(h))
}

#ITC BOUNDS
h_gh_bounds <-function(amt1,delay1,amt2,delay2){
  k <- (amt2 - amt1)/((amt1*delay2)-(amt2*delay1))
  return(k)
}

e_q_bounds <- function(amt1, delay1, amt2, delay2) {
  k <- (log(amt2)-log(amt1))/(delay2 - delay1)
  return(k)
}

