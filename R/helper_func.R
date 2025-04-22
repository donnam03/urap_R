#Average Likelihood
avg_like <- function (choice, util_diff) {
  logit <- 1/(1+ exp(-util_diff))
  logit <- pmin(pmax(logit, 1e-6), 1 - 1e-6)
  likelihood <- (1- choice) * logit + (1-logit) *(choice)
  likelihood2 <- ifelse(likelihood != 0, likelihood, exp(-307))
  return (mean(likelihood2))
}

#Multiple Starting Points
startingpoints <- function(indiff_vals){
  ten <- quantile(indiff_vals, 0.1)
  twentyfive <- quantile(indiff_vals, 0.25)
  fifty <- quantile(indiff_vals, 0.5)
  seventyfive <- quantile(indiff_vals, 0.75)
  ninty <- quantile(indiff_vals, 0.9)
  return(c(ten, twentyfive, fifty, seventyfive, ninty))
}

#Utility Models

#Expected Utility Theory
eut <- function(prob, amt, a) {
  return (amt^a * prob)
}

#Risk Return
risk_return <- function (prob, amt, b) {
  ev <- amt*prob
  var <- prob * (amt - ev)^ 2 + (1 - prob) * (-ev) ^2
  return (ev - b*var)
}

#Weber
co_of_var <- function (prob, amt, b) {
  ev <- amt*prob
  var <- prob * (amt - ev)^ 2 + (1 - prob) * (-ev) ^2
  return (ev - b*sqrt(var)/ev)
}

#Hyperbolic
prob_discount <- function (prob, amt, h){
  theta <- (1-prob)/prob
  return (amt/(1+h*theta))
}

#Exponential
exponential <- function(amt, k, delay) {
  return (amt*exp(-k*delay))
}

#Delay Hyperbolic
hyperbolic <- function(amt, k, delay) {
  return (amt/(1 + k*delay))
}

#Quasi Hypberbolic
quasi_h <-function(amt, k, delay, b) {
  return (amt*b*exp(-k*delay))
}

#Generalized Hyperbolic
gen_h <- function(amt, k, delay, s) {
  return(amt/(1+k*delay)^s)
}

