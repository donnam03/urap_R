flagged <- function(choice, amt1, amt2, prob1, prob2, modeltype, risk_itc) {
  # Function is called when all input data is one-sided to calculate the minimum
  # and maximum parameter and their likelihood.

  num_obs <- length(amt1)
  fit_metrics <- "n/a"
  if (risk_itc == "R"){
    #RISKY CHOICE
    if (modeltype == "E") {
    bounds <- exp(e_bounds(prob1, prob2, amt1, amt2))
    model <- "Expected Utility Theory"
    } else if (modeltype == "R") {
      bounds <- r_bounds(prob1, prob2, amt1, amt2)
      model <- "Risk Return"
    } else if (modeltype == "W") {
      bounds <- w_bounds(prob1, prob2, amt1, amt2)
      model <- "Weber"
    } else {
      bounds <- exp(h_bounds(prob1, prob2, amt1, amt2))
      model <- "Hyperbolic"
    }
    util_diff_min <- util_diff_rc(min(bounds), choice, amt1, amt2, prob1, prob2, modeltype)
    util_diff_max <- util_diff_rc(max(bounds), choice, amt1, amt2, prob1, prob2, modeltype)
    } else {
      #ITC, ie. let prob be delay
      s_min <- 1
      s_max <- 1
      if (modeltype == "E"|| modeltype == "Q"){
      bounds <- e_q_bounds(amt1, prob1, amt2, prob2)
      if (modeltype == "E") {
        model <- "Exponential"
      } else {
        model <- "Quasi Hyperbolic"
        s_min <- 1
        s_max <- 1
      }
    } else {
      bounds <-h_gh_bounds(amt1,prob1,amt2,prob2)
      if (modeltype == "H") {
        model <- "Hyperbolic"
      } else {
        model <- "Generalized Hyperbolic"
        #s_min<- log((log(amt1)-log(amt2))/(log(1+min(bounds)*delay1)-log(1+min(bounds)*delay2)))
        #s_max<- log((log(amt1)-log(amt2))/(log(1+max(bounds)*delay1)-log(1+max(bounds)*delay2)))
      }
    }
      lower_k_log <- ifelse(min(bounds) > 0,log(min(bounds)),log(0.00001))
      upper_k_log <- ifelse(max(bounds) > 0,log(max(bounds)),log(0.00001))
      util_diff_min <- util_diff_itc(c(lower_k_log,0,s_min), choice, amt1, amt2, delay1, delay2, modeltype)
      util_diff_max <- util_diff_itc(c(upper_k_log,0,s_max), choice, amt1, amt2, delay1, delay2, modeltype)
      }
  like1 <- avg_like(choice, util_diff_min)
  like2 <- avg_like(choice,util_diff_max)
  print("flagged")
  if (like1 > like2) {
    return(list(min(bounds), like1, fit_metrics, model, num_obs))
  } else{
    return(list(max(bounds), like2, fit_metrics, model, num_obs))
  }
}
