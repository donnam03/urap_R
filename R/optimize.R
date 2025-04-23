#rc
util_diff_rc <-function(target, choice, amt1, amt2, prob1, prob2, modeltype){
  if (modeltype == "E"){
    util1 <- eut(prob1, amt1, exp(target[1]))
    util2 <- eut(prob2, amt2, exp(target[1]))
  } else if (modeltype == "R") {
    util1 <- risk_return(prob1, amt1, target[1])
    util2 <- risk_return(prob2, amt2, target[1])
  } else if (modeltype == "W") {
    s <- exp(target[3])
    util1 <- co_of_var(prob1, amt1, target[1])
    util2 <- co_of_var(prob2, amt2, target[1])
  } else {
    util1 <- prob_discount(prob1, amt1, exp(target[1]))
    util2 <- prob_discount(prob2, amt2, exp(target[1]))
  }
  return (util2 - util1)
}

parent_func <- function(target, choice, amt1, amt2, prob1, prob2, modeltype){
  DV1 <- util_diff_rc(target[1], choice, amt1, amt2, prob1, prob2, modeltype)
  DV2 <- -ifelse(choice == 0, DV1, -DV1)
  DV3 <- DV2/exp(target[2])
  log_p <- ifelse(DV3 < 709, -log(1+exp(DV3)), -DV3)
  return(-sum(log_p))
}

min_func <- function(choice, amt1, amt2, prob1, prob2,modeltype){
  likelihoods <- c()
  params <- c()
  invtemps <- c()
  if (modeltype == "E") {
    model <- "Expected Utility Theory"
    bound <- e_bounds(prob1, prob2, amt1, amt2)
    start <- startingpoints(bound)
    lower_bounds <- c(min(bound), 0)
    upper_bounds <- c(max(bound), 37)
    for (i in 1:5) {
      result <- optim(par = c(start[i],0), fn = parent_func,choice =choice,
                      amt1 = amt1, amt2 = amt2, prob1 = prob1, prob2 = prob2,
                      modeltype = modeltype,method = "L-BFGS-B",
                      lower = lower_bounds, upper = upper_bounds)
      a <- result$par[1]
      util_diff <- util_diff_rc(a, choice, amt1, amt2, prob1, prob2, modeltype)
      avglikelihood <- avg_like(choice, util_diff)
      likelihoods <- c(likelihoods,avglikelihood)
      params <- c(params, exp(a))
      invtemps<- c(invtemps, result$par[2])
    }
  } else if (modeltype == "R"){
    model = "Risk Return"
    bound = r_bounds(prob1, prob2, amt1, amt2)
    start <- startingpoints(bound)
    lower_bounds <- c(min(bound), 0)
    upper_bounds <- c(max(bound), 37)
    for (i in 1:5) {
      result <- optim(par = c(start[i],0), fn = parent_func,choice =choice,
                      amt1 = amt1, amt2 = amt2, prob1 = prob1, prob2 = prob2,
                      modeltype = modeltype,method = "L-BFGS-B",
                      lower = lower_bounds, upper = upper_bounds)
      b <- result$par[1]
      util_diff <- util_diff_rc(b, choice, amt1, amt2, prob1, prob2, modeltype)
      avglikelihood <- avg_like(choice, util_diff)
      likelihoods <- c(likelihoods,avglikelihood)
      params <- c(params, b)
      invtemps<- c(invtemps, result$par[2])
    }
  } else if (modeltype == "W"){
    model <- "Weber"
    bound <- w_bounds(prob1, prob2, amt1, amt2)
    start <- startingpoints(bound)
    lower_bounds <- c(min(bound), 0)
    upper_bounds <- c(max(bound), 37)
    for (i in 1:5) {
      result <- optim(par = c(start[i],0), fn = parent_func,choice =choice,
                      amt1 = amt1, amt2 = amt2, prob1 = prob1, prob2 = prob2,
                      modeltype = modeltype, method = "L-BFGS-B",
                      lower = lower_bounds, upper = upper_bounds)
      b <- result$par[1]
      util_diff <- util_diff_rc(b, choice, amt1, amt2, prob1, prob2, modeltype)
      avglikelihood <- avg_like(choice, util_diff)
      likelihoods <- c(likelihoods, avglikelihood)
      params <- c(params, b)
      invtemps<- c(invtemps, result$par[2])
    }
  } else {
    model <- "Hyperbolic"
    bound <- h_bounds(prob1, prob2, amt1, amt2)
    start <- startingpoints(bound)
    lower_bounds <- c(min(bound), 0)
    upper_bounds <- c(max(bound), 37)
    for (i in 1:5) {
      result <- optim(par = c(start[i],0), fn = parent_func,choice =choice,
                      amt1 = amt1, amt2 = amt2, prob1 = prob1, prob2 = prob2,
                      modeltype = modeltype, method = "L-BFGS-B",
                      lower = lower_bounds, upper = upper_bounds)
      h <- result$par[1]
      util_diff <- util_diff_rc(h, choice, amt1, amt2, prob1, prob2, modeltype)
      avglikelihood <- avg_like(choice, util_diff)
      likelihoods <- c(likelihoods,avglikelihood)
      params <- c(params, exp(h))
      invtemps<- c(invtemps, result$par[2])
    }
  }
  fitted_param <- params[which.max(likelihoods)]
  inv_temp <- invtemps[which.max(likelihoods)]
  fit_metrics = "L-BFGS-B"
  num_obs = length(amt1)
  return(list(fitted_param,inv_temp, fit_metrics, model, num_obs))
}

#itc
util_diff_itc <-function(target, choice, amt1, amt2, delay1, delay2, modeltype){
  # target = (log(k),inv_temp, s)
  k = exp(target[1])
  if (modeltype == "E"){
    util1 <- exponential(amt1, k, delay1)
    util2 <- exponential(amt2, k, delay2)
  } else if (modeltype == "H") {
    util1 <- hyperbolic(amt1, k, delay1)
    util2 <- hyperbolic(amt2, k, delay2)
  } else if (modeltype == "GH") {
    s <- exp(target[3])
    util1 <- gen_h(amt1,k,delay1,s)
    util2 <- gen_h(amt2, k, delay2, s)
  } else {
    b <- target[3]
    util1 <- quasi_h(amt1, k, delay1, b)
    util2 <- quasi_h(amt2, k, delay2, b)
  }
  return (util2 - util1)
}

fun <- function(target, choice, amt1, amt2, delay1, delay2, modeltype){
  DV1 <- util_diff_itc(target, choice, amt1, amt2, delay1, delay2, modeltype)
  DV2 <- -ifelse(choice == 0, DV1, -DV1)
  DV3 <- DV2/exp(target[2])
  log_p <- ifelse(DV3 < 709, -log(1+exp(DV3)), -DV3)
  return (-mean(log_p))
}

itc_min <- function(choice, amt1, amt2, delay1, delay2,modeltype){
  likelihoods <- c()
  k_params <- c()
  b_params <- c()
  invtemps <- c()
  if (modeltype == "Q"){
    model <- "Quasi Hyperbolic"
    bounds <-e_q_bounds(amt1, delay1, amt2, delay2)
    start <- startingpoints(bounds)
    lower_k_log <- ifelse(min(bounds) > 0,log(min(bounds)),log(0.00001))
    upper_k_log <- ifelse(max(bounds) > 0,log(max(bounds)),log(0.00001))
    lower_bounds <- c(lower_k_log, -1, 0)
    upper_bounds <- c(upper_k_log, 2, 1)
    for (i in 1:5){
      for (j in c(0.25,0.5,0.75)){
        # target = (log(k),inv_temp, s)
        init <- c(start[i],1,j)
        result <- optim(par = init, fn = fun,choice = choice,amt1 = amt1,
                  amt2 = amt2, delay1 = delay1, delay2 = delay2, modeltype = modeltype,
                  method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)
        estimated_k <- exp(result$par[1])
        estimated_it <- exp(result$par[2])
        estimated_b <- result$par[3]
        params <- c(log(estimated_k), log(estimated_it), estimated_b)
        util_diff <- util_diff_itc(params,choice, amt1, amt2, delay1, delay2, modeltype)
        avglikelihood <- avg_like(choice, util_diff)
        likelihoods <- c(likelihoods,avglikelihood)
        k_params <- c(k_params, estimated_k)
        invtemps <- c(invtemps, estimated_it)
        b_params <- c(b_params, estimated_b)
      }
    }

  } else if (modeltype == "GH") {
    model <- "Generalized Hyperbolic"
    bounds <-h_gh_bounds(amt1, delay1, amt2, delay2)
    lower_k_log <- ifelse(min(bounds) > 0,log(min(bounds)),log(0.00001))
    upper_k_log <- ifelse(max(bounds) > 0,log(max(bounds)),log(0.00001))
    lower_bounds <- c(lower_k_log, 0,log(0.001))
    upper_bounds <- c(upper_k_log,2,log(2))
    start <- startingpoints(bounds)
    for (i in 1:5){
      for (j in seq(0, 2, length.out = 5)) {
      init <- c(start[i],1,j)
      result <- optim(par = init, fn = fun,choice = choice,amt1 = amt1, amt2 = amt2,
                  delay1 = delay1, delay2 = delay2, modeltype = modeltype, method
                  = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)
      util_diff <- util_diff_itc(c(result$par[1],result$par[2],result$par[3]),choice, amt1, amt2, delay1, delay2, modeltype)
      avglikelihood <- avg_like(choice, util_diff)
      likelihoods <- c(likelihoods,avglikelihood)

      k_params <- c(k_params, exp(result$par[1]))
      invtemps <- c(invtemps, exp(result$par[2]))
      b_params <- c(b_params, exp(result$par[3]))
      }
    }

  } else if (modeltype == "E") {
    model <- "Exponential"
    bounds <-e_q_bounds(amt1, delay1, amt2, delay2)
    start <- startingpoints(bounds)
    lower_k_log <- ifelse(min(bounds) > 0,log(min(bounds)),log(0.00001))
    upper_k_log <- ifelse(max(bounds) > 0,log(max(bounds)),log(0.00001))
    lower_bounds <- c(lower_k_log, -1)
    upper_bounds <- c(upper_k_log, 2)
    for (i in 1:5){
      for (j in c(0.75, 1.5, 2.25)){
        # target = (log(k),inv_temp, s)
        init <- c(start[i],j)
        result <- optim(par = init, fn = fun,choice = choice,amt1 = amt1,
                        amt2 = amt2, delay1 = delay1, delay2 = delay2, modeltype = modeltype,
                        method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)
        estimated_k <- exp(result$par[1])
        estimated_it <- exp(result$par[2])
        params <- log(c(estimated_k,estimated_it))
        util_diff <- util_diff_itc(params,choice, amt1, amt2, delay1, delay2, modeltype)
        avglikelihood <- avg_like(choice, util_diff)
        likelihoods <- c(likelihoods,avglikelihood)
        k_params <- c(k_params, estimated_k)
        invtemps <- c(invtemps, estimated_it)
      }
    }
  } else {
    # modeltype = H
    model <- "Hyperbolic"
    bounds <-h_gh_bounds(amt1, delay1, amt2, delay2)
    start <- startingpoints(bounds)
    lower_k_log <- ifelse(min(bounds) > 0,log(min(bounds)),log(0.00001))
    upper_k_log <- ifelse(max(bounds) > 0,log(max(bounds)),log(0.00001))
    lower_bounds <- c(lower_k_log, -1)
    upper_bounds <- c(upper_k_log, 2)
    for (i in 1:5){
      for (j in c(0.75, 1.5, 2.25)){
        # target = (log(k),inv_temp, s)
        init <- c(start[i],j)
        result <- optim(par = init, fn = fun,choice = choice,amt1 = amt1,
                        amt2 = amt2, delay1 = delay1, delay2 = delay2, modeltype = modeltype,
                        method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)
        estimated_k <- exp(result$par[1])
        estimated_it <- exp(result$par[2])
        params <- log(c(estimated_k,estimated_it))
        util_diff <- util_diff_itc(params,choice, amt1, amt2, delay1, delay2, modeltype)
        avglikelihood <- avg_like(choice, util_diff)
        likelihoods <- c(likelihoods,avglikelihood)
        k_params <- c(k_params, estimated_k)
        invtemps <- c(invtemps, estimated_it)
      }
    }
  }
  best <- which.max(likelihoods)
  fitted_k <- k_params[best]
  fitted_b <- b_params[best]
  inv_temp <- invtemps[best]
  fit_metrics = "L-BFGS-B"
  num_obs = length(amt1)
  return (list(fitted_k,inv_temp, fitted_b, fit_metrics, model, num_obs))
}
