#' Fit a Utility-Based Risky Choice Model
#'
#' Fits one of several utility-based decision models to binary choice data.
#'
#' @param modeltype Character. One of "E", "R", "W", or "H".
#' \itemize{
#'   \item{"E"}: Expected utility, where U = p * A^alpha
#'   \item{"R"}: Risk-return, where U = EV - b * Var
#'   \item{"W"}: Weber, where U = EV - b * CV, with CV = sqrt(Var)/EV
#'   \item{"H"}: Hyperbolic, where U = A / (1 + h * theta), with theta = (1 - p) / p
#' }

#' @param choice Numeric vector of 0s and 1s, where 1 = option 1 chosen.
#' @param amt1, prob1 Numeric vectors of outcomes and probabilities for option 1.
#' @param amt2, prob2 Numeric vectors of outcomes and probabilities for option 2.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{output}{A list with:
#'     \describe{
#'       \item{fitted_param}{Estimated model parameter (alpha, b, or h)}
#'       \item{inv_temp}{Inverse temperature}
#'     }
#'   }
#'   \item{fit_metrics}{Optimization result object}
#'   \item{modeltype}{The model type used}
#'   \item{num_observations}{Number of observations}
#' }
#'
#' @examples
#' util_rc("E", c(0,0,1), c(10,10,10), c(1,1,1), c(20,30,40), c(0.6,0.5,0.4))
#'
#' @export
util_rc<- function(modeltype, choice, amt1, prob1, amt2, prob2){
  flag <- validate_inputs(choice, amt1, amt2, prob1, prob2, modeltype, "R")
  if (flag == 1) {
    output <- flagged(choice, amt1, amt2, prob1, prob2, modeltype, "R")
  } else{
    output <- min_func(choice, amt1, amt2, prob1, prob2, modeltype)
  }
  #predict()
  return(output)
}
util_itc <- function(modeltype, choice, amt1, delay1, amt2, delay2){
  flag <- validate_inputs(choice, amt1, amt2, delay1, delay2, modeltype, "I")
  if (flag == 1) {
    output <- flagged(choice, amt1, amt2, delay1, delay2, modeltype, "I")
  } else{
    output <- itc_min(choice, amt1, amt2, delay1, delay2, modeltype)
    #predict()
    return(output)
  }
}
