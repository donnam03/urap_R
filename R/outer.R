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
