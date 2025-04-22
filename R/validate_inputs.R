validate_inputs <- function(choice, amt1, amt2, prob_delay1, prob_delay2,
                            modeltype, r_itc) {
  # Check vectors
  if (! is.vector(choice) && is.vector(amt1) && is.vector(amt2) &&
      is.vector(prob_delay1) && is.vector(prob_delay2))
    stop("Inputs should be a vector")
  if (!(length(choice) == length(amt1) && length(amt1) == length(amt2) &&
        length(amt2) == length(prob_delay1) && length(prob_delay1) == length(prob_delay2))) {
    stop("All vectors must have the same length")
  }

  # Check model type
  if (!is.character(modeltype)) {
    stop("modeltype must be a string")
  }

  # Check choice
  if (!is.numeric(choice)) {
    stop("choice must be numeric")
  }
  if (!all(choice %in% c(0, 1))) {
    stop("choice must only consist of 0 and 1 where 1 is choosing option 1 and
         0 is choosing option 2")
  }

  # Check amounts
  if (!is.numeric(amt1) || !is.numeric(amt2)) {
    stop("amounts must be numeric")
  }
  if (any(amt1 <= 0) || any(amt2 <= 0)) {
    stop("amounts must be positive")
  }
  if (length(amt1) < 3) {
    stop("must have at least 3 observations")
  }

  #JUST RC
  if (r_itc == "R") {
    if (!(toupper(modeltype) %in% c("E", "R", "W", "H"))) {
      stop("modeltype must be E for expected utility theory, R for risk-return,
         W for weber, or H for hyperbolic")
    }

    # Check probabilities
    if (!is.numeric(prob_delay1) || !is.numeric(prob_delay2)) {
      stop("probabilities must be numeric")
    }
    if (any(prob_delay1 <= 0 | prob_delay1 > 1) || any(prob_delay2 <= 0 | prob_delay2 > 1)) {
      stop("probabilities must be between 0 (exclusive) and 1 (inclusive)")
    }

  } else {
    if (!(toupper(modeltype) %in% c("E", "H", "GH", "Q"))) {
      stop("modeltype must be 'E' for exponential, 'H' for hyperbolic,
         'GH' for generalized hyperbolic, or 'Q' for quasi-hyperbolic")
    }

    #Check delay
    if (!is.numeric(prob_delay1) || !is.numeric(prob_delay2)) {
      stop("delays must be numeric")
    }
    if (any(prob_delay1 < 0 ) || any(prob_delay2 < 0)) {
      stop("delays must be greater than or equal to 0")
    }
  }

  # Warning if choice data is one-sided
  if (all(as.logical(choice)) || !any(as.logical(choice))) {
    warning("All input data is one-sided")
    return(1)
  } else {
    return(0)
  }
}
