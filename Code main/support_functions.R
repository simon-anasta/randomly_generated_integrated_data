# Support functions

## Assert ---------------------------------------------------------------------
#' Throws an error if condition is not TRUE
#'
assert <- function(condition, msg) {
  # condition must be logical
  if (!is.logical(condition)) {
    this = class(condition)
    msg <- glue::glue("condition must be logical, received {this[1]} instead")
    stop(msg)
  }
  # check condition and throw error
  if (condition == FALSE) {
    stop(msg)
  }
}

## Inform user with time stamped measure --------------------------------------
#' Prints to console time of function call followed by msg.
#'
run_time_inform_user <- function(msg) {
  assert(is.character(msg), "msf must be of type character")
  
  # time
  now <- as.character(Sys.time())
  # display
  cat(now, "|", msg, "\n")
}

## Not In ---------------------------------------------------------------------
#' Negative of %in%
#'
"%not_in%" <- function(x, y) {
  !("%in%"(x, y))
}
## Weighted option ------------------------------------------------------------
#' Given a data frame with columns value and weight, makes a vector containing
#' each value the weighted number of times. This vector can then be sampled
#' with replacement to produce the described description.
#' 
make_weighted_options_array <- function(df){
  assert(is.data.frame(df), "df must be a data frame")
  assert("value" %in% colnames(df), "df must have column named 'value'")
  assert("weight" %in% colnames(df), "df must have column named 'weight'")
  
  values = df$value
  weights = as.numeric(df$weight)
  
  reps = rep(values, weights)
  return(reps)
}

## Random between -------------------------------------------------------------
#' Generate random integers between a lower and upper bound.
#' 
randbetween <- function(lower, upper, reps = 1){
  assert(is.numeric(lower), "lower must be numeric")
  assert(is.numeric(upper), "upper must be numeric")
  assert(is.numeric(reps), "reps must be numeric")
  
  lower = ceiling(lower)
  upper = floor(upper)
  reps = floor(reps)
  
  range = upper - lower + 1
  rands = runif(reps)
  
  results = floor(lower + rands * range)
  return(results)
}



