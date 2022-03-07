#' compute_NPV
#'
#' compute net present value
#' @param value/cost ($)
#' @param time in the future that cost/value occurs (years)
#' @param discount rate
#' @return value in $
#' @author Naomi Tague


compute_NPV = function(value, time, discount) {
  # use value to compute NPV
  result = value / (1 + discount)**time
  return(result)
}
