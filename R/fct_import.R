#' import
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
import_bound <- function() {
  bound = read.csv("http://hbrsensor.sr.unh.edu/data/snownet/bound.csv", head = T, sep = ",")
}
