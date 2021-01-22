#' @title Helper functions
#'
#' @description Helper functions for serCymruTools
#'
#' @param x Double to transform
#' @param lower Lower limit of admissible values
#' @param upper Upper limit of admissible values
#'
#'Transforming data to be bound between a lower and upper limit http://www.maths.bristol.ac.uk/R/web/packages/fable/vignettes/transformations.html
scaled_logit <- function(x, lower=0, upper=1){
  log((x-lower)/(upper-x))}

inv_scaled_logit <- function(x, lower=0, upper=1){
  (upper-lower)*exp(x)/(1+exp(x)) + lower}

my_scaled_logit <-
  fabletools::new_transformation(scaled_logit, inv_scaled_logit)
