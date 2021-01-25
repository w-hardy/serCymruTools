#' @title Helper functions
#'
#' @description Helper functions for serCymruTools
#'
#' @param x Double to transform
#' @param lower Lower limit of admissible values
#' @param upper Upper limit of admissible values
#'
#' @export
#'
#'Transforming data to be bound between a lower and upper limit http://www.maths.bristol.ac.uk/R/web/packages/fable/vignettes/transformations.html

my_scaled_logit <-
  fabletools::new_transformation(
    function(x, lower=0, upper=100)
    {
      log((x-lower)/(upper-x))
    },
    function(x, lower=0, upper=100)
    {
      (upper-lower)*exp(x)/(1+exp(x)) + lower
    })
