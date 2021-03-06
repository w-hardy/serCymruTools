#' @title Forecasts for each regional unit
#'
#' @description Various fable forecasts for monthly quantity for each regional_unit. Can make use of plan() futures.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`.
#' @param h The number of months that should forecast.
#'
#' @importFrom distributional variance
#' @import dplyr
#' @import fabletools
#' @import furrr
#' @import future
#' @importFrom stats median quantile
#' @import tsibble
#'
#' @return Model forecasts
#' @export
#'
#' @examples
#' \dontrun{regionalJointFcsts(data)}
#'
regionalJointFcsts <- function(data, h ="8 months"){
  regions <- unique(data$regional_unit)

  fable_fcst <-
    future_map_dfr(.x = regions,
                   .f = ~fableModels(filter(data, regional_unit ==.x)) %>%
                     forecast(h = h) %>% as_tibble(),
                   .options = furrr_options(seed = TRUE))
  message("fable forecasts complete, step 1/2")


  joint_fcst <-
    fable_fcst %>%
    dplyr::mutate(lb_95 = quantile(n, .025),
                  lb_80 = quantile(n, .1),
                  med = median(n),
                  ub_80 = quantile(n, .9),
                  ub_95 = quantile(n, .975),
                  sd = distributional::variance(n)^2,
                  n = NULL # Remove distribution from returned object to reduce size
                  )

  return(joint_fcst)
}
