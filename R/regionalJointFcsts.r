#' @title Forecasts for each regional unit
#'
#' @description Various fable and prophet forecasts for monthly quantity for each regional_unit.
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
#' @return List of model fits and forecasts
#' @export
#'
#' @examples
#' \dontrun{regionalJointFcsts(data)}
#'
regionalJointFcsts <- function(data, h ="5 months"){
  regions <- unique(data$regional_unit)
  plan(multisession)
  fable_fit <-
    future_map_dfr(.x = regions,
                   .f = ~fableModels(filter(data, regional_unit ==.x)),
                   .options = furrr_options(seed = TRUE))
  message("fable fits complete")

  fable_fcst <- fable_fit %>%
    forecast(h = h) %>%
    as_tibble()
  message("fable forecasts complete")


  prophet_fit <-
    future_map_dfr(.x = regions,
                   .f = ~prophetModels(filter(data, regional_unit ==.x)),
                   .options = furrr_options(seed = TRUE))
  message("prophet fits complete")

  prophet_fcst <-
    forecast(prophet_fit, h = h) %>%
    as_tibble()
  # Some prophet forecasts are giving daily forecasts. We want monthly forecasts.
  # This may also be influencing accuracy

  message("prophet forecasts complete")
  plan(sequential)
  # prophetModels now being run in parallel. Needed to reinstall prophet from
  # source

  joint_fit <-
    left_join(fable_fit, prophet_fit, by = "regional_unit")
  joint_fcst <-
    bind_rows(fable_fcst, prophet_fcst) %>%
    mutate(lb_95 = quantile(n, .025),
           lb_80 = quantile(n, .1),
           med = median(n),
           ub_80 = quantile(n, .9),
           ub_95 = quantile(n, .975),
           sd = distributional::variance(n)^2,
           dist = n,
           n = NULL)

  return(list(fits = joint_fit, forecasts = joint_fcst))
}
