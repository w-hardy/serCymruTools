#' @title Forecasts for each regional unit
#'
#' @description Various fable and prophet forecasts for monthly quantity for each regional_unit.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`.
#' @param h The number of months that should forecast.
#' @param keep_fits Logical. Should the model objects be kept. Keeping them will increase memory requirements, use with caution.
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
regionalJointFcsts <- function(data, h ="5 months", keep_fits = FALSE){
  if(!is.logical(keep_fits)){stop("`keep_fits` must be of type logical")}
  regions <- unique(data$regional_unit)

  plan(multisession, gc = TRUE)

  fable_fit <-
    future_map_dfr(.x = regions,
                   .f = ~fableModels(filter(data, regional_unit ==.x)),
                   .options = furrr_options(seed = TRUE))
  message("fable fits complete, step 1/4")

  fable_fcst <-
    future_map_dfr(.x = regions,
                   .f = ~as_tibble(forecast(filter(fable_fit,
                                                   regional_unit ==.x),
                                            h = h)),
                   .options = furrr_options(seed = TRUE))
  message("fable forecasts complete, step 2/4")

  prophet_fit <-
    future_map_dfr(.x = regions,
                   .f = ~prophetModels(filter(data, regional_unit ==.x)),
                   .options = furrr_options(seed = TRUE))
  message("prophet fits complete, step 3/4")

  prophet_fcst <-
    future_map_dfr(.x = regions,
                   .f = ~as_tibble(forecast(filter(prophet_fit,
                                                   regional_unit ==.x),
                                            h = h)),
                   .options = furrr_options(seed = TRUE))
  # Some prophet forecasts are giving daily forecasts. We want monthly forecasts.
  # This may also be influencing accuracy

  message("prophet forecasts complete, step 4/4")
  plan(sequential)
  # prophetModels now being run in parallel. Needed to reinstall prophet from
  # source

  joint_fcst <-
    dplyr::bind_rows(fable_fcst, prophet_fcst) %>%
    dplyr::mutate(lb_95 = quantile(n, .025),
                  lb_80 = quantile(n, .1),
                  med = median(n),
                  ub_80 = quantile(n, .9),
                  ub_95 = quantile(n, .975),
                  sd = distributional::variance(n)^2,
                  dist = n,
                  n = NULL)

  if (keep_fits == TRUE) {
    joint_fit <-
      dplyr::left_join(fable_fit, prophet_fit, by = "regional_unit")

    return(list(fits = joint_fit, forecasts = joint_fcst))
  } else {
    return(list(forecasts = joint_fcst))
  }

}
