#' @title Cross-validated model fits for bound data
#'
#' @description Cross-validated fits of various fable and prophet models. Will take advantage of plan() if called outside of function.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`.
#' @param cv_dist The number of months that should be cross-validated.
#' @param init The number of months that cross-validations should start from.
#' @param step The number of months between each cross-validation step.
#'
#' @import dplyr
#' @import rstan
#' @import fable.prophet
#' @import fabletools
#' @import furrr
#' @import future
#' @import tsibble
#' @import progressr
#'
#' @return Dataframe of prophet model fits
#' @export
#'
#' @examples
#' \dontrun{regionalCV(data)}
#'

regionalCVBound <- function(data, cv_dist = 6, init = 48, step = 3){

  # CV accuracy for fableModelsBound() and prophetModelsBound()

  regions <- unique(data$regional_unit) # Get unique regional_units for map()

  fableCV <- function(data){ # Return CV model accuracy for fableModelsBound()
    x <- data$regional_unit

    data_test <-
      data %>%
      mutate(datename = yearmonth(datename))

    data %>%
      select(regional_unit, datename, n) %>%
      mutate(datename = yearmonth(datename)) %>%
      fill_gaps() %>%
      slice(1:(n()-cv_dist), .preserve = TRUE) %>%
      stretch_tsibble(.init = init, .step = step) %>%
      fableModelsBound(lower = 0, upper = 100) %>%
      forecast(h = cv_dist) %>%
      accuracy(data_test, list(RMSE = RMSE, MAE = MAE, MAPE = MAPE,
                               rmse_skill = skill_score(RMSE),
                               crps_skill = skill_score(CRPS), ACF1 =ACF1,
                               winkler = winkler_score))

  }

  prophetCV <- function(data){# Return CV model accuracy for prophetModelsBound()
    x <- data$regional_unit

    data_test <-
      data %>%
      mutate(datename = yearmonth(datename))

    data_trn <-
      data %>%
      select(regional_unit, datename, n) %>%
      mutate(datename = yearmonth(datename)) %>%
      fill_gaps() %>%
      slice(1:(n()-cv_dist), .preserve = TRUE) %>%
      stretch_tsibble(.init = init, .step = step)

    data_trn %>%
      prophetModelsBound(lower = 0, upper = 100) %>%
      forecast(h = cv_dist) %>%
      accuracy(data_test, list(RMSE = RMSE, MAE = MAE, MAPE = MAPE,
                               rmse_skill = skill_score(RMSE),
                               crps_skill = skill_score(CRPS), ACF1 =ACF1,
                               winkler = winkler_score))

  }

  prophet_fits <-
    future_map_dfr(.x =  regions,
                   .f = ~prophetCV(data = filter(data, regional_unit == .x)),
                   .options = furrr_options(seed = TRUE))
  message("prophetModelsBound() complete")

  # Fable models will run using plan(cluster), but need to be run as below
  # with `furrr_options(seed = TRUE)` otherwise improper random numbers are
  # generated
  fable_fits <-
    future_map_dfr(.x =  regions,
                   .f = ~fableCV(data = filter(data, regional_unit == .x)),
                   .options = furrr_options(seed = TRUE))
  message("fableModelsBound() complete")

  bind_rows(fable_fits, prophet_fits) %>%
    group_by(regional_unit) %>%
    arrange(RMSE, MAE)
}

regionalJointFcsts <- function(data, h ="6 months"){
  regions <- unique(data$regional_unit)

  fable_fcst <-
    future_map_dfr(.x = regions,
                   .f = ~fableModelsBound(filter(data, regional_unit ==.x)) %>%
                     forecast(h = h),
                   .options = furrr_options(seed = TRUE)) %>%
    as_tibble()

  message("fable forecasts complete")


  prophet_fcst <-
    future_map_dfr(.x = regions,
                   .f = ~prophetModelsBound(filter(data, regional_unit ==.x)) %>%
                     forecast(h = h),
                   .options = furrr_options(seed = TRUE)) %>%
    as_tibble()

  message("prophet forecasts complete")

  joint_fcst <-
    bind_rows(fable_fcst, prophet_fcst) %>%
    mutate(lb_95 = quantile(n, .025),
           lb_80 = quantile(n, .1),
           med = median(n),
           ub_80 = quantile(n, .9),
           ub_95 = quantile(n, .975),
           sd = distributional::variance(n)^2,
           n = NULL # Remove distribution from saved object
    )

  return(joint_fcst)
}
