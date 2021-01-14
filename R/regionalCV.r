#' @title Cross-validated model fits
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

regionalCV <- function(data, cv_dist = 6, init = 36, step = 1){

  # CV accuracy for fableModels() and prophetModels()

  regions <- unique(data$regional_unit) # Get unique regional_units for map()

  p <- progressr::progressor(along = regions)

  fableCV <- function(data){ # Return CV model accuracy for fableModels()
    data_test <-
      data %>%
      mutate(datename = yearmonth(datename))

    data %>%
      select(regional_unit, datename, n) %>%
      mutate(datename = yearmonth(datename)) %>%
      fill_gaps() %>%
      slice(1:(n()-cv_dist), .preserve = TRUE) %>%
      stretch_tsibble(.init = init, .step = step) %>%
      fableModels() %>%
      forecast(h = cv_dist) %>%
      accuracy(data_test)

    p(sprintf("x=%g"))

  }

  prophetCV <- function(data){# Return CV model accuracy for prophetModels()
    data_test <-
      data %>%
      mutate(datename = yearmonth(datename))

    data %>%
      select(regional_unit, datename, n) %>%
      mutate(datename = yearmonth(datename)) %>%
      fill_gaps() %>%
      slice(1:(n()-cv_dist), .preserve = TRUE) %>%
      stretch_tsibble(.init = init, .step = step) %>%
      prophetModels() %>%
      forecast(h = cv_dist) %>%
      accuracy(data_test)

    p(sprintf("x=%g"))

  }

  prophet_fits <-
    future_map_dfr(.x =  regions,
                   .f = ~prophetCV(data = filter(data, regional_unit == .x)),
                   .options = furrr_options(seed = TRUE))
  message("prophetModels() complete")

  # Fable models will run using plan(multisession), but need to be run as below
  # with `furrr_options(seed = TRUE)` otherwise improper random numbers are
  # generated
  fable_fits <-
    future_map_dfr(.x =  regions,
                   .f = ~fableCV(data = filter(data, regional_unit == .x)),
                   .options = furrr_options(seed = TRUE))
  message("fableModels() complete")

  bind_rows(fable_fits, prophet_fits) %>%
    group_by(regional_unit) %>%
    arrange(RMSE, MAE)
}
