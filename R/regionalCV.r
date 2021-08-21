#' @title Cross-validated model fits
#'
#' @description Cross-validated fits of various fable models. Will take advantage of plan() if called outside of function.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`.
#' @param cv_dist The number of months that should be cross-validated.
#' @param init The number of months that cross-validations should start from.
#' @param step The number of months between each cross-validation step.
#'
#' @import dplyr
#' @import fabletools
#' @import furrr
#' @import future
#' @import tsibble
#' @import progressr
#'
#' @return Dataframe of model fits
#' @export
#'
#' @examples
#' \dontrun{regionalCV(data)}
#'

regionalCV <- function(data, cv_dist = 8, init = 36, step = 3){

  # CV accuracy for fableModels()

  regions <- unique(data$regional_unit) # Get unique regional_units for map()


  fableCV <- function(data){ # Return CV model accuracy for fableModels()

    x <- data$regional_unit

    data_test <-
      data %>%
      mutate(datename = yearmonth(datename))

    data %>%
      select(regional_unit, datename, n) %>%
      mutate(datename = yearmonth(datename)) %>%
      fill_gaps() %>%
      slice(1:(n()-cv_dist), .preserve = TRUE) %>% # Exclude last 2 months of training data
      stretch_tsibble(.init = init, .step = step) %>%
      fableModels() %>%
      forecast(h = cv_dist) %>%
      group_by(.id) %>%
      mutate(h = .id) %>%
      ungroup() %>%
      accuracy(data_test, list(RMSE = RMSE, MAE = MAE, MAPE = MAPE,
                               rmse_skill = skill_score(RMSE),
                               crps_skill = skill_score(CRPS), ACF1 =ACF1,
                               winkler = winkler_score)) %>%
      group_by(regional_unit, .model) %>%
      summarise(regional_unit = regional_unit,
                across(where(is.numeric), mean), .groups = "keep")

  }


  # Fable models will run using plan(multisession), but need to be run as below
  # with `furrr_options(seed = TRUE)` otherwise improper random numbers are
  # generated
  fable_fits <-
    future_map_dfr(.x =  regions,
                   .f = ~fableCV(data = filter(data, regional_unit == .x)),
                   .options = furrr_options(seed = TRUE))
  message("fableModels complete")

  fable_fits %>%
    group_by(regional_unit) %>%
    arrange(RMSE, MAE)
}
