#' @title Fable model fits for bound data
#'
#' @description  Fit various fable time series models.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`
#' @param lower Lower limit of admissible values
#' @param upper Upper limit of admissible values
#'
#' @import dplyr
#' @import fable
#' @import fabletools
#' @import feasts
#' @import tsibble
#'
#' @return Dataframe of fable model fits
#' @export
#'
#' @examples
#' \dontrun{fableModelsBound(data, lower = 0, upper = 100)}

fableModelsBound <-
  function(data, lower = 0, upper = 100){
    if (upper <= lower) {
      stop("lower must be less than upper")
    }
    if(!is_tsibble(data)){data <- as_tsibble(data, index = datename)}

    data %>%
      mutate(datename = yearmonth(datename)) %>%
      model(trend_model2 = TSLM(my_scaled_logit(n, lower, upper) ~ trend() + season("1 year")),
            ets2 = ETS(my_scaled_logit(n, lower, upper) ~ trend("A") + season("A")), # Holt-Winters Additive Model
            arima = ARIMA(my_scaled_logit(n, lower, upper), stepwise = FALSE),
            comb1 = combination_model(TSLM(my_scaled_logit(n, lower, upper) ~ trend()),
                                      ETS(my_scaled_logit(n, lower, upper) ~ trend())),
            comb2 = combination_model(TSLM(my_scaled_logit(n, lower, upper) ~ trend() + season("1 year")),
                                      ETS(my_scaled_logit(n, lower, upper) ~ trend() + season("A"))),
            stl_dcmp2 = decomposition_model(STL(my_scaled_logit(n, lower, upper) ~ trend() + season("1 year"),
                                                robust = TRUE),
                                            SNAIVE(season_adjust)),
            s_naive = SNAIVE(my_scaled_logit(n, lower, upper)),
            s_naive_drift = SNAIVE(my_scaled_logit(n, lower, upper) ~ drift()),
            .safely = TRUE) %>%
      mutate(comb3 = (arima + stl_dcmp2) / 2)
  }
