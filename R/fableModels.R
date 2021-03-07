#' @title Fable model fits
#'
#' @description  Fit various fable time series models.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`
#'
#' @import dplyr
#' @import fable
#' @import fabletools
#' @import feasts
#' @import tsibble
#' @import fasster
#'
#' @return Dataframe of fable model fits
#' @export
#'
#' @examples
#' \dontrun{fableModels(data)}

fableModels <-
  function(data){

    if(!is_tsibble(data)){data <- as_tsibble(data, index = datename)}

    data %>%
      mutate(datename = yearmonth(datename)) %>%
      model(trend_model1 = TSLM(log(n) ~ trend()),#time series linear model
            trend_model2 = TSLM(log(n) ~ trend() + season("1 year")),
            ets1 = ETS(log(n) ~ trend("N") + season("N")), # Simple exponential smoothing
            ets2 = ETS(log(n) ~ trend("A") + season("A")), # Holt-Winters Additive Model
            arima_step = ARIMA(log(n)), # Default stepwise method
            # arima_search = ARIMA(log(n), stepwise = FALSE), # Search larger space
            fasster = FASSTER(log(n) ~ season("1 year") + trend(1) + fourier(12)),
            stl_dcmp1 = decomposition_model(STL(log(n) ~ trend(window = 21),
                                                robust = TRUE),
                                            NAIVE(season_adjust)),
            stl_dcmp2 = decomposition_model(STL(log(n) ~ trend(window = 21) +
                                                  season(window = "periodic"), # Fixed seasonality
                                                robust = TRUE),
                                            NAIVE(season_adjust)),
            naive = NAIVE(log(n)),
            naive_drift = NAIVE(log(n) ~ drift()),
            s_naive = SNAIVE(log(n) ~ lag("year")),
            s_naive_drift = SNAIVE(log(n) ~ lag("year") + drift()),
            comb1 = combination_model(TSLM(log(n) ~ trend()),
                                      ETS(log(n) ~ trend("N") + season("N"))),
            comb2 = combination_model(TSLM(log(n) ~ trend() + season("1 year")),
                                      ETS(log(n) ~ trend("A") + season("A"))),
            comb3 = combination_model(ARIMA(log(n), stepwise = FALSE),
                                      decomposition_model(STL(log(n) ~ trend(window = 21),
                                                              robust = TRUE),
                                                          SNAIVE(season_adjust))),
            .safely = TRUE)
  }
