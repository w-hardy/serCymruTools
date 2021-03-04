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
      model(#trend_model1 = TSLM(log(n) ~ trend()),#time series linear model
            trend_model2 = TSLM(log(n) ~ trend() + season("1 year")),
            #ets1 = ETS(log(n) ~ trend()),
            ets2 = ETS(log(n) ~ trend("A") + season("A")), # Holt-Winters Additive Model
            #ets3 = ETS(log(n) ~ trend() + season("M")),
            arima = ARIMA(log(n), stepwise = FALSE),
            #neur_net = NNETAR(log(n)),
            fasster = FASSTER(log(n) ~ season("1 year") + trend(1) + fourier(12)),
            comb1 = combination_model(TSLM(log(n) ~ trend()),
                                      ETS(log(n) ~ trend())),
            comb2 = combination_model(TSLM(log(n) ~ trend() + season("1 year")),
                                      ETS(log(n) ~ trend() + season("A"))),
            # stl_dcmp1 = decomposition_model(STL(log(n) ~ trend(),
            #                                    # iterations = 1000,
            #                                     robust = TRUE),
            #                                 SNAIVE(season_adjust)),
            stl_dcmp2 = decomposition_model(STL(log(n) ~ trend() + season("1 year"),
                                               # iterations = 1000,
                                                robust = TRUE),
                                            SNAIVE(season_adjust)),
            s_naive = SNAIVE(log(n)),
            s_naive_drift = SNAIVE(log(n) ~ drift()),
            .safely = TRUE) %>%
      mutate(comb3 = (arima + stl_dcmp2) / 2)
  }
