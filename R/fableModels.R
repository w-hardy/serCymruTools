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
      model(#trend_model1 = TSLM(n ~ trend()),#time series linear model
            trend_model2 = TSLM(n ~ trend() + season("1 year")),
            #ets1 = ETS(n ~ trend()),
            ets2 = ETS(n ~ trend("A") + season("A")), # Holt-Winters Additive Model
            #ets3 = ETS(n ~ trend() + season("M")),
            arima = ARIMA(n, stepwise = FALSE),
            #neur_net = NNETAR(n),
            #fasster = FASSTER(n ~ season("1 year") + trend(1) + fourier(12)),
            comb1 = combination_model(TSLM(n ~ trend()),
                                      ETS(n ~ trend())),
            comb2 = combination_model(TSLM(n ~ trend() + season("1 year")),
                                      ETS(n ~ trend() + season("A"))),
            # stl_dcmp1 = decomposition_model(STL(n ~ trend(),
            #                                    # iterations = 1000,
            #                                     robust = TRUE),
            #                                 SNAIVE(season_adjust)),
            stl_dcmp2 = decomposition_model(STL(n ~ trend() + season("1 year"),
                                               # iterations = 1000,
                                                robust = TRUE),
                                            SNAIVE(season_adjust)),
            s_naive = SNAIVE(n),
            s_naive_drift = SNAIVE(n ~ drift()),
            .safely = TRUE) %>%
      mutate(comb3 = (arima + stl_dcmp2) / 2)
  }
