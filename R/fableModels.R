#' @title Fable model fits
#'
#' @description  Fit various fable time series models.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`
#' @param value Response variable
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
function(data, value = n){
  if(!is_tsibble(data)){data <- as_tsibble(data, index = datename)}

  value <- enquo(value)

  data %>%
    mutate(datename = yearmonth(datename)) %>%
    model(trend_model1 = TSLM(!!value ~ trend()),#time series linear model
          trend_model2 = TSLM(!!value ~ trend() + season("year")),
          ets1 = ETS(!!value ~ trend()),
          ets2 = ETS(!!value ~ trend() + season("A")),
          ets3 = ETS(!!value ~ trend() + season("M")),
          arima = ARIMA(!!value, stepwise = FALSE, approximation = FALSE),
          #neur_net = NNETAR(!!value),
          #fasster = FASSTER(!!value ~ season("1 year") + trend(1) + fourier(12)),
          comb1 = combination_model(TSLM(!!value ~ trend()),
                                    ETS(!!value ~ trend())),
          comb2 = combination_model(TSLM(!!value ~ trend() + season("year")),
                                    ETS(!!value ~ trend() + season("A"))),
          stl_dcmp1 = decomposition_model(STL(!!value ~ trend(),
                                              iterations = 1000,
                                              robust = TRUE),
                                          SNAIVE(season_adjust)),
          stl_dcmp2 = decomposition_model(STL(!!value ~ trend() + season(),
                                              iterations = 1000,
                                              robust = TRUE),
                                          SNAIVE(season_adjust)),
          s_naive = SNAIVE(!!value))
}
