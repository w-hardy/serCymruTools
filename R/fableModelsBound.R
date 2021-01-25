#' @title Fable model fits for bound data
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
#' \dontrun{fableModelsBound(data)}

fableModelsBound <-
  function(data){


    if(!is_tsibble(data)){data <- as_tsibble(data, index = datename)}

    data %>%
      mutate(datename = yearmonth(datename)) %>%
      model(trend_model2 = TSLM(my_scaled_logit(n) ~ trend() + season("1 year")),
            ets2 = ETS(my_scaled_logit(n) ~ trend("A") + season("A")), # Holt-Winters Additive Model
            arima = ARIMA(my_scaled_logit(n), stepwise = FALSE),
            comb1 = combination_model(TSLM(my_scaled_logit(n) ~ trend()),
                                      ETS(my_scaled_logit(n) ~ trend())),
            comb2 = combination_model(TSLM(my_scaled_logit(n) ~ trend() + season("1 year")),
                                      ETS(my_scaled_logit(n) ~ trend() + season("A"))),
            stl_dcmp2 = decomposition_model(STL(my_scaled_logit(n) ~ trend() + season("1 year"),
                                                robust = TRUE),
                                            SNAIVE(season_adjust)),
            s_naive = SNAIVE(my_scaled_logit(n)),
            s_naive_drift = SNAIVE(my_scaled_logit(n) ~ drift()),
            .safely = TRUE) %>%
      mutate(comb3 = (arima + stl_dcmp2) / 2)
  }
