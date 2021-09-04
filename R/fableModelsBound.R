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
      model(trend_model1 = TSLM(my_scaled_logit(n) ~ trend()),#time series linear model
            trend_model2 = TSLM(my_scaled_logit(n) ~ trend() + season("1 year")),
            ets1 = ETS(my_scaled_logit(n) ~ trend("N") + season("N")), # Simple exponential smoothing
            ets2 = ETS(my_scaled_logit(n) ~ trend("A") + season("A")), # Holt-Winters Additive Model
            arima_step = ARIMA(my_scaled_logit(n)), # Default stepwise method
            # arima_search = ARIMA(my_scaled_logit(n), stepwise = FALSE), # Search larger space
            # fasster = FASSTER(my_scaled_logit(n) ~ season("1 year") + trend(1) + fourier(12)),
            stl_dcmp1 = decomposition_model(STL(my_scaled_logit(n) ~ trend(window = 21),
                                                robust = TRUE),
                                            NAIVE(season_adjust)),
            stl_dcmp2 = decomposition_model(STL(my_scaled_logit(n) ~ trend(window = 21) +
                                                  season(window = "periodic"), # Fixed seasonality
                                                robust = TRUE),
                                            NAIVE(season_adjust)),
            naive = NAIVE(my_scaled_logit(n)),
            naive_drift = NAIVE(my_scaled_logit(n) ~ drift()),
            s_naive = SNAIVE(my_scaled_logit(n) ~ lag("year")),
            s_naive_drift = SNAIVE(my_scaled_logit(n) ~ lag("year") + drift())) %>%
      mutate(
        comb1 = (trend_model1 + ets1) / 2,
        comb2 = (trend_model2 + ets2) / 2,
        comb3 = (arima_step + stl_dcmp1) / 2,
        comb4 = (arima_step + stl_dcmp2) / 2,
        comb5 = (arima_step + stl_dcmp2 + s_naive_drift) / 3,
        # comb6 = (arima_step + fasster) / 2
        )
  }
