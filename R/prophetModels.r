#' @title Prophet model fits
#'
#' @description Fit various prophet time series models.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`
#'
#' @import dplyr
#' @import rstan
#' @import fable.prophet
#' @import tsibble
#'
#' @return Dataframe of prophet model fits
#' @export
#'
#' @examples
#' \dontrun{prophetModels(data)}
#'
prophetModels <-
  function(data){
    if(!is_tsibble(data)){data <- as_tsibble(data, index = datename)}

    holidays <-
      bind_rows(tibble(holiday = "christmas",
                       ds = as.Date(c("2016-12-25", "2017-12-25", "2018-12-25",
                                      "2019-12-25", "2020-12-25", "2021-12-25")),
                       lower_window = -1,
                       upper_window = 1),

                tibble(holiday = "easter", # Sunday
                       ds = as.Date(c("2016-03-27", "2017-04-16", "2018-04-01",
                                      "2019-04-21", "2020-04-12", "2021-04-04")),
                       lower_window = -2,
                       upper_window = 1))

    oc_prophet_model1 <-
      fable.prophet::prophet(log(n) ~ growth("linear", n_changepoints = 0) +
                               season("year", type = "additive") +
                               holiday(holidays))
    # oc_prophet_model2 <-
    #   fable.prophet::prophet(log(n) ~ growth("linear", n_changepoints = 0) +
    #                            season("year", type = "multiplicative") +
    #                            holiday(holidays))
    oc_prophet_model3 <-
      fable.prophet::prophet(log(n) ~ growth("linear") +
                               season("year", type = "additive") +
                               holiday(holidays))
    # oc_prophet_model4 <-
    #   fable.prophet::prophet(log(n) ~ growth("linear") +
    #                            season("year", type = "multiplicative") +
    #                            holiday(holidays))
    oc_prophet_model5 <-
      fable.prophet::prophet(log(n) ~ season("year", type = "additive") +
                               holiday(holidays))
    # oc_prophet_model6 <-
    #   fable.prophet::prophet(log(n) ~ season("year", type = "multiplicative") +
    #                            holiday(holidays))
    oc_prophet_model7 <-
      fable.prophet::prophet(log(n) ~ holiday(holidays))
    oc_prophet_model8 <-
      fable.prophet::prophet(log(n))

    data %>%
      mutate(datename = yearmonth(datename)) %>%
      model(oc_prophet_model1,
            oc_prophet_model3,
            oc_prophet_model5,
            oc_prophet_model7,
            oc_prophet_model8,
            # oc_prophet_model2,
            # oc_prophet_model4,
            # oc_prophet_model6,
            .safely = TRUE)
  }
