#' @title Prepare dispensing data for time series analysis
#'
#' @description Function to prepare dispensing data for time series analysis
#'
#' @param data Data to be prepared
#'
#' @import dplyr
#' @import janitor
#' @import tsibble
#'
#' @return tsibble
#' @export
#'
#' @examples
#' \dontrun{monthlyPrescribing(drug)}
#'

monthlyPrescribing <-
  function(data){
    data %>%
      janitor::clean_names() %>%
      mutate(datename = dmy(paste0("28-", datename)),
             practice_id = as.character(practice_id)) %>%
      countRegional(practice_id, datename, quantity) %>%
      mutate(year = year(datename)) %>%
      as_tibble() %>%
      inner_join(gp %>%
                   select(practice_id, year, npat),
                 by = c("regional_unit" = "practice_id", "year")) %>%
      mutate(n = 1000*n/npat,
             datename = yearmonth(datename),
             regional_unit = as_factor(regional_unit)) %>%
      select(-c(npat, year)) %>%
      as_tsibble(index = datename, key = regional_unit) %>%
      fill_gaps()
  }
