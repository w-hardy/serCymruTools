#' @title Sumamry of prescribing volume for a regional unit
#'
#' @description Function to create tsibble object for a count of `unit` per `datename` e.g. countRegional(oc, Locality, datename, Quantity) is the `Quantity` per `datename` grouped by `Locality` from `oc`. `Quantity` is corrected for the number of days in each month.
#'
#' @param data A dataframe with date and unit of medicines
#' @param region Column name of regional unit of interest (e.g., `practiceid`, `gp_cluster`)
#' @param datename Name of the colume containing the date data
#' @param unit Unit of interest (e.g., `quantity`, `items`)
#'
#'
#' @import dplyr
#' @import forcats
#' @import tsibble
#'
#' @return Dataframe of units prescribed grouped by date and regional unit
#' @export
#'
#' @examples
#' \dontrun{countRegional(data, region = practiceid)}
#'
countRegional <- function(data, region, datename = datename, unit = Quantity){
  region <- enquo(region)
  datename <- enquo(datename)
  unit <- enquo(unit)

  data %>%
    mutate(regional_unit = as_factor(set_names(!!region))) %>%
    group_by(regional_unit, !!datename) %>%
    summarise(n = sum(!!unit)) %>%
    mutate(n = case_when(str_detect(!!datename, pattern = "-01-") ~ n/31,
                         str_detect(!!datename, pattern = "-02-") ~ n/28.25,
                         str_detect(!!datename, pattern = "-03-") ~ n/31,
                         str_detect(!!datename, pattern = "-04-") ~ n/30,
                         str_detect(!!datename, pattern = "-05-") ~ n/31,
                         str_detect(!!datename, pattern = "-06-") ~ n/30,
                         str_detect(!!datename, pattern = "-07-") ~ n/31,
                         str_detect(!!datename, pattern = "-08-") ~ n/31,
                         str_detect(!!datename, pattern = "-09-") ~ n/30,
                         str_detect(!!datename, pattern = "-10-") ~ n/31,
                         str_detect(!!datename, pattern = "-11-") ~ n/30,
                         str_detect(!!datename, pattern = "-12-") ~ n/31)) %>%
    ungroup() %>%
    as_tsibble(index = !!datename, key = regional_unit)
}
