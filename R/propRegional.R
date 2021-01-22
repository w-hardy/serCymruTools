#' @title Summary of proportion of a medicine precribed for a regional unit over time
#'
#' @description Function to create tsibble object for a count of `unit` per `datename` e.g. countRegional(oc, Locality, datename, Quantity) is the `Quantity` per `datename` grouped by `Locality` from `oc`. `Quantity` is corrected for the number of days in each month.
#'
#' @param data A dataframe with date and unit of medicines with multiple BNF codes
#' @param region Column name of regional unit of interest (e.g., `practiceid`, `gp_cluster`)
#' @param datename Name of the column containing the date data
#' @param unit Unit of interest (e.g., `quantity`, `items`)
#' @param bnf BNF code of medicine of interest
#'
#'
#' @import dplyr
#' @import forcats
#' @import tsibble
#'
#' @return Dataframe of proportion of `bnf` prescribed grouped by date and regional unit
#' @export
#'
#' @examples
#' \dontrun{propRegional(data, region = practiceid)}
#'
propRegional <- function(data, region, datename = datename, unit = Quantity,
                         bnf){
  # Function to create tsibble object for a proportion of a drug by a given `unit`
  # per `datename`
  # e.g. propRegional(oc, Locality, datename, Quantity) is the `Quantity`
  # per `datename` grouped by `Locality` from `oc`

  region <- enquo(region)
  datename <- enquo(datename)
  unit <- enquo(unit)

  data %>%
    mutate(regional_unit = forcats::as_factor(set_names(!!region))) %>%
    group_by(regional_unit, !!datename, bnfchem) %>%
    summarise(n = sum(!!unit), .groups = "drop_last") %>%
    mutate(n = 100 * n / sum(n)) %>%
    filter(bnfchem == bnf) %>%
    ungroup() %>%
    as_tsibble(index = !!datename, key = regional_unit)
}
