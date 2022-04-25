#' @title Prepare data for GLS analysis
#'
#' @description Function to prepare difference data for GLS analysis
#'
#' @param data A drug to filter `differences` by
#'
#' @import dplyr
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{glsData(drug)}
#'

glsData <- function(data){
  df <-
    differences %>%
    filter(drug == data) %>%
    mutate(Month = as.numeric(as.factor(datename)),
           pct_change = 100*(prop - 1)) %>%
    select(c(regional_unit, Month, pct_change)) %>%
    filter(Month > 2) %>%
    left_join(gp_2020, by = "regional_unit") %>%
    mutate(gp_load = npat / ngp)

  dd <- datadist(df)
  options(datadist = "dd")

  return(df)
}
