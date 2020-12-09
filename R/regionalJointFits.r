#' @title Forecasts for each regional unit
#'
#' @description Various fable and prophet forecasts for monthly quantity for each regional_unit.
#'
#' @param data A dataframe with time as `datename` and quantity as `n`.
#'
#' @import dplyr
#' @import furrr
#' @import future
#'
#' @return Dataframe of model fits
#' @export
#'
#' @examples
#' \dontrun{regionalJointFcsts(data)}
#'
regionalJointFits <-
  function(data){
    regions <- unique(data$regional_unit)

    plan(multisession)
    fable_fit <-
      future_map_dfr(.x = regions,
                     .f = ~fableModels(filter(data, regional_unit == .x)),
                     .options = furrr_options(seed = TRUE))
    message("fable fits complete")


    prophet_fit <-
      future_map_dfr(.x = regions,
              .f = ~prophetModels(filter(data, regional_unit == .x)),
              .options = furrr_options(seed = TRUE))
    message("prophet fits complete")

    plan(sequential)

    joint_fit <-
      left_join(fable_fit, prophet_fit, by = "regional_unit")

    return(joint_fit)
  }
