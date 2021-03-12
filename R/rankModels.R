#' @title rankModels
#'
#' @description Rank models according to rmse and winkler scores
#'
#' @importFrom stats na.omit
#'
#' @param data Tibble of model accuracies including rmse and winkler
#'
#' @export
#'

rankModels <-
  function(data){
    data %>%
      na.omit() %>%
      dplyr::mutate(rmse_rank = rank(RMSE),
                    winkler_rank = rank(winkler),
                    rank = (rmse_rank + winkler_rank)/2) %>%
      dplyr::arrange(rank)
  }
