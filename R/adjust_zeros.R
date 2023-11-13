#' Adjust zeros to make relativizing, logging, etc smoother
#'
#' @param data dataframe
#' @param adjust_col column to adjust (numeric)
#' @param amount how much to adjust, default 0. Either numeric or `'auto'`. Auto shifts by
#'   `0.1*min(abs(data[data != 0]))`. If all nonzero data is positive or negative, the zeros are shifted only up or down. Otherwise, they are shifted randomly up and down `abs(amount)`
#' @param onlyzeros logical, default `FALSE`. Shift all data or only zeros (`TRUE`). Nonzero data is shifted away from 0 in whatever direction it already is if `FALSE`
#'
#' @return dataframe with no zeros in `adjust_col`

adjust_zeros <- function(data, adjust_col, amount, onlyzeros = FALSE) {

  # handle 'auto' adjustment
  if (grepl('auto', amount)) {
    amount <- min(abs(data[[adjust_col]])[data[[adjust_col]] != 0], na.rm = TRUE)*0.1
  }


  # if the data doesn't span 0, only move in the direction of the data. This is especially important if we'll be logging
  adjust_dir <- dplyr::case_when(all(data[[adjust_col]] >= 0, na.rm = TRUE) ~ 'pos',
                                 all(data[[adjust_col]] <= 0, na.rm = TRUE) ~ 'neg',
                                 .default = 'both')

  if (onlyzeros) {
    data <- data |>
      dplyr::mutate("{adjust_col}" := dplyr::case_when(.data[[adjust_col]] != 0 ~ .data[[adjust_col]],
                                                       .data[[adjust_col]] == 0  & adjust_dir == 'pos' ~ .data[[adjust_col]] + amount,
                                                       .data[[adjust_col]] == 0  & adjust_dir == 'neg' ~ .data[[adjust_col]] - amount,
                                                       .data[[adjust_col]] == 0  & adjust_dir == 'both' ~ .data[[adjust_col]] + sample(c(amount, -1*amount), 1)))
  } else {
    data <- data |>
      dplyr::mutate("{adjust_col}" := dplyr::case_when(.data[[adjust_col]] > 0 ~ .data[[adjust_col]] + amount,
                                                       .data[[adjust_col]] < 0 ~ .data[[adjust_col]] - amount,
                                                       .data[[adjust_col]] == 0  & adjust_dir == 'pos' ~ .data[[adjust_col]] + amount,
                                                       .data[[adjust_col]] == 0  & adjust_dir == 'neg' ~ .data[[adjust_col]] - amount,
                                                       .data[[adjust_col]] == 0  & adjust_dir == 'both' ~ .data[[adjust_col]] + sample(c(amount, -1*amount), 1)))
  }


}
