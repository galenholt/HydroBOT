#' Make a table of the aggregations from a plot
#'
#' @param werp_plot A ggplot object with WERP-style data underneath. Needs to have the aggregation sequence in columns, not the column name.
#'
#' @return a table with the aggregation step, the function used, and the level of aggregation
#' @export
#'
make_plot_table <- function(werp_plot) {
  # I'm not convinced we need more than this- the facets etc should be
  # self-explanatory, but I'll leave the framework here if we want to do more.
  aggseqs <- get_data_agg(werp_plot)

  return(aggseqs)
}

#' The data aggregation extractor for make_plot_table
#'
#' @inheritParams make_plot_table
#'
get_data_agg <- function(werp_plot) {

  # expects !namehistory. we could generate it wiht agg_names_to_cols, but then
  # we'd need the sequences. so I think just make the user do this for now.

  dataseq <- werp_plot$data |>
    sf::st_drop_geometry() |>
    dplyr::select(tidyselect::starts_with("agg")) |>
    dplyr::distinct()

  # Check there's anything (ie are there columsn with the aggregations?)
  if (ncol(dataseq) == 0 | nrow(dataseq) == 0) {
    rlang::abort(c('Data used for this plot does not contain aggregation information.',
                 'i' = 'If this is werp data, has it been processed after the aggregator?',
                 'i' = 'If this is werp data, does it still have the history in names? If so, use `agg_names_to_cols()`',
                 'i' = 'If this is not werp data, this function is not appropriate.'))
  }

  # I should include a 'startingLevel' or 'aggLevel_0', but it's not obvious how in general. If we always start with a theme-scale agg, we could pass in the full aggsequence rather than just the names, and use the first item in the first one. But that won't work if we immediately aggregate spatially. For now, let's see if we can auto-set based on the data type.
  if (any(stringr::str_detect(names(werp_plot$data), "ewr"))) {
    startinglevel <- "ewr_code_timing"
  } else {
    rlang::inform("Unknown data type, setting starting level to Unknown")
    startinglevel <- "unknown"
  }

  dataseqtab <- dataseq |>
    tidyr::pivot_longer(cols = tidyselect::everything()) |>
    dplyr::mutate(name = stringr::str_remove_all(.data$name, "agg")) |>
    tidyr::separate("name", into = c("type", "step"), sep = "_") |>
    tidyr::pivot_wider(id_cols = "step", names_from = "type", values_from = "value") |>
    dplyr::rename(
      `aggregation function` = "fun",
      `aggregation level` = "Level"
    )

  # add the starting level
  dataseqtab <- dplyr::bind_rows(tibble::tibble(step = '0',
                                                `aggregation function` = NA,
                                                `aggregation level` = startinglevel),
                                 dataseqtab)

  return(dataseqtab)
}
