#' Some standard data preparation for plotting
#'
#' Handles common data preparation for plotting. One of the goals here is to
#' avoid making a million very similar datasets- doing it in functions keeps
#' those changes sandboxed and allows consistent error checking and formatting.
#'
#' @param data dataframe to prep
#' @param outcome_col character, column name for outcome variable.
#' @param sceneorder character or factor giving the order to present scenario
#'   levels. Default NULL uses default ordering.
#' @param base_list NULL (default) or list of arguments for [baseline_compare()]
#' @param zero_adjust numeric (default 0) or `"auto"`, adjustment to data to
#'   avoid zeros by adding `zero_adjust` to `abs(data)`, e.g shifting all data
#'   away from zero, either positively or negatively. Zeros themselves are
#'   shifted up or down randomly. Used for avoiding x/0, NaN, and Inf when
#'   relativiszing and taking logs, primarily. Auto shifts by
#'   `0.1*min(abs(data[data != 0]))`.
#'   *note* the adjustment happens *before* baselining (so the baselining works), but if the baseline reintroduces zeros, they will not be re-adjusted out. This is done under the expectation that zeros returned by baselining are desired, e.g. difference of a baseline with itself.
#' @param onlyzeros logical, default `FALSE`. Should all values be adjusted away
#'   from zero (`TRUE`) or only adjust zero values (`FALSE`)?
#'
#' @return a list with prepped versions of `data`, `outcome_col`, `ylab_append`
#'   to be used in plot calls
#' @export
#'
#' @examples
plot_data_prep <- function(data, outcome_col,
                      sceneorder = NULL,
                      base_list = NULL,
                      zero_adjust = 0,
                      onlyzeros = FALSE) {

  # Order the scenario if I've given it an order (and if it exists).
  if (!is.null(sceneorder) && 'scenario' %in% names(data)) {
    if (inherits(sceneorder, 'factor')) {sceneorder <- levels(sceneorder)}
    data <- data |>
      dplyr::mutate(scenario = forcats::fct_relevel(scenario, sceneorder))
  }

  # if outcome_col is NULL, as happens with maps sometimes, or non-numeric, just skip
  # this entirely there's no y data to adjust mathematically
  if (is.null(outcome_col) || !is.numeric(data[[outcome_col]])) {
    ylab_append = ''
    return(tibble::lst(data, outcome_col, ylab_append))
  }

  # Adjust to keep off zero if requested, but warn if not relative
  # Not sure this is necessary.
  if (is.null(base_list) & zero_adjust != 0) {
    if (!('relative' %in% base_list$comp_fun)) {
      rlang::warn(glue::glue("`comp_fun` is {base_list$comp_fun}, but you're adjusting the data by {zero_adjust}. Do you really want to do that? It may be appropriate if you're log-transforming or dividing, but be careful.\n"))
    }
  }

  if (grepl('auto', zero_adjust)) {
    zero_adjust <- min(abs(data[[outcome_col]])[data[[outcome_col]] != 0], na.rm = TRUE)*0.1
  }
  # move data away from zero
  data <- adjust_zeros(data, outcome_col, zero_adjust, onlyzeros)

  # Baseline data
  ylab_append <- ''
  if (!is.null(base_list)) {
    # We've already handled the zero-shifts, so don't do it again
    data <- data |>
      baseline_compare(compare_col = 'scenario', base_lev = base_list$base_lev,
                       values_col = outcome_col, comp_fun = base_list$comp_fun,
                       group_cols = base_list$group_cols,
                       zero_adjust = 0, onlyzeros = TRUE)

    if (is.character(base_list$comp_fun)) {
      comp_fun_name <- base_list$comp_fun
    } else if (is.list(base_list$comp_fun)) {
      comp_fun_name = names(base_list$comp_fun)
    } else {
      rlang::abort("getting the name of comp_fun isn't supported,
                       likely because of bare names lost in the call stack.
                       Please use character or list-defined functions. ")
    }
    base_col_name <- paste0(comp_fun_name, '_', outcome_col)
    names(data)[names(data) == paste0('comp_fun_', outcome_col)] <- base_col_name
    outcome_col <- base_col_name
    ylab_append <- paste0('\n', comp_fun_name, ' to ', as.character(base_list$base_lev))
  }

  # That can introduce some infs and nans, often from division by zero in the relativiser
  new_nan_inf <- sum(is.nan(data[[outcome_col]])) + sum(is.infinite(data[[outcome_col]]))
  old_nan_inf <- sum(is.nan(data[[outcome_col]])) + sum(is.infinite(data[[outcome_col]]))
  if (new_nan_inf > old_nan_inf) {
    rlang::warn(glue::glue("NaN and Inf introduced in `plot_data_prep`, likely due to division by zero. {new_nan_inf - old_nan_inf} values were lost."))
  }

  # This names the data the same thing as it was interactively, but fails in the function. Just call it data for consistency
  # dataname <- as.character(substitute(data))
  return(tibble::lst(data, outcome_col, ylab_append))
}

#' Some data preparation, checking, and type-finding based on the type of data
#' and the way we want plots to look
#'
#' @inheritParams plot_outcomes
#'
#' @param prepped a prepped data list returned by [plot_data_prep()]
#'
#' @return a prepped data list with more information needed to make the plots
#' @export
#'
#' @examples
plot_style_prep <- function(prepped, colorset, colorgroups, pal_list, pal_direction,
                            transoutcome, transx, point_group) {
  # Warn about conflicts with outcome trans
  if (!is.null(prepped$outcome_col)) {
    if (!inherits(transoutcome, 'trans') &&
        transoutcome %in% c('log', 'log10') &
        any(prepped$data[[prepped$outcome_col]] == 0)) {
      rlang::warn(glue::glue("`transoutcome` takes logs, but data has
                           {sum(prepped$data[[prepped$outcome_col]] == 0)} zeros and
                           {sum(prepped$data[[prepped$outcome_col]] < 0)} less than 0,
                           which will get lost"))
    }
  }


  # Handle different color options
  color_type <- find_color_type(pal_list)

  # abort if direction is the wrong length
  if ((grepl('paletteer', color_type) | grepl('grouped', color_type)) &
      (length(pal_list) != length(pal_direction))) {
    rlang::abort(glue::glue("pal_list is trying to choose paletteer colors from a list {length(pal_list)} long,
                            but pal_direction is {length(pal_direction)} long. They need to match."))
  }

  # Name the direction vector This should be fine in all cases, but really only
  # needs to happen if paletteer or grouped if it starts causing problems
  names(pal_direction) <- names(pal_list)

  # if the data is qualitative but the palette is continuous, we need to make a named palette
  if (!is.null(colorset)) {
    if (is.numeric(dplyr::pull(sf::st_drop_geometry(prepped$data[,colorset, drop = FALSE])))) {
      dataqualquant <- 'quant'
    } else {
      dataqualquant <- 'qual'
    }
  } else {
    dataqualquant <- 'none'
  }

  if (color_type == 'paletteer_c' & dataqualquant == 'qual') {
    pal_list <- make_pal(prepped$data[[colorset]], pal_list[[1]],
                         direction = pal_direction[1])
    color_type <- 'colorobj'
  }

  # if the data is quantitative but the palette is discrete, we need to do something, but maybe just fail.
  if (color_type == 'paletteer_d' & dataqualquant == 'quant') {
    rlang::abort(glue::glue("Using discrete palette {pal_list[[1]]} for continous data {colorset}"))
  }

  # This is annoying, but having a column with the same name lets us avoid a lot of duplicated code
  if (color_type == 'grouped') {
    prepped$data <- grouped_colors(prepped$data,
                                   pal_list = pal_list,
                                   pal_direction = pal_direction,
                                   colorgroups = colorgroups,
                                   colorset = colorset)

    prepped$data <- prepped$data |>
      dplyr::mutate(color = forcats::fct_inorder(color)) # Works, but explictly making it line up with colordef might be better?
  } else if (color_type == 'fixed') {
    prepped$data$color <- pal_list[[1]]
  } else {
    prepped$data$color <- prepped$data[[colorset]]
  }

  # Deal with additional point_groups
  if (is.null(point_group)) {
    if (is.null(colorset)) {
      prepped$data$pointgroup <- NA
    } else {
      prepped$data$pointgroup <- prepped$data[[colorset]]
    }
    # prepped$data <- prepped$data |>
    #   dplyr::mutate(pointgroup = ifelse(is.null(colorset), NA,
    #                                     .data[[colorset]]))
  } else {
    prepped$data <- prepped$data |>
      dplyr::mutate(pointgroup = interaction(.data[[colorset]], .data[[point_group]]))
  }

  prepped <- utils::modifyList(prepped, list(color_type = color_type,
                                             pal_list = pal_list,
                                             direction = pal_direction))

}
