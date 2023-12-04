#' Some standard data preparation for plotting
#'
#' This is an attempt to pull a lot of copy-paste out of the top of plot
#' functions. It's likely to continue to evolve quite a bit as we see all the
#' different things we need to manage in the plot setup. One of the goals here
#' is to avoid making a million very similar datasets- doing it in functions
#' keeps those changes sandboxed
#'
#' @param data dataframe to prep
#' @param sceneorder character or factor giving the order to present scenario
#'   levels
#' @param y_col character, column name for what's plotted on the y-axis.
#' @param base_lev value to use as the base for comparison. Default NULL, no
#'   comparison. See [baseline_compare()] and [create_base()] for options.
#' @param comp_fun function to use in comparison. Default NULL, no comparison.
#'   See [baseline_compare()] and [create_base()] for options.
#' @param zero_adjust numeric (default 0) or `"auto"`, adjustment to data to
#'   avoid zeros by adding `zero_adjust` to `abs(data)`, e.g shifting all data
#'   away from zero, either positively or negatively. Zeros themselves are
#'   shifted up or down randomly. Used for avoiding x/0, NaN, and Inf when
#'   relativiszing and taking logs, primarily. Auto shifts by
#'   `0.1*min(abs(data[data != 0]))`.
#' @param onlyzeros logical, default `FALSE`. Should all values be adjusted away from zero (`TRUE`) or only adjust zero values (`FALSE`)?
#'
#' @return a list with prepped versions of `data`, `y_col`, `ylab_append` to
#'   be used in plot calls
#' @export
#'
#' @examples
plot_data_prep <- function(data, y_col,
                      sceneorder = NULL,
                      base_list = NULL,
                      zero_adjust = 0,
                      onlyzeros = FALSE) {

  # if y_col is NULL, as happens with maps sometimes, or non-numeric, just skip
  # this entirely there's no y data to adjust mathematically
  if (is.null(y_col) || !is.numeric(data[[y_col]])) {
    ylab_append = ''
    return(tibble::lst(data, y_col, ylab_append))
  }

  # Data adjustments
  # ensure y is numeric
  # Have to [[]] because [] yields an sf-tibble, not just the col.
  # This was here for a reason, but it seems dangerous- better to put data type on the user.
  # if (!is.numeric(data[[y_col]])) {
  #   data <- data |>
  #     dplyr::mutate("{y_col}" := as.numeric(.data[[y_col]]))
  # }

  # Adjust to keep off zero if requested, but warn if not relative
  # Not sure this is necessary.
  if (is.null(base_list) & zero_adjust != 0) {
    if (!('relative' %in% base_list$comp_fun)) {
      rlang::warn(glue::glue("`comp_fun` is {base_list$comp_fun}, but you're adjusting the data by {zero_adjust}. Do you really want to do that? It may be appropriate if you're log-transforming or dividing, but be careful.\n"))
    }
  }

  if (grepl('auto', zero_adjust)) {
    zero_adjust <- min(abs(data[[y_col]])[data[[y_col]] != 0], na.rm = TRUE)*0.1
  }
  # move data away from zero
  data <- adjust_zeros(data, y_col, zero_adjust, onlyzeros)

  # Baseline data
  ylab_append <- ''
  if (!is.null(base_list)) {
    # We've already handled the zero-shifts, so don't do it again
    data <- data |>
      baseline_compare(compare_col = 'scenario', base_lev = base_list$base_lev,
                       values_col = y_col, comp_fun = base_list$comp_fun, group_cols = base_list$group_cols,
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
    base_col_name <- paste0(comp_fun_name, '_', y_col)
    names(data)[names(data) == paste0('comp_fun_', y_col)] <- base_col_name
    y_col <- base_col_name
    ylab_append <- paste0('\n', comp_fun_name, ' to ', as.character(base_list$base_lev))
  }

  # Order the scenario if I've given it an order.
  if (!is.null(sceneorder)) {
    if (inherits(sceneorder, 'factor')) {sceneorder <- levels(sceneorder)}
    data <- data |>
      dplyr::mutate(scenario = forcats::fct_relevel(scenario, sceneorder))
  }

  # That can introduce some infs and nans, often from division by zero in the relativiser
  new_nan_inf <- sum(is.nan(data[[y_col]])) + sum(is.infinite(data[[y_col]]))
  old_nan_inf <- sum(is.nan(data[[y_col]])) + sum(is.infinite(data[[y_col]]))
  if (new_nan_inf > old_nan_inf) {
    rlang::warn(glue::glue("NaN and Inf introduced in `plot_prep`, likely due to division by zero. {new_nan_inf - old_nan_inf} values were lost."))
  }

  # This names the data the same thing as it was interactively, but fails in the function. Just call it data for consistency
  # dataname <- as.character(substitute(data))
  return(tibble::lst(data, y_col, ylab_append))
}

plot_style_prep <- function(prepped, colorset, colorgroups, pal_list, transy, transx, point_group) {
  # Warn about conflicts with y-axis trans
  if (!is.null(prepped$y_col)) {
    if (!inherits(transy, 'trans') &&
        transy %in% c('log', 'log10') &
        any(prepped$data[[prepped$y_col]] == 0)) {
      rlang::warn(glue::glue("`transy` takes logs, but data has
                           {sum(prepped$data[[prepped$y_col]] == 0)} zeros and
                           {sum(prepped$data[[prepped$y_col]] < 0)} less than 0,
                           which will get lost"))
    }
  }


  # Handle different color options
  color_type <- find_color_type(pal_list)

  # if the data is qualitative but the palette is continuous, we need to make a named palette
  if (!is.null(colorset)) {
    if (is.numeric(dplyr::pull(sf::st_drop_geometry(prepped$data[,colorset])))) {
      dataqualquant <- 'quant'
    } else {
      dataqualquant <- 'qual'
    }
  } else {
    dataqualquant <- 'none'
  }

  if (color_type == 'paletteer_c' & dataqualquant == 'qual') {
    pal_list <- make_pal(prepped$data[[colorset]], pal_list[[1]])
    color_type <- 'colorobj'
  }

  # if the data is quantitative but the palette is discrete, we need to do something, but maybe just fail.
  if (color_type == 'paletteer_d' & dataqualquant == 'quant') {
    rlang::abort(glue::glue("Using discrete palette {pal_list[[1]]} for continous data {colorset}"))
  }

  # This is annoying, but having a column with the same name lets us avoid a lot of duplicated code
  if (color_type == 'grouped') {
    prepped$data <- grouped_colors(prepped$data, pal_list, colorgroups, colorset)
    prepped$data <- prepped$data |>
      dplyr::mutate(color = forcats::fct_inorder(color)) # Works, but explictly making it line up with colordef might be better?
  } else if (color_type == 'fixed') {
    prepped$data$color <- pal_list[[1]]
  } else {
    prepped$data$color <- prepped$data[[colorset]]
  }

  # Deal with additional point_groups
  if (is.null(point_group)) {
    prepped$data <- prepped$data |>
      dplyr::mutate(pointgroup = ifelse(is.null(colorset), NA,
                                        .data[[colorset]]))
  } else {
    prepped$data <- prepped$data |>
      dplyr::mutate(pointgroup = interaction(.data[[colorset]], .data[[point_group]]))
  }

  prepped <- utils::modifyList(prepped, list(color_type = color_type, pal_list = pal_list))

}
