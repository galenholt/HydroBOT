plot_bar <- function(prepped, x_col, x_lab, y_lab,
                     color_type, pal_list,
                     position, transy) {

    outcome_plot <- prepped$data |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[x_col]],
                                   y = .data[[prepped$y_col]],
                                   fill = .data$color)) +
      ggplot2::geom_col(position = position) +
      ggplot2::scale_y_continuous(trans = transy) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))


    outcome_plot <- handle_palettes(outcome_plot, aes_type = 'fill', pal_list, color_type)

    return(outcome_plot)

}


plot_numeric <- function(prepped, x_col, x_lab, y_lab,
                         color_type, pal_list,
                         smooth, position,
                         transy, transx) {

  if (!inherits(position, 'gg')) {
    if (is.null(position) || position == 'stack') {
      position = 'identity'
    }
  }


  outcome_plot <- prepped$data |>
    dplyr::filter(scenario %in% prepped$scenariofilter) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_col]],
                                 y = .data[[prepped$y_col]],
                                 color = .data$color,
                                 group = .data$pointgroup)) +
    ggplot2::geom_point(position = position) +
    ggplot2::scale_y_continuous(trans = transy) +
    ggplot2::scale_x_continuous(trans = transx)

  if (smooth) {
    outcome_plot <- outcome_plot +
      ggplot2::geom_smooth(mapping = ggplot2::aes(fill = .data$color),
                           method = smooth_method,
                           method.args = smooth_args,
                           linewidth = 0.1,
                           se = smooth_se)
  }
  if (!smooth) {
    outcome_plot <- outcome_plot +
      ggplot2::geom_line()
  }

  # do I need to do this for fill as well when I have smooth?
  outcome_plot <- handle_palettes(outcome_plot, aes_type = 'color', pal_list, color_type)

}
