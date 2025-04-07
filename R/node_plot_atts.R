#' Attach attribute columns to node df to define plot
#'
#' This function primarily sets attributes related to size and position.
#' (fontsize, width, height, x, y) as well as shape and tooltips. It effectively
#' sets a default look for the structure of the graph. It also needs work- the
#' numbers are hardcoded, and need to be made relative. It's possible to add
#' other attributes (see `DiagrammeR` documentation), and we'll likely also want
#' to set some of these (especially height and width) functionally (like we do
#' with color)
#'
#' @param nodedf a tibble, the dataframe of nodes
#'
#' @return a tibble of nodes with new attribute columns needed to control the
#'   look in `DiagrammeR`
#' @keywords internal
node_plot_atts <- function(nodedf) {
  # The way I'm getting x is crude, and likely will fail once we have more
  # complex networks
  nodedf <- nodedf |>
    dplyr::mutate(
      shape = "rectangle",
      tooltip = .data$Name,
      # Name = stringr::str_wrap(Name, 40),
      fontsize = 25,
      # width = 5, #stringr::str_length(Name)/2,
      # 1/5 is *approximately* one letter size
      width = ifelse(stringr::str_length(.data$Name) < 40,
        stringr::str_length(.data$Name) / 5 + 0.4,
        40 / 5
      ),
      # x = nodeorder * 15,
      height = (stringr::str_count(.data$Name, "\\n") + 1) / 2
    )

  # If we want things centered, need to get y with grouping
  nodedf <- nodedf |>
    dplyr::group_by(.data$NodeType) |>
    # This sets everything to have a midpoint at 0
    # Trying something less crude. This works to separate the boxes, but gets
    # way too tall
    # dplyr::mutate(num = dplyr::n(),
    #        toth = sum(height, na.rm = TRUE)*2,
    #        mid = toth/2,
    #        rown = cumsum(height*2)-height*2,
    #        y = rown-mid) |>
    # Double columns are a bit better?
    dplyr::mutate(
      num = dplyr::n(),
      mid = .data$num / 2,
      rown = dplyr::row_number(),
      y = .data$rown - .data$mid
    ) |>
    # Kinda silly this way instead of mid-rown, but it keeps things from
    # flipping
    dplyr::mutate(
      basex = .data$nodeorder * 15,
      devx = .data$width / 1.8,
      plusminus = rlang::rep_along(.data$devx, c(1, -1)) * .data$devx,
      x = .data$basex + .data$plusminus
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"num", -"mid", -"rown", "basex", "devx", "plusminus")


  return(nodedf)
}
