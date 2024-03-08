agg_theme_space <- make_test_agg(namehistory = FALSE)
# create a quant description of scenarios
scenarios <- tibble::tibble(scenario = c("base_base", "down4_down4", "up4_up4", "MAX"), delta = c(1, 0.25, 4, Inf))

obj_sdl_to_plot <- agg_theme_space$sdl_units |>
  dplyr::mutate(env_group = stringr::str_extract(env_obj, "^[A-Z]+")) |>
  dplyr::filter(!is.na(env_group)) |>
  dplyr::arrange(env_group, env_obj) |>
  # and join the quant descriptions
  dplyr::left_join(scenarios, by = "scenario")

basin_to_plot <- agg_theme_space$mdb |>
  dplyr::filter(!is.na(Objective))

scene_pal <- make_pal(
  levels = unique(obj_sdl_to_plot$scenario),
  palette = "calecopal::superbloom3"
)

test_that("werp plots work", {
  basin_plot <- plot_outcomes(basin_to_plot,
    outcome_col = "ewr_achieved",
    colorset = "Objective",
    pal_list = list("scico::oslo"),
    sceneorder = c("down4_down4", "base_base", "up4_up4")
  ) +
    ggplot2::theme(legend.position = "none")

  basin_table <- make_plot_table(basin_plot)

  expect_snapshot(basin_table)

  sdl_plot <- obj_sdl_to_plot |>
    plot_outcomes(
      outcome_col = "ewr_achieved",
      x_col = "env_obj",
      colorgroups = NULL,
      colorset = "scenario",
      pal_list = scene_pal,
      facet_row = "SWSDLName",
      facet_col = ".",
      sceneorder = c("down4_down4", "base_base", "up4_up4")
    )

  sdl_table <- make_plot_table(sdl_plot)

  expect_snapshot(sdl_table)
})

test_that("non-werp fails", {
  iris_plot <- ggplot2::ggplot(
    iris,
    ggplot2::aes(
      x = Sepal.Length, y = Petal.Width,
      color = Species
    )
  ) +
    ggplot2::geom_point()

  expect_error(make_plot_table(iris_plot))
})
