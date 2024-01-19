test_that("intersecting works correctly, including area", {
  # we know these intersect
  plantest <- planning_units |> add_polyID()
  sdltest <- sdl_units |> add_polyID()

  expect_equal(sum(duplicated(plantest$PlanningUnitName)), 0)
  expect_equal(sum(duplicated(sdltest$SWSDLName)), 0)

  joinedpusdl <- spatial_joiner(plantest, sdltest, sf::st_crs(sdltest))

  expect_equal(sum(duplicated(joinedpusdl$PlanningUnitName)), 211)
  expect_equal(sum(duplicated(joinedpusdl$SWSDLName)), 441)

  # check one that split to make sure it has in fact split into different areas
  bcws <- joinedpusdl |>
    dplyr::filter(planning_unit_name == 'Baradine Creek Water Source')

  expect_equal(length(unique(bcws$area)), nrow(bcws))

  # re-join just as in spatial_aggregate
  # I don't think these are worth actually saving as tests.

  # dplyr::left_join(bcws, sdltest, by = 'polyID') |>
  #   sf::st_as_sf() |>
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(mapping = ggplot2::aes(fill = log10(area)))
  #
  # dplyr::left_join(bcws, sdltest, by = 'polyID') |>
  #   sf::st_as_sf() |>
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(mapping = ggplot2::aes(fill = SWSDLName.y))

})
