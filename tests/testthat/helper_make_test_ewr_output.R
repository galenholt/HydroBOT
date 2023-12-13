make_test_ewr_output <- function() {
  # Set up some base directory structures
  temp_hydro_dir <- '_test_data/hydrographs'
  temp_parent_dir <- '_test_data'

  temp_hydro_multi <- '_test_data/hydrographs'
  temp_parent_multi <- '_test_data'

  # create dir so building makes sense
  make_temp_hydro()

  ewr_out <- prep_run_save_ewrs(hydro_dir = temp_hydro_dir,
                                output_parent_dir = temp_parent_dir,
                                outputType = list('none'),
                                datesuffix = FALSE,
                                returnType = list('summary', 'yearly'))
  return(ewr_out)
}

make_test_ewr_prepped <- function() {
  ewr <- make_test_ewr_output()
  ewr_prepped <- prep_ewr_agg(ewr, geopath = bom_basin_gauges)

  # check that works correctly
  ewrmaps <- ewr$summary |>
    dplyr::select(gauge, planningUnit) |>
    dplyr::distinct() |>
    dplyr::mutate(gaugexpu = paste0(gauge, '_', planningUnit))

  prepmaps <- ewr_prepped |>
    dplyr::select(gauge, planning_unit) |>
    dplyr::distinct() |>
    dplyr::mutate(gaugexpu = paste0(gauge, '_', planning_unit))

  expect_true(all(ewrmaps$gaugexpu %in% prepmaps$gaugexpu) &
                all(prepmaps$gaugexpu %in% ewrmaps$gaugexpu))

  return(ewr_prepped)
}
