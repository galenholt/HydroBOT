test_that("group_until parser", {
  # basic usage
  aggseq <- list(
    ewr_code_main = c("ewr_code", "ewr_code_main"),
    eco_objective = c("ewr_code_main", "eco_objective"),
    sdl_units = sdl_units,
    objective_text = c("eco_objective", "objective_text"),
    catchment = cewo_valleys,
    theme = c("objective_text", "theme"),
    mdb = basin
  )


  gv <- parse_group_until(
    c(NA, "sdl_units"),
    c("scenario", "planning_unit_name"),
    aggseq
  )
  gn <- parse_group_until(
    c(NA, 3),
    c("scenario", "planning_unit_name"),
    aggseq
  )
  gf <- parse_group_until(
    c(NA, is_notpoint),
    c("scenario", "planning_unit_name"),
    aggseq
  )
  gl2 <- parse_group_until(
    list(scenario = NA, planning_unit_name = is_notpoint),
    c("scenario", "planning_unit_name"),
    aggseq
  )
  gl1 <- parse_group_until(
    list(planning_unit_name = is_notpoint),
    c("scenario", "planning_unit_name"),
    aggseq
  )
  gR <- parse_group_until(
    gv,
    c("scenario", "planning_unit_name"),
    aggseq
  )

  expect_snapshot_output(list(gv, gn, gf, gl2, gl1, gR))
})
