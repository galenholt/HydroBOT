test_that("group_until parser", {
  # basic usage
  aggseq <- list(
    ewr_code = c("ewr_code_timing", "ewr_code"),
    env_obj = c("ewr_code", "env_obj"),
    sdl_units = sdl_units,
    Specific_goal = c("env_obj", "Specific_goal"),
    catchment = cewo_valleys,
    Objective = c("Specific_goal", "Objective"),
    mdb = basin,
    target_5_year_2024 = c("Objective", "target_5_year_2024")
  )


  gv <- parse_group_until(c(NA, 'sdl_units'),
                          c('scenario', 'planning_unit_name'),
                          aggseq)
  gn <- parse_group_until(c(NA, 3),
                          c('scenario', 'planning_unit_name'),
                          aggseq)
  gf <- parse_group_until(c(NA, is_notpoint),
                          c('scenario', 'planning_unit_name'),
                          aggseq)
  gl2 <- parse_group_until(list(scenario = NA, planning_unit_name = is_notpoint),
                          c('scenario', 'planning_unit_name'),
                          aggseq)
  gl1 <- parse_group_until(list(planning_unit_name = is_notpoint),
                           c('scenario', 'planning_unit_name'),
                           aggseq)
  gR <- parse_group_until(gv,
                          c('scenario', 'planning_unit_name'),
                          aggseq)

  expect_snapshot_output(list(gv, gn, gf, gl2, gl1, gR))

})
