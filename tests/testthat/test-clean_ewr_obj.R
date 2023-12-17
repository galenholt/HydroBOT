# skip_if_no_raw() because data-raw only exists in dev, not build, and these are
# really just consistency checks for the building of the causal networks, which
# happens during the dev process and isn't part of the package per se

ewrpath <- system.file('data-raw/causal_networks/ewr_obj_codes_nsw/obj_codes_dec22.csv', package = 'werptoolkitr')

test_that("no gaugescale produces expected output", {

  skip_if_no_file(ewrpath)
  ewr2obj <- clean_ewr_obj(ewrobjpath = ewrpath,
                           gaugescale = FALSE,
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  expect_s3_class(ewr2obj, 'tbl_df')
  # paste(names(ewr2obj), collapse = '", "')
  ewrnames <- c("LTWPShortName", "ewr_code", "env_obj", "ewr_code_timing")
  expect_equal(names(ewr2obj), ewrnames)

  # being a bit more general here, since the ordering could easily change
  expect_equal(sum(is.na(ewr2obj$ewr_code)), 0)
  expect_snapshot_value(as.list(unique(ewr2obj$ewr_code[!is.na(ewr2obj$ewr_code)])))
  expect_snapshot_value(as.list(unique(ewr2obj$env_obj)))


})

test_that("gaugescale produces expected output", {

  skip_if_no_file(ewrpath)
  ewr2obj <- clean_ewr_obj(ewrobjpath = ewrpath,
                           gaugescale = TRUE,
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  expect_s3_class(ewr2obj, 'tbl_df')
  # paste(names(ewr2obj), collapse = '", "')
  ewrnames <- c('PlanningUnitID',"planning_unit_name", "LTWPShortName", 'gauge', "ewr_code", "ewr_code_timing", "env_obj")
  expect_equal(names(ewr2obj), ewrnames)

  # being a bit more general here, since the ordering could easily change
  expect_equal(sum(is.na(ewr2obj$ewr_code)), 0)
  expect_snapshot_value(as.list(unique(ewr2obj$ewr_code[!is.na(ewr2obj$ewr_code)])))
  expect_snapshot_value(as.list(unique(ewr2obj$env_obj)))


})
