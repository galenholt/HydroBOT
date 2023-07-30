# skip_if_no_raw() because data-raw only exists in dev, not build, and these are
# really just consistency checks for the building of the causal networks, which
# happens during the dev process and isn't part of the package per se

ewrpath <- system.file('data-raw/causal_networks/ewr_cache/NSWEWR_LIVE.csv', package = 'werptoolkitr')
objpath <- system.file("data-raw/causal_networks/tbl10/tbl10a_WatRequirements5.4.csv", package = 'werptoolkitr')

test_that("long produces expected output", {

  skip_if_no_file(ewrpath)
  skip_if_no_file(objpath)

  ewr2obj <- clean_ewr_obj(ewrpath = ewrpath,
                           objtablepath = objpath,
                           returnformat = 'long',
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  expect_s3_class(ewr2obj, 'tbl_df')
  # paste(names(ewr2obj), collapse = '", "')
  ewrnames <- c("WatReqID", "PlanningUnitID", "LTWPShortName", "gauge", "ewr_code", "ewr_code_timing", "env_obj")
  expect_equal(names(ewr2obj), ewrnames)

  # being a bit more general here, since the ordering could easily change
  expect_equal(sum(is.na(ewr2obj$ewr_code)), 1777)
  expect_snapshot_value(as.list(unique(ewr2obj$ewr_code[!is.na(ewr2obj$ewr_code)])))
  expect_snapshot_value(as.list(unique(ewr2obj$env_obj)))


})

test_that("long produces expected output", {

  skip_if_no_file(ewrpath)
  skip_if_no_file(objpath)

  ewr2objw <- clean_ewr_obj(ewrpath = ewrpath,
                           objtablepath = objpath,
                           returnformat = 'wide',
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  expect_s3_class(ewr2objw, 'tbl_df')

  # being a bit more general here, since the ordering could easily change
  # basically, this should have a bunch of logical columns
  expect_snapshot_value(purrr::map(ewr2objw, is.logical))

})

