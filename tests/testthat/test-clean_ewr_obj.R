# skip_if_no_raw() because data-raw only exists in dev, not build, and these are
# really just consistency checks for the building of the causal networks, which
# happens during the dev process and isn't part of the package per se

# This is now where we get it with 'ewrtool'
ewrpath <- '.venv/Lib/site-packages/py_ewr/parameter_metadata/parameter_sheet.csv' # system.file('data-raw/causal_networks/ewr_obj_codes_nsw/obj_codes_dec22.csv', package = 'HydroBOT')

test_that("no gaugescale produces expected output", {

    skip_if_no_file(ewrpath)


  ewr2obj <- clean_ewr_obj(ewrobjpath = 'ewrtool',
                           gaugescale = FALSE,
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  expect_s3_class(ewr2obj, 'tbl_df')
  expect_snapshot_value(names(ewr2obj), style = 'deparse')

  # being a bit more general here, since the ordering could easily change
  expect_equal(sum(is.na(ewr2obj$ewr_code)), 0)
  expect_snapshot_value(as.list(unique(ewr2obj$ewr_code[!is.na(ewr2obj$ewr_code)])))
  expect_snapshot_value(as.list(unique(ewr2obj$env_obj)))


})

test_that("gaugescale produces expected output", {

    skip_if_no_file(ewrpath)

  ewr2obj <- clean_ewr_obj(ewrobjpath = 'ewrtool',
                           gaugescale = TRUE,
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  expect_s3_class(ewr2obj, 'tbl_df')
  expect_snapshot_value(names(ewr2obj), style = 'deparse')


  # being a bit more general here, since the ordering could easily change
  expect_equal(sum(is.na(ewr2obj$ewr_code)), 0)
  expect_snapshot_value(as.list(unique(ewr2obj$ewr_code[!is.na(ewr2obj$ewr_code)])))
  expect_snapshot_value(as.list(unique(ewr2obj$env_obj)))


})
