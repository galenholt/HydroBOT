# skip_if_no_raw() because data-raw only exists in dev, not build, and these are
# really just consistency checks for the building of the causal networks, which
# happens during the dev process and isn't part of the package per se

test_that("clean_long_term as expected", {
  yrpath <- system.file('data-raw/causal_networks/unknown/EObjYrTargets.csv',
                        package = 'werptoolkitr')

  skip_if_no_file(yrpath)

  yrtarget <- clean_long_term(yrpath,
                              saveout = FALSE,
                              outdir = NULL,
                              savename = NULL)

  expect_equal(names(yrtarget), c('env_obj', 'Target', 'Target_Category',
                                  'Objective',
                                  'target_5_year_2024',
                                  'target_10_year_2029',
                                  'target_20_year_2039'))
  # test the values are there
  expect_snapshot_value(as.list(unique(yrtarget$env_obj[!is.na(yrtarget$env_obj)])))
  expect_snapshot_value(as.list(unique(yrtarget$Target[!is.na(yrtarget$Target)])))
  expect_snapshot_value(as.list(unique(yrtarget$Target_Category[!is.na(yrtarget$Target_Category)])))
  expect_snapshot_value(as.list(unique(yrtarget$target_5_year_2024[!is.na(yrtarget$target_5_year_2024)])))
  expect_snapshot_value(as.list(unique(yrtarget$target_5_year_2024[!is.na(yrtarget$target_5_year_2024)])))
  expect_snapshot_value(as.list(unique(yrtarget$target_10_year_2029[!is.na(yrtarget$target_10_year_2029)])))
  expect_snapshot_value(as.list(unique(yrtarget$target_20_year_2039[!is.na(yrtarget$target_20_year_2039)])))


})
