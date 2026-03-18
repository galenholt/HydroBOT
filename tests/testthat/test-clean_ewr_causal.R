test_that("2.4.1 cleans", {
  ewr_causal <- get_causal_ewr(struct = 'df')
  et <- get_ewr_table()

  expect_equal(sort(unique(ewr_causal$planning_unit_name)), sort(unique(et$PlanningUnitName)))
  expect_equal(sort(unique(ewr_causal$SWSDLName)), sort(unique(et$SWSDLName)))
  expect_equal(sort(unique(ewr_causal$gauge)), sort(unique(et$Gauge)))
  expect_equal(sort(unique(ewr_causal$ewr_code)), sort(unique(et$Code)))

  expect_false(any(duplicated(ewr_causal)))

  # One of Georgia's fixes makes the numeric objectives unique to swsdl and
  # Theme. I'm not quite sure why; any objective can map to different themes,
  # and they always mean different things across Swsdlnames.

  expect_s3_class(ewr_causal, 'tbl_df')

  ecl <- get_causal_ewr()
  expect_type(ecl, 'list')

})
