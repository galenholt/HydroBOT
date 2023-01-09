# These are sort of silly tests to have in R, since they test the python functions. Move there once this beds down.

# some preamble settings of parameters that we expect to work
scenario_dir = system.file("extdata/testsmall/hydrographs", package = 'werptoolkitr')
outdir = 'tmp'
model_format = 'IQQM - NSW 10,000 years'

MINT = (100 - 0)/100
MAXT = (100 + 0 )/100
DUR = (100 - 0 )/100
DRAW = (100 -0 )/100

allowance = list('minThreshold' = MINT, 'maxThreshold' = MAXT, 'duration' = DUR, 'drawdown' = DRAW)

climate = 'Standard - 1911 to 2018 climate categorisation'

sceneinfodict = make_scenario_info(scenario_dir)

# On to tests
test_that("sceneinfodict returns expected structure", {
  expect_equal(class(sceneinfodict), 'list')
  expect_equal(length(sceneinfodict), 3)
})

# I don't really want to test make_output_dir

# And I don't want to test rebuilding data

# More R-specific setup
# The inner level turned into characters in R and needs to stay as a list. This is annoying but needed *only in R*
for (i in 1:length(sceneinfodict)) {
  for (j in 1:2) {
    sceneinfodict[[i]][[j]] <- as.list(sceneinfodict[[i]][[j]])
  }
}

# more list-making to work from R-Python
everyhydro = as.list(paths_gauges(sceneinfodict)[[1]])

test_that('returns one result', {
  ewr_out = run_save_ewrs(everyhydro,
                          outdir,
                          model_format,
                          allowance,
                          climate,
                          outputType = list('none'),
                          datesuffix = FALSE,
                          returnType = list('summary'))
  expect_equal(length(ewr_out), 1)
  expect_equal(names(ewr_out), 'summary')
})

# test multiple returns- use the next one once EWR is debugged.
test_that('returns list', {
  ewr_out = run_save_ewrs(everyhydro,
                          outdir,
                          model_format,
                          allowance,
                          climate,
                          outputType = list('none'),
                          datesuffix = FALSE,
                          returnType = list('summary', 'all'))
  expect_equal(length(ewr_out), 2)
  expect_true(all(c('summary', 'all') %in% names(ewr_out)))
})


# I know this one will fail currently- it's better than the previous, but there's a bug in EWR.
# Would be nice to have a version check, but that would require testing the version in python
# test_that('running and returning list of annual, all, and summary ewr outputs works', {
#   ewr_out = run_save_ewrs(everyhydro,
#                           outdir,
#                           model_format,
#                           allowance,
#                           climate,
#                           outputType = list('none'),
#                           datesuffix = FALSE,
#                           returnType = list('summary', 'annual', 'all'))
#   expect_equal(length(ewr_out), 3)
#   expect_true(all(c('summary', 'all', 'annual') %in% names(ewr_out)))
# })


