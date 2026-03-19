temp_parent_dir <- '_test_data'
# create dir so building makes sense
make_temp_hydro()


ewroutlist <- list('summary',
                   'yearly',
                   'all_events',
                   'all_successful_events',
                   'all_interEvents',
                   'all_successful_interEvents')

ewr_out <- prep_run_save_ewrs(hydro_dir = file.path(temp_parent_dir,
                                                    'hydrographs'),
                              output_parent_dir = temp_parent_dir,
                              outputType = ewroutlist,
                              returnType = ewroutlist)

# Need a vectorized mean of two cols.
# No clean NA handling here, but that's ok, a NA in one *should* be na in the output
pmean <- function(x,y) {
  (x + y)/2
}

pgeomean <- function(x,y) {
  exp((log(x) + log(y)) / 2)
}

# The path to those
ewrpath <- file.path(temp_parent_dir, 'module_output', 'EWR')

# Just have to check they run
test_that("binary", {
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)

  system.time({
    assessed <- control_achievement(datain, year_roll = 3,
                                    k_freq = Inf, k_inter = Inf, combo = pmean)
  })
  # 0.16

})

test_that("logistic", {
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)

  system.time({
    assessed <- control_achievement(datain, year_roll = 3,
                                    k_freq = 20, k_inter = 50, combo = 'pmean')
  })
  # 0.16

})

test_that("combos", {
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)

  system.time({
    assessedmin <- control_achievement(datain, year_roll = 3,
                                    k_freq = 20, k_inter = 50, combo = 'pmin')
  })

  system.time({
    assessedmax <- control_achievement(datain, year_roll = 3,
                                       k_freq = 20, k_inter = 50, combo = 'pmax')
  })

  system.time({
    assessedgm <- control_achievement(datain, year_roll = 3,
                                       k_freq = 20, k_inter = 50, combo = 'pgeomean')
  })
  # 0.16

  # need another that gets a 2d normal with peak at 1,1
})

test_that("linear", {
  datain <- read_and_geo(ewrpath, type = 'yearly', geopath = bom_basin_gauges)

  system.time({
    assessed1 <- control_achievement(datain, year_roll = 3,
                                    k_freq = -1, k_inter = -1, combo = pmean)
  })

  system.time({
    assessed2 <- control_achievement(datain, year_roll = 3,
                                     k_freq = -2, k_inter = -2, combo = pmean)
  })
  # 0.16

  max(assessed1$frequency_achieved, na.rm = T)
  max(assessed2$frequency_achieved, na.rm = T)

  max(assessed1$interevent_achieved, na.rm = T)
  max(assessed2$interevent_achieved, na.rm = T)

  max(assessed1$ewr_achieved, na.rm = T)
  max(assessed2$ewr_achieved, na.rm = T)
})

test_that('vectors', {
  novec <- adjusted_logistic(x = 15, k = 20, threshold = 10, thresh_is = 'max')

  vecx <- adjusted_logistic(x = 0:20, k = 20, threshold = 10, thresh_is = 'max')

  vecthresh <- adjusted_logistic(x = 0:20, k = 20, threshold = rep(10, 21), thresh_is = 'max')


})
