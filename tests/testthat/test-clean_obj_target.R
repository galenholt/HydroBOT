# skip_if_no_raw() because data-raw only exists in dev, not build, and these are
# really just consistency checks for the building of the causal networks, which
# happens during the dev process and isn't part of the package per se

ewrpath <- 'ewrtool' # system.file('data-raw/causal_networks/ewr_obj_codes_nsw/obj_codes_dec22.csv', package = 'HydroBOT')
tarpath <- system.file('data-raw/causal_networks/unknown/Env_objectives_Target_species_nodes.csv', package = 'HydroBOT')

qcs <- c(system.file('data-raw/causal_networks/unknown/Target_species_per_PU_Murray.csv', package = 'HydroBOT'),
         system.file('data-raw/causal_networks/unknown/PUs.csv', package = 'HydroBOT'))


test_that("produces desired format for obj-target", {
  skip_if_no_file(ewrpath)
  skip_if_no_file(tarpath)
  skip_if_no_file(qcs[1])
  skip_if_no_file(qcs[2])

  ewr2obj <- clean_ewr_obj(ewrobjpath = 'ewrtool',
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  obj2target <- clean_obj_target(ewrobjs = ewr2obj,
                                 targetpath = tarpath,
                                 qcfiles = qcs,
                                 saveout = FALSE,
                                 outdir = NULL,
                                 savename = NULL)

  expect_equal(names(obj2target), c('PlanningUnitID', "planning_unit_name", 'LTWPShortName', 'env_obj',
                                    'Specific_goal', 'Objective', 'Target'))
  # test the values are there
  expect_snapshot_value(as.list(unique(obj2target$env_obj[!is.na(obj2target$env_obj)])))
  expect_snapshot_value(as.list(unique(obj2target$Specific_goal[!is.na(obj2target$Specific_goal)])))
  expect_snapshot_value(as.list(unique(obj2target$Objective[!is.na(obj2target$Objective)])))
  expect_snapshot_value(as.list(unique(obj2target$Target[!is.na(obj2target$Target)])))


})

test_that("above the planning unit scale", {
  # THis really depends on clean_ewr_obj, but make sure it propagates
  skip_if_no_file(ewrpath)
  skip_if_no_file(tarpath)
  skip_if_no_file(qcs[1])
  skip_if_no_file(qcs[2])

  ewr2obj <- clean_ewr_obj(ewrobjpath = ewrpath,
                           gaugescale = FALSE,
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  obj2target <- clean_obj_target(ewrobjs = ewr2obj,
                                 targetpath = tarpath,
                                 qcfiles = qcs,
                                 saveout = FALSE,
                                 outdir = NULL,
                                 savename = NULL)

  expect_equal(names(obj2target), c('LTWPShortName', 'env_obj',
                                    'Specific_goal', 'Objective', 'Target'))
  # test the values are there
  expect_snapshot_value(as.list(unique(obj2target$env_obj[!is.na(obj2target$env_obj)])))
  expect_snapshot_value(as.list(unique(obj2target$Specific_goal[!is.na(obj2target$Specific_goal)])))
  expect_snapshot_value(as.list(unique(obj2target$Objective[!is.na(obj2target$Objective)])))
  expect_snapshot_value(as.list(unique(obj2target$Target[!is.na(obj2target$Target)])))


})
