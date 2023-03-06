test_that("produces desired format for obj-target", {
  ewr2obj <- clean_ewr_obj(ewrpath = system.file('data-raw/causal_networks/ewr_cache/NSWEWR_LIVE.csv', package = 'werptoolkitr'),
                           objtablepath = system.file("data-raw/causal_networks/tbl10/tbl10a_WatRequirements5.4.csv", package = 'werptoolkitr'),
                           returnformat = 'long',
                           saveout = FALSE,
                           outdir = NULL,
                           savename = NULL)

  tarpath <- system.file('data-raw/causal_networks/unknown/Env_objectives_Target_species_nodes.csv', package = 'werptoolkitr')

  qcs <- c(system.file('data-raw/causal_networks/unknown/Target_species_per_PU_Murray.csv', package = 'werptoolkitr'),
           system.file('data-raw/causal_networks/unknown/PUs.csv', package = 'werptoolkitr'))

  obj2target <- clean_obj_target(ewrobjs = ewr2obj,
                                 targetpath = tarpath,
                                 qcfiles = qcs,
                                 saveout = FALSE,
                                 outdir = NULL,
                                 savename = NULL)

  expect_equal(names(obj2target), c('PlanningUnitID', 'LTWPShortName', 'env_obj',
                                    'Specific_goal', 'Objective', 'Target'))
  # test the values are there
  expect_snapshot_value(as.list(unique(obj2target$env_obj[!is.na(obj2target$env_obj)])))
  expect_snapshot_value(as.list(unique(obj2target$Specific_goal[!is.na(obj2target$Specific_goal)])))
  expect_snapshot_value(as.list(unique(obj2target$Objective[!is.na(obj2target$Objective)])))
  expect_snapshot_value(as.list(unique(obj2target$Target[!is.na(obj2target$Target)])))


})
