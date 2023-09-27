# build the aggregated data so we don't have to include it in the package
sumspat <- gauge2geo(summary_ewr_output,
                     gaugelocs = bom_basin_gauges)

aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
               env_obj =  c('ewr_code', "env_obj"),
               sdl_units = sdl_units,
               Specific_goal = c('env_obj', "Specific_goal"),
               catchment = cewo_valleys,
               Objective = c('Specific_goal', 'Objective'),
               mdb = basin,
               target_5_year_2024 = c('Objective', 'target_5_year_2024'))

funseq <- list('CompensatingFactor',
               'ArithmeticMean',
               'ArithmeticMean',
               c('ArithmeticMean', 'LimitingFactor'),
               list(wm = ~weighted.mean(., w = area,
                                        na.rm = TRUE)),
               'ArithmeticMean',
               list(wm = ~weighted.mean(., w = area,
                                        na.rm = TRUE)),
               'ArithmeticMean')

# Expect_warning because sf throws a warning about spatially constant attributes. and it gets thrown multiple times
agg_theme_space <- multi_aggregate(sumspat,
                                   aggsequence = aggseq,
                                   groupers = 'scenario',
                                   aggCols = 'ewr_achieved',
                                   funsequence = funseq,
                                   causal_edges = causal_ewr,
                                   saveintermediate = TRUE)


