# parameters that involve R objects/syntax can be set in R scripts, though that is less ideal

# Aggregation sequence
aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
               planning_units = planning_units,
               env_obj =  c('ewr_code', "env_obj"),
               sdl_units = sdl_units,
               Specific_goal = c('env_obj', "Specific_goal"),
               cewo_valleys = cewo_valleys,
               Objective = c('Specific_goal', 'Objective'),
               basin = basin,
               target_5_year_2024 = c('Objective', 'target_5_year_2024'))

# Functions for each aggregation
funseq <- list('CompensatingFactor',
               'ArithmeticMean',
               'ArithmeticMean',
               'SpatialWeightedMean',
               'ArithmeticMean',
               'SpatialWeightedMean',
               'ArithmeticMean',
               'SpatialWeightedMean',
               'ArithmeticMean')
