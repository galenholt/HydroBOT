# parameters that involve R objects/syntax can be set in R scripts, though that is less ideal

# Aggregation sequence
aggseq <- list(
  ewr_code_main = c('ewr_code', 'ewr_code_main'),
  planning_units = planning_units,
  eco_objective = c('ewr_code_main', "eco_objective"),
  sdl_units = sdl_units,
  objective_text = c('eco_objective', "objective_text"),
  cewo_valleys = cewo_valleys,
  theme = c('objective_text', 'theme'),
  basin = basin
)

# Functions for each aggregation
funseq <- list(
  'CompensatingFactor',
  'ArithmeticMean',
  'ArithmeticMean',
  'SpatialWeightedMean',
  'ArithmeticMean',
  'SpatialWeightedMean',
  'ArithmeticMean',
  'SpatialWeightedMean'
)
