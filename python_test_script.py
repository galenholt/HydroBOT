# from py_ewr import scenario_handling


# # # Testing the netcdf format:
# # # Input params
# # # scenarios =  'unit_testing_files/ex_tasker.nc'
# # # scenarios = '../AshResults/just_outputs/historical/Straight Node (Gauge).nc'
# # # scenarios = '../AshResults/just_outputs/stochastic/8580/Straight Node (Gauge).nc'
# # # scenarios = '../AshResults/just_outputs/dec23/Straight Node (Gauge).nc'
# scenarios = 'inst/extdata/ncdfexample/nchydros/werp_ncdf.nc'

# model_format = 'IQQM - netcdf'

# ewr_sh = scenario_handling.ScenarioHandler(scenarios, model_format)

# # this just checks it will process
# ewr_sh.process_scenarios()

# ewrout = ewr_sh.get_ewr_results()


# # # does passing straight into the zip work? NO, not surprisingly
# # # scenarios = '../AshResults/dec23_results.zip/dec23_results_4/historical/licvolfactor_1/0/Straight Node (Gauge).nc'
# # # The above works with the new format. the named dict in the readme for the new format reflects user-side setup; the new tool interface needs a single string filepath per run.

# # model_format = 'IQQM - netcdf'
# # allowance = {'minThreshold': 1.0, 'maxThreshold': 1.0, 'duration': 1.0, 'drawdown': 1.0}
# # climate = 'NSW 10,000 year climate sequence'

# # # Pass to the class

# # ewr_sh = scenario_handling.ScenarioHandler(scenarios, model_format)

# # # this just checks it will process
# # ewr_sh.process_scenarios()

# # ewrout = ewr_sh.get_ewr_results()


# # Testing the iqqm csv format:
# # Input params
# # This works
# scenarios =  'inst/extdata/testsmall/hydrographs/base/base.csv'
# model_format = 'IQQM - NSW 10,000 years'
# # allowance = {'minThreshold': 1.0, 'maxThreshold': 1.0, 'duration': 1.0, 'drawdown': 1.0}
# # climate = 'NSW 10,000 year climate sequence'

# # Pass to the class

# ewr_sh = scenario_handling.ScenarioHandler(scenarios, model_format)

# # ewr_sh.process_scenarios()

# # Testing the controller
# from inst.python import controller_functions
# ewr_sh = controller_functions.run_save_ewrs(scenarios, output_path = '', model_format = 'IQQM - NSW 10,000 years', outputType = ['none'], returnType = ['summary'])
# This bit kills the progress bars
# import os
# os.environ["TQDM_DISABLE"] = "1"
# zips
# scenarios = 'inst/extdata/ncdfexample/zipcdf.zip/zipcdf/S1/Straight Node (Gauge).nc'
# op = 'PYTEST'
# scenarios = 'C:/Users/galen/Documents/../Deakin University/QAEL - WERP in house - WERP/Toolkit/eslt-data-2/hydrographs/historical_scale_rain_0_8_evap_1_0/licvolfactor_0_5/0/Straight Node (Gauge).nc'
# op = 'C:/Users/galen/Documents/../Deakin University/QAEL - WERP in house - WERP/Toolkit/eslt-data-2/module_output/EWR'
# sn = 'historical_scale_rain_0_8_evap_1_0_licvolfactor_0_5_0'
# # model_format = 'IQQM - netcdf'
# # Testing the controller
# from inst.python import controller_functions
# ewr_sh = controller_functions.run_save_ewrs(scenarios, output_path = op, model_format = 'IQQM - netcdf', outputType = ['summary', 'yearly'], returnType = ['none'], scenario_name = sn)

# csvs
# Run make_temp_hydro() first
# scenarios = 'C:/Users/galen/Documents/code/WERP/HydroBOT/_test_data/hydrographs/base/412005.csv'
# op = 'C:/Users/galen/Documents/code/WERP/HydroBOT/_test_data/module_output/EWR'
# sn = 'base_412005'
# # model_format = 'IQQM - netcdf'
# # model_format = 'Standard time-series'
# # Testing the controller
# from inst.python import controller_functions
# ewr_sh = controller_functions.run_save_ewrs(scenarios, output_path = op, model_format = 'IQQM - NSW 10,000 years', outputType = ['summary', 'yearly'], returnType = ['summary', 'yearly', 'all_events'], scenario_name = sn)


# #  Run make_temp_multifile() first and stop prep_run_save_ewrs just before actually running them.
# scenarios = 'C:/Users/galen/Documents/code/WERP/HydroBOT/_test_data/hydrographs/base/412005.csv'
# scenarios2 = 'C:/Users/galen/Documents/code/WERP/HydroBOT/_test_data/hydrographs/base/412002.csv'

# op = 'C:/Users/galen/Documents/code/WERP/HydroBOT/_test_data/module_output/EWR'
# sn = 'base'
# # model_format = 'IQQM - netcdf'
# # model_format = 'Standard time-series'
# # Testing the controller
# from inst.python import controller_functions
# ewr_sh = controller_functions.run_save_ewrs(scenarios, output_path = op, model_format = 'Standard time-series', outputType = ['summary', 'yearly'], returnType = ['summary', 'yearly', 'all_events'], scenario_name = sn)
# ewr_sh2 = controller_functions.run_save_ewrs(scenarios2, output_path = op, model_format = 'Standard time-series', outputType = ['summary', 'yearly'], returnType = ['summary', 'yearly', 'all_events'], scenario_name = sn)

scenarios = 'C:/Users/galen/Documents/code/WERP/HydroBOT/_test_data/hydrographs/base/base.csv'

op = 'C:/Users/galen/Documents/code/WERP/HydroBOT/_test_data/module_output/EWR'
sn = 'base'
# model_format = 'IQQM - netcdf'
# model_format = 'Standard time-series'
# Testing the controller
from inst.python import controller_functions
ewr_sh = controller_functions.run_save_ewrs(scenarios, output_path = op, model_format = 'Standard time-series', outputType = ['summary', 'yearly'], returnType = ['summary', 'yearly', 'all_events'], scenario_name = sn)
