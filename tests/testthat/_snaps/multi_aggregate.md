# spatial input data works

    Code
      names(agged)
    Output
      [1] "scenario"                   "polyID"                    
      [3] "gauge"                      "planning_unit_name"        
      [5] "ewr_code"                   "ewr_code_mean_ewr_achieved"
      [7] "geometry"                  

# multi-step theme agg works, nongeom

    Code
      names(agged)
    Output
      [1] "scenario"                                                                                                                                               
      [2] "target_5_year_2024"                                                                                                                                     
      [3] "target_5_year_2024_ArithmeticMean_Objective_ArithmeticMean_Specific_goal_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_CompensatingFactor_ewr_achieved"

# multi-step theme agg works, auto-edges

    Code
      names(agged)
    Output
      [1] "scenario"                                                                                                                                               
      [2] "target_5_year_2024"                                                                                                                                     
      [3] "target_5_year_2024_ArithmeticMean_Objective_ArithmeticMean_Specific_goal_ArithmeticMean_env_obj_ArithmeticMean_ewr_code_CompensatingFactor_ewr_achieved"

