# werp plots work

    Code
      basin_table
    Output
      # A tibble: 9 x 3
        step  `aggregation function` `aggregation level`
        <chr> <chr>                  <chr>              
      1 0     <NA>                   ewr_code_timing    
      2 1     CompensatingFactor     ewr_code           
      3 2     ArithmeticMean         planning_units     
      4 3     ArithmeticMean         env_obj            
      5 4     SpatialWeightedMean    sdl_units          
      6 5     ArithmeticMean         Specific_goal      
      7 6     wm                     catchment          
      8 7     ArithmeticMean         Objective          
      9 8     wm                     mdb                

---

    Code
      sdl_table
    Output
      # A tibble: 5 x 3
        step  `aggregation function` `aggregation level`
        <chr> <chr>                  <chr>              
      1 0     <NA>                   ewr_code_timing    
      2 1     CompensatingFactor     ewr_code           
      3 2     ArithmeticMean         planning_units     
      4 3     ArithmeticMean         env_obj            
      5 4     SpatialWeightedMean    sdl_units          

