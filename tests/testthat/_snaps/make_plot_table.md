# werp plots work

    Code
      basin_table
    Output
      # A tibble: 10 x 3
         step  `aggregation function` `aggregation level`
         <chr> <chr>                  <chr>              
       1 0     <NA>                   ewr_code_timing    
       2 1     ArithmeticMean         all_time           
       3 2     CompensatingFactor     ewr_code           
       4 3     ArithmeticMean         planning_units     
       5 4     ArithmeticMean         env_obj            
       6 5     SpatialWeightedMean    sdl_units          
       7 6     ArithmeticMean         Specific_goal      
       8 7     SpatialWeightedMean    catchment          
       9 8     ArithmeticMean         Objective          
      10 9     SpatialWeightedMean    mdb                

---

    Code
      sdl_table
    Output
      # A tibble: 6 x 3
        step  `aggregation function` `aggregation level`
        <chr> <chr>                  <chr>              
      1 0     <NA>                   ewr_code_timing    
      2 1     ArithmeticMean         all_time           
      3 2     CompensatingFactor     ewr_code           
      4 3     ArithmeticMean         planning_units     
      5 4     ArithmeticMean         env_obj            
      6 5     SpatialWeightedMean    sdl_units          

