# werp plots work

    Code
      basin_table
    Output
      # A tibble: 8 x 3
        step  `aggregation function` `aggregation level`
        <chr> <chr>                  <chr>              
      1 0     <NA>                   ewr_code_timing    
      2 1     CompensatingFactor     ewr_code           
      3 2     ArithmeticMean         env_obj            
      4 3     ArithmeticMean         sdl_units          
      5 4     ArithmeticMean         Specific_goal      
      6 5     wm                     catchment          
      7 6     ArithmeticMean         Objective          
      8 7     wm                     mdb                

---

    Code
      sdl_table
    Output
      # A tibble: 4 x 3
        step  `aggregation function` `aggregation level`
        <chr> <chr>                  <chr>              
      1 0     <NA>                   ewr_code_timing    
      2 1     CompensatingFactor     ewr_code           
      3 2     ArithmeticMean         env_obj            
      4 3     ArithmeticMean         sdl_units          

