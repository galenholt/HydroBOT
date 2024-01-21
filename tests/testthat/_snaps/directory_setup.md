# scenario paths works for single csvs in each

    Code
      relative_part
    Output
      [1] "extdata/testsmall/hydrographs/base/base.csv"  
      [2] "extdata/testsmall/hydrographs/down4/down4.csv"
      [3] "extdata/testsmall/hydrographs/up4/up4.csv"    

# scenario paths and the name fixer works for multiple csvs in each

    Code
      scenario_paths
    Output
      $base_412002
      [1] "_test_data/hydrographs/base/412002.csv"
      
      $base_412005
      [1] "_test_data/hydrographs/base/412005.csv"
      
      $base_412038
      [1] "_test_data/hydrographs/base/412038.csv"
      
      $base_421001
      [1] "_test_data/hydrographs/base/421001.csv"
      
      $base_421004
      [1] "_test_data/hydrographs/base/421004.csv"
      
      $base_421011
      [1] "_test_data/hydrographs/base/421011.csv"
      
      $down4_412002
      [1] "_test_data/hydrographs/down4/412002.csv"
      
      $down4_412005
      [1] "_test_data/hydrographs/down4/412005.csv"
      
      $down4_412038
      [1] "_test_data/hydrographs/down4/412038.csv"
      
      $down4_421001
      [1] "_test_data/hydrographs/down4/421001.csv"
      
      $down4_421004
      [1] "_test_data/hydrographs/down4/421004.csv"
      
      $down4_421011
      [1] "_test_data/hydrographs/down4/421011.csv"
      
      $up4_412002
      [1] "_test_data/hydrographs/up4/412002.csv"
      
      $up4_412005
      [1] "_test_data/hydrographs/up4/412005.csv"
      
      $up4_412038
      [1] "_test_data/hydrographs/up4/412038.csv"
      
      $up4_421001
      [1] "_test_data/hydrographs/up4/421001.csv"
      
      $up4_421004
      [1] "_test_data/hydrographs/up4/421004.csv"
      
      $up4_421011
      [1] "_test_data/hydrographs/up4/421011.csv"
      

---

    Code
      scenario_paths
    Output
      $base_base_DIRECTORYAPPEND_412002
      [1] "_test_data/hydrographs/base/base_DIRECTORYAPPEND_412002.csv"
      
      $base_base_DIRECTORYAPPEND_412005
      [1] "_test_data/hydrographs/base/base_DIRECTORYAPPEND_412005.csv"
      
      $base_base_DIRECTORYAPPEND_412038
      [1] "_test_data/hydrographs/base/base_DIRECTORYAPPEND_412038.csv"
      
      $base_base_DIRECTORYAPPEND_421001
      [1] "_test_data/hydrographs/base/base_DIRECTORYAPPEND_421001.csv"
      
      $base_base_DIRECTORYAPPEND_421004
      [1] "_test_data/hydrographs/base/base_DIRECTORYAPPEND_421004.csv"
      
      $base_base_DIRECTORYAPPEND_421011
      [1] "_test_data/hydrographs/base/base_DIRECTORYAPPEND_421011.csv"
      
      $down4_down4_DIRECTORYAPPEND_412002
      [1] "_test_data/hydrographs/down4/down4_DIRECTORYAPPEND_412002.csv"
      
      $down4_down4_DIRECTORYAPPEND_412005
      [1] "_test_data/hydrographs/down4/down4_DIRECTORYAPPEND_412005.csv"
      
      $down4_down4_DIRECTORYAPPEND_412038
      [1] "_test_data/hydrographs/down4/down4_DIRECTORYAPPEND_412038.csv"
      
      $down4_down4_DIRECTORYAPPEND_421001
      [1] "_test_data/hydrographs/down4/down4_DIRECTORYAPPEND_421001.csv"
      
      $down4_down4_DIRECTORYAPPEND_421004
      [1] "_test_data/hydrographs/down4/down4_DIRECTORYAPPEND_421004.csv"
      
      $down4_down4_DIRECTORYAPPEND_421011
      [1] "_test_data/hydrographs/down4/down4_DIRECTORYAPPEND_421011.csv"
      
      $up4_up4_DIRECTORYAPPEND_412002
      [1] "_test_data/hydrographs/up4/up4_DIRECTORYAPPEND_412002.csv"
      
      $up4_up4_DIRECTORYAPPEND_412005
      [1] "_test_data/hydrographs/up4/up4_DIRECTORYAPPEND_412005.csv"
      
      $up4_up4_DIRECTORYAPPEND_412038
      [1] "_test_data/hydrographs/up4/up4_DIRECTORYAPPEND_412038.csv"
      
      $up4_up4_DIRECTORYAPPEND_421001
      [1] "_test_data/hydrographs/up4/up4_DIRECTORYAPPEND_421001.csv"
      
      $up4_up4_DIRECTORYAPPEND_421004
      [1] "_test_data/hydrographs/up4/up4_DIRECTORYAPPEND_421004.csv"
      
      $up4_up4_DIRECTORYAPPEND_421011
      [1] "_test_data/hydrographs/up4/up4_DIRECTORYAPPEND_421011.csv"
      

# creating output dirs works with hydro_dir having all scenarios

    Code
      realised_structure
    Output
       [1] "hydrographs"                        "hydrographs/base"                  
       [3] "hydrographs/base/base.csv"          "hydrographs/base/base.json"        
       [5] "hydrographs/down4"                  "hydrographs/down4/down4.csv"       
       [7] "hydrographs/down4/down4.json"       "hydrographs/scenario_metadata.json"
       [9] "hydrographs/scenario_metadata.yml"  "hydrographs/up4"                   
      [11] "hydrographs/up4/up4.csv"            "hydrographs/up4/up4.json"          
      [13] "module_output"                      "module_output/EWR"                 
      [15] "module_output/EWR/base"             "module_output/EWR/down4"           
      [17] "module_output/EWR/up4"             

# creating output dirs works with hydro_dir as a single scenario

    Code
      realised_structure
    Output
       [1] "hydrographs"                            
       [2] "hydrographs/base"                       
       [3] "hydrographs/base/base.csv"              
       [4] "hydrographs/base/base.json"             
       [5] "hydrographs/base/module_output"         
       [6] "hydrographs/base/module_output/EWR"     
       [7] "hydrographs/base/module_output/EWR/base"
       [8] "hydrographs/down4"                      
       [9] "hydrographs/down4/down4.csv"            
      [10] "hydrographs/down4/down4.json"           
      [11] "hydrographs/scenario_metadata.json"     
      [12] "hydrographs/scenario_metadata.yml"      
      [13] "hydrographs/up4"                        
      [14] "hydrographs/up4/up4.csv"                
      [15] "hydrographs/up4/up4.json"               

