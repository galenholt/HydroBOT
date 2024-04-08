# complex dir structure

    Code
      realised_structure
    Output
       [1] "hydrographs"                                  
       [2] "hydrographs/S1"                               
       [3] "hydrographs/S1/base"                          
       [4] "hydrographs/S1/base/base.csv"                 
       [5] "hydrographs/S1/base/base.json"                
       [6] "hydrographs/S2"                               
       [7] "hydrographs/S2/up4"                           
       [8] "hydrographs/S2/up4/up4.csv"                   
       [9] "hydrographs/S2/up4/up4.json"                  
      [10] "hydrographs/base"                             
      [11] "hydrographs/base/base.csv"                    
      [12] "hydrographs/base/base.json"                   
      [13] "hydrographs/down4"                            
      [14] "hydrographs/down4/down4.csv"                  
      [15] "hydrographs/down4/down4.json"                 
      [16] "hydrographs/scenario_metadata.json"           
      [17] "hydrographs/scenario_metadata.yml"            
      [18] "hydrographs/up4"                              
      [19] "hydrographs/up4/up4.csv"                      
      [20] "hydrographs/up4/up4.json"                     
      [21] "module_output"                                
      [22] "module_output/EWR"                            
      [23] "module_output/EWR/S1_base_base"               
      [24] "module_output/EWR/S1_base_base/all_events.csv"
      [25] "module_output/EWR/S1_base_base/summary.csv"   
      [26] "module_output/EWR/S2_up4_up4"                 
      [27] "module_output/EWR/S2_up4_up4/all_events.csv"  
      [28] "module_output/EWR/S2_up4_up4/summary.csv"     
      [29] "module_output/EWR/base_base"                  
      [30] "module_output/EWR/base_base/all_events.csv"   
      [31] "module_output/EWR/base_base/summary.csv"      
      [32] "module_output/EWR/down4_down4"                
      [33] "module_output/EWR/down4_down4/all_events.csv" 
      [34] "module_output/EWR/down4_down4/summary.csv"    
      [35] "module_output/EWR/ewr_metadata.json"          
      [36] "module_output/EWR/ewr_metadata.yml"           
      [37] "module_output/EWR/up4_up4"                    
      [38] "module_output/EWR/up4_up4/all_events.csv"     
      [39] "module_output/EWR/up4_up4/summary.csv"        

# manual scenario naming

    Code
      realised_structure
    Output
       [1] "hydrographs"                         "hydrographs/base"                   
       [3] "hydrographs/base/base.csv"           "hydrographs/base/base.json"         
       [5] "hydrographs/down4"                   "hydrographs/down4/down4.csv"        
       [7] "hydrographs/down4/down4.json"        "hydrographs/scenario_metadata.json" 
       [9] "hydrographs/scenario_metadata.yml"   "hydrographs/up4"                    
      [11] "hydrographs/up4/up4.csv"             "hydrographs/up4/up4.json"           
      [13] "module_output"                       "module_output/EWR"                  
      [15] "module_output/EWR/S1"                "module_output/EWR/S1/all_events.csv"
      [17] "module_output/EWR/S1/summary.csv"    "module_output/EWR/S2"               
      [19] "module_output/EWR/S2/all_events.csv" "module_output/EWR/S2/summary.csv"   
      [21] "module_output/EWR/S3"                "module_output/EWR/S3/all_events.csv"
      [23] "module_output/EWR/S3/summary.csv"    "module_output/EWR/ewr_metadata.json"
      [25] "module_output/EWR/ewr_metadata.yml" 

# csv per gauge works

    Code
      realised_structure
    Output
       [1] "hydrographs"                               
       [2] "hydrographs/base"                          
       [3] "hydrographs/base/412002.csv"               
       [4] "hydrographs/base/412005.csv"               
       [5] "hydrographs/base/412038.csv"               
       [6] "hydrographs/base/421001.csv"               
       [7] "hydrographs/base/421004.csv"               
       [8] "hydrographs/base/421011.csv"               
       [9] "hydrographs/down4"                         
      [10] "hydrographs/down4/412002.csv"              
      [11] "hydrographs/down4/412005.csv"              
      [12] "hydrographs/down4/412038.csv"              
      [13] "hydrographs/down4/421001.csv"              
      [14] "hydrographs/down4/421004.csv"              
      [15] "hydrographs/down4/421011.csv"              
      [16] "hydrographs/up4"                           
      [17] "hydrographs/up4/412002.csv"                
      [18] "hydrographs/up4/412005.csv"                
      [19] "hydrographs/up4/412038.csv"                
      [20] "hydrographs/up4/421001.csv"                
      [21] "hydrographs/up4/421004.csv"                
      [22] "hydrographs/up4/421011.csv"                
      [23] "module_output"                             
      [24] "module_output/EWR"                         
      [25] "module_output/EWR/base_412002"             
      [26] "module_output/EWR/base_412002/summary.csv" 
      [27] "module_output/EWR/base_412002/yearly.csv"  
      [28] "module_output/EWR/base_412005"             
      [29] "module_output/EWR/base_412005/summary.csv" 
      [30] "module_output/EWR/base_412005/yearly.csv"  
      [31] "module_output/EWR/base_412038"             
      [32] "module_output/EWR/base_412038/summary.csv" 
      [33] "module_output/EWR/base_412038/yearly.csv"  
      [34] "module_output/EWR/base_421001"             
      [35] "module_output/EWR/base_421001/summary.csv" 
      [36] "module_output/EWR/base_421001/yearly.csv"  
      [37] "module_output/EWR/base_421004"             
      [38] "module_output/EWR/base_421004/summary.csv" 
      [39] "module_output/EWR/base_421004/yearly.csv"  
      [40] "module_output/EWR/base_421011"             
      [41] "module_output/EWR/base_421011/summary.csv" 
      [42] "module_output/EWR/base_421011/yearly.csv"  
      [43] "module_output/EWR/down4_412002"            
      [44] "module_output/EWR/down4_412002/summary.csv"
      [45] "module_output/EWR/down4_412002/yearly.csv" 
      [46] "module_output/EWR/down4_412005"            
      [47] "module_output/EWR/down4_412005/summary.csv"
      [48] "module_output/EWR/down4_412005/yearly.csv" 
      [49] "module_output/EWR/down4_412038"            
      [50] "module_output/EWR/down4_412038/summary.csv"
      [51] "module_output/EWR/down4_412038/yearly.csv" 
      [52] "module_output/EWR/down4_421001"            
      [53] "module_output/EWR/down4_421001/summary.csv"
      [54] "module_output/EWR/down4_421001/yearly.csv" 
      [55] "module_output/EWR/down4_421004"            
      [56] "module_output/EWR/down4_421004/summary.csv"
      [57] "module_output/EWR/down4_421004/yearly.csv" 
      [58] "module_output/EWR/down4_421011"            
      [59] "module_output/EWR/down4_421011/summary.csv"
      [60] "module_output/EWR/down4_421011/yearly.csv" 
      [61] "module_output/EWR/ewr_metadata.json"       
      [62] "module_output/EWR/ewr_metadata.yml"        
      [63] "module_output/EWR/up4_412002"              
      [64] "module_output/EWR/up4_412002/summary.csv"  
      [65] "module_output/EWR/up4_412002/yearly.csv"   
      [66] "module_output/EWR/up4_412005"              
      [67] "module_output/EWR/up4_412005/summary.csv"  
      [68] "module_output/EWR/up4_412005/yearly.csv"   
      [69] "module_output/EWR/up4_412038"              
      [70] "module_output/EWR/up4_412038/summary.csv"  
      [71] "module_output/EWR/up4_412038/yearly.csv"   
      [72] "module_output/EWR/up4_421001"              
      [73] "module_output/EWR/up4_421001/summary.csv"  
      [74] "module_output/EWR/up4_421001/yearly.csv"   
      [75] "module_output/EWR/up4_421004"              
      [76] "module_output/EWR/up4_421004/summary.csv"  
      [77] "module_output/EWR/up4_421004/yearly.csv"   
      [78] "module_output/EWR/up4_421011"              
      [79] "module_output/EWR/up4_421011/summary.csv"  
      [80] "module_output/EWR/up4_421011/yearly.csv"   

# csv per gauge works for filenames

    Code
      unique(ewr_out$summary$scenario)
    Output
       [1] "base_412002"  "base_412005"  "base_412038"  "base_421001"  "base_421004" 
       [6] "base_421011"  "down4_412002" "down4_412005" "down4_412038" "down4_421001"
      [11] "down4_421004" "down4_421011" "up4_412002"   "up4_412005"   "up4_412038"  
      [16] "up4_421001"   "up4_421004"   "up4_421011"  

---

    Code
      realised_structure
    Output
       [1] "hydrographs"                               
       [2] "hydrographs/base"                          
       [3] "hydrographs/base/412002.csv"               
       [4] "hydrographs/base/412005.csv"               
       [5] "hydrographs/base/412038.csv"               
       [6] "hydrographs/base/421001.csv"               
       [7] "hydrographs/base/421004.csv"               
       [8] "hydrographs/base/421011.csv"               
       [9] "hydrographs/down4"                         
      [10] "hydrographs/down4/412002.csv"              
      [11] "hydrographs/down4/412005.csv"              
      [12] "hydrographs/down4/412038.csv"              
      [13] "hydrographs/down4/421001.csv"              
      [14] "hydrographs/down4/421004.csv"              
      [15] "hydrographs/down4/421011.csv"              
      [16] "hydrographs/up4"                           
      [17] "hydrographs/up4/412002.csv"                
      [18] "hydrographs/up4/412005.csv"                
      [19] "hydrographs/up4/412038.csv"                
      [20] "hydrographs/up4/421001.csv"                
      [21] "hydrographs/up4/421004.csv"                
      [22] "hydrographs/up4/421011.csv"                
      [23] "module_output"                             
      [24] "module_output/EWR"                         
      [25] "module_output/EWR/base_412002"             
      [26] "module_output/EWR/base_412002/summary.csv" 
      [27] "module_output/EWR/base_412002/yearly.csv"  
      [28] "module_output/EWR/base_412005"             
      [29] "module_output/EWR/base_412005/summary.csv" 
      [30] "module_output/EWR/base_412005/yearly.csv"  
      [31] "module_output/EWR/base_412038"             
      [32] "module_output/EWR/base_412038/summary.csv" 
      [33] "module_output/EWR/base_412038/yearly.csv"  
      [34] "module_output/EWR/base_421001"             
      [35] "module_output/EWR/base_421001/summary.csv" 
      [36] "module_output/EWR/base_421001/yearly.csv"  
      [37] "module_output/EWR/base_421004"             
      [38] "module_output/EWR/base_421004/summary.csv" 
      [39] "module_output/EWR/base_421004/yearly.csv"  
      [40] "module_output/EWR/base_421011"             
      [41] "module_output/EWR/base_421011/summary.csv" 
      [42] "module_output/EWR/base_421011/yearly.csv"  
      [43] "module_output/EWR/down4_412002"            
      [44] "module_output/EWR/down4_412002/summary.csv"
      [45] "module_output/EWR/down4_412002/yearly.csv" 
      [46] "module_output/EWR/down4_412005"            
      [47] "module_output/EWR/down4_412005/summary.csv"
      [48] "module_output/EWR/down4_412005/yearly.csv" 
      [49] "module_output/EWR/down4_412038"            
      [50] "module_output/EWR/down4_412038/summary.csv"
      [51] "module_output/EWR/down4_412038/yearly.csv" 
      [52] "module_output/EWR/down4_421001"            
      [53] "module_output/EWR/down4_421001/summary.csv"
      [54] "module_output/EWR/down4_421001/yearly.csv" 
      [55] "module_output/EWR/down4_421004"            
      [56] "module_output/EWR/down4_421004/summary.csv"
      [57] "module_output/EWR/down4_421004/yearly.csv" 
      [58] "module_output/EWR/down4_421011"            
      [59] "module_output/EWR/down4_421011/summary.csv"
      [60] "module_output/EWR/down4_421011/yearly.csv" 
      [61] "module_output/EWR/ewr_metadata.json"       
      [62] "module_output/EWR/ewr_metadata.yml"        
      [63] "module_output/EWR/up4_412002"              
      [64] "module_output/EWR/up4_412002/summary.csv"  
      [65] "module_output/EWR/up4_412002/yearly.csv"   
      [66] "module_output/EWR/up4_412005"              
      [67] "module_output/EWR/up4_412005/summary.csv"  
      [68] "module_output/EWR/up4_412005/yearly.csv"   
      [69] "module_output/EWR/up4_412038"              
      [70] "module_output/EWR/up4_412038/summary.csv"  
      [71] "module_output/EWR/up4_412038/yearly.csv"   
      [72] "module_output/EWR/up4_421001"              
      [73] "module_output/EWR/up4_421001/summary.csv"  
      [74] "module_output/EWR/up4_421001/yearly.csv"   
      [75] "module_output/EWR/up4_421004"              
      [76] "module_output/EWR/up4_421004/summary.csv"  
      [77] "module_output/EWR/up4_421004/yearly.csv"   
      [78] "module_output/EWR/up4_421011"              
      [79] "module_output/EWR/up4_421011/summary.csv"  
      [80] "module_output/EWR/up4_421011/yearly.csv"   

# saving works for one

    Code
      realised_structure
    Output
       [1] "hydrographs"                         "hydrographs/base"                   
       [3] "hydrographs/base/base.csv"           "hydrographs/base/base.json"         
       [5] "hydrographs/down4"                   "hydrographs/down4/down4.csv"        
       [7] "hydrographs/down4/down4.json"        "hydrographs/scenario_metadata.json" 
       [9] "hydrographs/scenario_metadata.yml"   "hydrographs/up4"                    
      [11] "hydrographs/up4/up4.csv"             "hydrographs/up4/up4.json"           
      [13] "module_output"                       "module_output/EWR"                  
      [15] "module_output/EWR/base"              "module_output/EWR/base/summary.csv" 
      [17] "module_output/EWR/down4"             "module_output/EWR/down4/summary.csv"
      [19] "module_output/EWR/ewr_metadata.json" "module_output/EWR/ewr_metadata.yml" 
      [21] "module_output/EWR/up4"               "module_output/EWR/up4/summary.csv"  

# saving works with subdir

    Code
      realised_structure
    Output
       [1] "hydrographs"                                
       [2] "hydrographs/base"                           
       [3] "hydrographs/base/base.csv"                  
       [4] "hydrographs/base/base.json"                 
       [5] "hydrographs/down4"                          
       [6] "hydrographs/down4/down4.csv"                
       [7] "hydrographs/down4/down4.json"               
       [8] "hydrographs/scenario_metadata.json"         
       [9] "hydrographs/scenario_metadata.yml"          
      [10] "hydrographs/up4"                            
      [11] "hydrographs/up4/up4.csv"                    
      [12] "hydrographs/up4/up4.json"                   
      [13] "module_output"                              
      [14] "module_output/EWR"                          
      [15] "module_output/EWR/testsub"                  
      [16] "module_output/EWR/testsub/base"             
      [17] "module_output/EWR/testsub/base/summary.csv" 
      [18] "module_output/EWR/testsub/down4"            
      [19] "module_output/EWR/testsub/down4/summary.csv"
      [20] "module_output/EWR/testsub/ewr_metadata.json"
      [21] "module_output/EWR/testsub/ewr_metadata.yml" 
      [22] "module_output/EWR/testsub/up4"              
      [23] "module_output/EWR/testsub/up4/summary.csv"  

# saving and returning works for all (or nearly all) ewr outputs

    Code
      realised_structure
    Output
       [1] "hydrographs"                                           
       [2] "hydrographs/base"                                      
       [3] "hydrographs/base/base.csv"                             
       [4] "hydrographs/base/base.json"                            
       [5] "hydrographs/down4"                                     
       [6] "hydrographs/down4/down4.csv"                           
       [7] "hydrographs/down4/down4.json"                          
       [8] "hydrographs/scenario_metadata.json"                    
       [9] "hydrographs/scenario_metadata.yml"                     
      [10] "hydrographs/up4"                                       
      [11] "hydrographs/up4/up4.csv"                               
      [12] "hydrographs/up4/up4.json"                              
      [13] "module_output"                                         
      [14] "module_output/EWR"                                     
      [15] "module_output/EWR/base"                                
      [16] "module_output/EWR/base/all_events.csv"                 
      [17] "module_output/EWR/base/all_interEvents.csv"            
      [18] "module_output/EWR/base/all_successful_events.csv"      
      [19] "module_output/EWR/base/all_successful_interEvents.csv" 
      [20] "module_output/EWR/base/summary.csv"                    
      [21] "module_output/EWR/base/yearly.csv"                     
      [22] "module_output/EWR/down4"                               
      [23] "module_output/EWR/down4/all_events.csv"                
      [24] "module_output/EWR/down4/all_interEvents.csv"           
      [25] "module_output/EWR/down4/all_successful_events.csv"     
      [26] "module_output/EWR/down4/all_successful_interEvents.csv"
      [27] "module_output/EWR/down4/summary.csv"                   
      [28] "module_output/EWR/down4/yearly.csv"                    
      [29] "module_output/EWR/ewr_metadata.json"                   
      [30] "module_output/EWR/ewr_metadata.yml"                    
      [31] "module_output/EWR/up4"                                 
      [32] "module_output/EWR/up4/all_events.csv"                  
      [33] "module_output/EWR/up4/all_interEvents.csv"             
      [34] "module_output/EWR/up4/all_successful_events.csv"       
      [35] "module_output/EWR/up4/all_successful_interEvents.csv"  
      [36] "module_output/EWR/up4/summary.csv"                     
      [37] "module_output/EWR/up4/yearly.csv"                      

# NETCDF saving and returning works for all (or nearly all) ewr outputs

    Code
      realised_structure
    Output
       [1] "module_output"                                      
       [2] "module_output/EWR"                                  
       [3] "module_output/EWR/S1"                               
       [4] "module_output/EWR/S1/all_events.csv"                
       [5] "module_output/EWR/S1/all_interEvents.csv"           
       [6] "module_output/EWR/S1/all_successful_events.csv"     
       [7] "module_output/EWR/S1/all_successful_interEvents.csv"
       [8] "module_output/EWR/S1/summary.csv"                   
       [9] "module_output/EWR/S1/yearly.csv"                    
      [10] "module_output/EWR/S2"                               
      [11] "module_output/EWR/S2/all_events.csv"                
      [12] "module_output/EWR/S2/all_interEvents.csv"           
      [13] "module_output/EWR/S2/all_successful_events.csv"     
      [14] "module_output/EWR/S2/all_successful_interEvents.csv"
      [15] "module_output/EWR/S2/summary.csv"                   
      [16] "module_output/EWR/S2/yearly.csv"                    
      [17] "module_output/EWR/ewr_metadata.json"                
      [18] "module_output/EWR/ewr_metadata.yml"                 
      [19] "nchydros"                                           
      [20] "nchydros/S1"                                        
      [21] "nchydros/S1/StraightNodeGauge.nc"                   
      [22] "nchydros/S2"                                        
      [23] "nchydros/S2/StraightNodeGauge.nc"                   

# zipped NETCDF saving and returning works for all (or nearly all) ewr outputs

    Code
      realised_structure
    Output
       [1] "hydrographs"                                    
       [2] "hydrographs/zipcdf.zip"                         
       [3] "module_output"                                  
       [4] "module_output/EWR"                              
       [5] "module_output/EWR/ewr_metadata.json"            
       [6] "module_output/EWR/ewr_metadata.yml"             
       [7] "module_output/EWR/zipcdf_S1"                    
       [8] "module_output/EWR/zipcdf_S1/all_events.csv"     
       [9] "module_output/EWR/zipcdf_S1/all_interEvents.csv"
      [10] "module_output/EWR/zipcdf_S1/summary.csv"        
      [11] "module_output/EWR/zipcdf_S1/yearly.csv"         
      [12] "module_output/EWR/zipcdf_S2"                    
      [13] "module_output/EWR/zipcdf_S2/all_events.csv"     
      [14] "module_output/EWR/zipcdf_S2/all_interEvents.csv"
      [15] "module_output/EWR/zipcdf_S2/summary.csv"        
      [16] "module_output/EWR/zipcdf_S2/yearly.csv"         

# NETCDF saving and returning works in parallel

    Code
      realised_structure
    Output
       [1] "module_output"                       "module_output/EWR"                  
       [3] "module_output/EWR/S1"                "module_output/EWR/S1/summary.csv"   
       [5] "module_output/EWR/S2"                "module_output/EWR/S2/summary.csv"   
       [7] "module_output/EWR/ewr_metadata.json" "module_output/EWR/ewr_metadata.yml" 
       [9] "nchydros"                            "nchydros/S1"                        
      [11] "nchydros/S1/StraightNodeGauge.nc"    "nchydros/S2"                        
      [13] "nchydros/S2/StraightNodeGauge.nc"   

# specifying *Type as character instead of list

    Code
      realised_structure
    Output
       [1] "hydrographs"                           
       [2] "hydrographs/base"                      
       [3] "hydrographs/base/base.csv"             
       [4] "hydrographs/base/base.json"            
       [5] "hydrographs/down4"                     
       [6] "hydrographs/down4/down4.csv"           
       [7] "hydrographs/down4/down4.json"          
       [8] "hydrographs/scenario_metadata.json"    
       [9] "hydrographs/scenario_metadata.yml"     
      [10] "hydrographs/up4"                       
      [11] "hydrographs/up4/up4.csv"               
      [12] "hydrographs/up4/up4.json"              
      [13] "module_output"                         
      [14] "module_output/EWR"                     
      [15] "module_output/EWR/base"                
      [16] "module_output/EWR/base/all_events.csv" 
      [17] "module_output/EWR/base/summary.csv"    
      [18] "module_output/EWR/down4"               
      [19] "module_output/EWR/down4/all_events.csv"
      [20] "module_output/EWR/down4/summary.csv"   
      [21] "module_output/EWR/ewr_metadata.json"   
      [22] "module_output/EWR/ewr_metadata.yml"    
      [23] "module_output/EWR/up4"                 
      [24] "module_output/EWR/up4/all_events.csv"  
      [25] "module_output/EWR/up4/summary.csv"     

# Single scenario among many, no access to the outer directory

    Code
      realised_structure
    Output
      [1] "base.csv"                            "base.json"                          
      [3] "module_output/EWR/base/summary.csv"  "module_output/EWR/ewr_metadata.json"
      [5] "module_output/EWR/ewr_metadata.yml" 

# Single scenario among many, no access to the outer directory, different names

    Code
      realised_structure
    Output
      [1] "base.csv"                            "base.json"                          
      [3] "module_output/EWR/base/summary.csv"  "module_output/EWR/ewr_metadata.json"
      [5] "module_output/EWR/ewr_metadata.yml" 

# parallel works for two

    Code
      realised_structure
    Output
       [1] "hydrographs"                         "hydrographs/base"                   
       [3] "hydrographs/base/base.csv"           "hydrographs/base/base.json"         
       [5] "hydrographs/down4"                   "hydrographs/down4/down4.csv"        
       [7] "hydrographs/down4/down4.json"        "hydrographs/scenario_metadata.json" 
       [9] "hydrographs/scenario_metadata.yml"   "hydrographs/up4"                    
      [11] "hydrographs/up4/up4.csv"             "hydrographs/up4/up4.json"           
      [13] "module_output"                       "module_output/EWR"                  
      [15] "module_output/EWR/base"              "module_output/EWR/base/summary.csv" 
      [17] "module_output/EWR/base/yearly.csv"   "module_output/EWR/down4"            
      [19] "module_output/EWR/down4/summary.csv" "module_output/EWR/down4/yearly.csv" 
      [21] "module_output/EWR/ewr_metadata.json" "module_output/EWR/ewr_metadata.yml" 
      [23] "module_output/EWR/up4"               "module_output/EWR/up4/summary.csv"  
      [25] "module_output/EWR/up4/yearly.csv"   

# parallel works for one

    Code
      realised_structure
    Output
       [1] "hydrographs"                         "hydrographs/base"                   
       [3] "hydrographs/base/base.csv"           "hydrographs/base/base.json"         
       [5] "hydrographs/down4"                   "hydrographs/down4/down4.csv"        
       [7] "hydrographs/down4/down4.json"        "hydrographs/scenario_metadata.json" 
       [9] "hydrographs/scenario_metadata.yml"   "hydrographs/up4"                    
      [11] "hydrographs/up4/up4.csv"             "hydrographs/up4/up4.json"           
      [13] "module_output"                       "module_output/EWR"                  
      [15] "module_output/EWR/base"              "module_output/EWR/base/summary.csv" 
      [17] "module_output/EWR/down4"             "module_output/EWR/down4/summary.csv"
      [19] "module_output/EWR/ewr_metadata.json" "module_output/EWR/ewr_metadata.yml" 
      [21] "module_output/EWR/up4"               "module_output/EWR/up4/summary.csv"  

# parallel works for no return

    Code
      realised_structure
    Output
       [1] "hydrographs"                         "hydrographs/base"                   
       [3] "hydrographs/base/base.csv"           "hydrographs/base/base.json"         
       [5] "hydrographs/down4"                   "hydrographs/down4/down4.csv"        
       [7] "hydrographs/down4/down4.json"        "hydrographs/scenario_metadata.json" 
       [9] "hydrographs/scenario_metadata.yml"   "hydrographs/up4"                    
      [11] "hydrographs/up4/up4.csv"             "hydrographs/up4/up4.json"           
      [13] "module_output"                       "module_output/EWR"                  
      [15] "module_output/EWR/base"              "module_output/EWR/base/summary.csv" 
      [17] "module_output/EWR/base/yearly.csv"   "module_output/EWR/down4"            
      [19] "module_output/EWR/down4/summary.csv" "module_output/EWR/down4/yearly.csv" 
      [21] "module_output/EWR/ewr_metadata.json" "module_output/EWR/ewr_metadata.yml" 
      [23] "module_output/EWR/up4"               "module_output/EWR/up4/summary.csv"  
      [25] "module_output/EWR/up4/yearly.csv"   

