# spatial input data works

    Code
      names(agged)
    Output
      [1] "scenario"                        "polyID"                         
      [3] "gauge"                           "planning_unit_name"             
      [5] "SWSDLName"                       "ewr_code_main"                  
      [7] "ewr_code_main_mean_ewr_achieved" "geometry"                       

# multi-step theme agg works, nongeom

    Code
      names(agged)
    Output
      [1] "scenario"                                                                                                                     
      [2] "gauge"                                                                                                                        
      [3] "planning_unit_name"                                                                                                           
      [4] "SWSDLName"                                                                                                                    
      [5] "theme"                                                                                                                        
      [6] "theme_ArithmeticMean_objective_text_ArithmeticMean_eco_objective_ArithmeticMean_ewr_code_main_CompensatingFactor_ewr_achieved"

# multi-step theme agg works, auto-edges

    Code
      names(agged)
    Output
      [1] "scenario"                                                                                                                     
      [2] "gauge"                                                                                                                        
      [3] "planning_unit_name"                                                                                                           
      [4] "SWSDLName"                                                                                                                    
      [5] "theme"                                                                                                                        
      [6] "theme_ArithmeticMean_objective_text_ArithmeticMean_eco_objective_ArithmeticMean_ewr_code_main_CompensatingFactor_ewr_achieved"

# Sequencing edge cases

    structure(c(16252, 16617, 16983, 17348, 17713, 18078), class = "Date")

---

    c("BF1_a", "BF1_b", "BF2_a", "BF2_b", "BK1_P", "BK1_S", "CF", 
    "CF1_b", "CF1_c", "CF_a", "CF_b", "CF_c", "LF1", "LF1_P", "LF1_S", 
    "LF2", "OB-WL", "OB-WL1_P", "OB-WL1_S", "OB-WL2_P", "OB-WL2_S", 
    "OB-WL3_P", "OB-WL3_S", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", 
    "OB1_P", "OB1_S", "OB2", "OB3_P", "OB3_S", "OB4_P", "OB4_S", 
    "OB5", "SF1", "SF1_P", "SF1_S", "SF2", "SF3", "VF_a", "VF_b")

---

    c("SS16", "SS20")

---

    structure(c(16252, 16617, 16983, 17348, 17713, 18078), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF1", "LF1", "LF2", "OB-WL1", "OB-WL2", 
    "OB-WL3", "OB-WL4", "OB1", "OB2", "OB3", "OB4", "OB5", "SF1", 
    "SF2", "VF", "CF", "OB-WL", "OB-WM", "OB-WS3", "OB-WS4", "SF3"
    )

---

    c("SS16", "SS20")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF", "CF1", "LF1", "LF2", "OB-WL", "OB-WL1", 
    "OB-WL2", "OB-WL3", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", "OB1", 
    "OB2", "OB3", "OB4", "OB5", "SF1", "SF2", "SF3", "VF")

---

    c("SS16", "SS20")

---

    structure(c(16252, 16617, 16983, 17348, 17713, 18078), class = "Date")

---

    c("BF1_a", "BF1_b", "BF2_a", "BF2_b", "BK1_P", "BK1_S", "CF", 
    "CF1_b", "CF1_c", "CF_a", "CF_b", "CF_c", "LF1", "LF1_P", "LF1_S", 
    "LF2", "OB-WL", "OB-WL1_P", "OB-WL1_S", "OB-WL2_P", "OB-WL2_S", 
    "OB-WL3_P", "OB-WL3_S", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", 
    "OB1_P", "OB1_S", "OB2", "OB3_P", "OB3_S", "OB4_P", "OB4_S", 
    "OB5", "SF1", "SF1_P", "SF1_S", "SF2", "SF3", "VF_a", "VF_b")

---

    c("SS16", "SS20")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1_a", "BF1_b", "BF2_a", "BF2_b", "BK1_P", "BK1_S", "CF", 
    "CF1_b", "CF1_c", "CF_a", "CF_b", "CF_c", "LF1", "LF1_P", "LF1_S", 
    "LF2", "OB-WL", "OB-WL1_P", "OB-WL1_S", "OB-WL2_P", "OB-WL2_S", 
    "OB-WL3_P", "OB-WL3_S", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", 
    "OB1_P", "OB1_S", "OB2", "OB3_P", "OB3_S", "OB4_P", "OB4_S", 
    "OB5", "SF1", "SF1_P", "SF1_S", "SF2", "SF3", "VF_a", "VF_b")

---

    c("SS16", "SS20")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF1", "LF1", "LF2", "OB-WL1", "OB-WL2", 
    "OB-WL3", "OB-WL4", "OB1", "OB2", "OB3", "OB4", "OB5", "SF1", 
    "SF2", "VF", "CF", "OB-WL", "OB-WM", "OB-WS3", "OB-WS4", "SF3"
    )

---

    c("SS16", "SS20")

---

    structure(c(16252, 16617, 16983, 17348, 17713, 18078), class = "Date")

---

    c("OB-WL1", "OB-WL2", "OB-WL3", "OB-WL4", "OB2", "OB3", "OB4", 
    "OB5", "BF1", "BF2", "BK1", "CF1", "LF1", "LF2", "SF1", "SF2", 
    "VF", "OB1", "CF", "OB-WL", "OB-WM", "OB-WS3", "OB-WS4", "SF3"
    )

---

    c("Merrimajeel Creek", "Muggabah Creek", "Western Lachlan Watercourse inc. The Great Cumbung Swamp", 
    "Upper Lachlan River", "Lachlan River - Lake Cargelligo to Willandra Weir", 
    "Merrowie Creek", "Baroona to Warren Weir", "Burrendong Dam to Baroona", 
    "Marthaguy Creek")

---

    structure(c(16252, 16617, 16983, 17348, 17713, 18078), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF", "CF1", "LF1", "LF2", "OB-WL", "OB-WL1", 
    "OB-WL2", "OB-WL3", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", "OB1", 
    "OB2", "OB3", "OB4", "OB5", "SF1", "SF2", "SF3", "VF")

---

    c("SS16", "SS20")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF", "CF1", "LF1", "LF2", "OB-WL", "OB-WL1", 
    "OB-WL2", "OB-WL3", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", "OB1", 
    "OB2", "OB3", "OB4", "OB5", "SF1", "SF2", "SF3", "VF")

---

    c("SS16", "SS20")

---

    structure(c(16252, 16617, 16983, 17348, 17713, 18078), class = "Date")

---

    c("OB-WL1", "OB-WL2", "OB-WL3", "OB-WL4", "OB2", "OB3", "OB4", 
    "OB5", "BF1", "BF2", "BK1", "CF1", "LF1", "LF2", "SF1", "SF2", 
    "VF", "OB1", "CF", "OB-WL", "OB-WM", "OB-WS3", "OB-WS4", "SF3"
    )

---

    c("Merrimajeel Creek", "Muggabah Creek", "Western Lachlan Watercourse inc. The Great Cumbung Swamp", 
    "Upper Lachlan River", "Lachlan River - Lake Cargelligo to Willandra Weir", 
    "Merrowie Creek", "Baroona to Warren Weir", "Burrendong Dam to Baroona", 
    "Marthaguy Creek")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF", "CF1", "LF1", "LF2", "OB-WL", "OB-WL1", 
    "OB-WL2", "OB-WL3", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", "OB1", 
    "OB2", "OB3", "OB4", "OB5", "SF1", "SF2", "SF3", "VF")

---

    c("Western Lachlan Watercourse inc. The Great Cumbung Swamp", 
    "Upper Lachlan River", "Lachlan River - Lake Cargelligo to Willandra Weir", 
    "Baroona to Warren Weir", "Burrendong Dam to Baroona", "Marthaguy Creek", 
    "Merrimajeel Creek", "Muggabah Creek", "Merrowie Creek")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF", "CF1", "LF1", "LF2", "OB-WL", "OB-WL1", 
    "OB-WL2", "OB-WL3", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", "OB1", 
    "OB2", "OB3", "OB4", "OB5", "SF1", "SF2", "SF3", "VF")

---

    c("SS16", "SS20")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1_a", "BF1_b", "BF2_a", "BF2_b", "BK1_P", "BK1_S", "CF", 
    "CF1_b", "CF1_c", "CF_a", "CF_b", "CF_c", "LF1", "LF1_P", "LF1_S", 
    "LF2", "OB-WL", "OB-WL1_P", "OB-WL1_S", "OB-WL2_P", "OB-WL2_S", 
    "OB-WL3_P", "OB-WL3_S", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", 
    "OB1_P", "OB1_S", "OB2", "OB3_P", "OB3_S", "OB4_P", "OB4_S", 
    "OB5", "SF1", "SF1_P", "SF1_S", "SF2", "SF3", "VF_a", "VF_b")

---

    c("Western Lachlan Watercourse inc. The Great Cumbung Swamp", 
    "Upper Lachlan River", "Lachlan River - Lake Cargelligo to Willandra Weir", 
    "Baroona to Warren Weir", "Burrendong Dam to Baroona", "Marthaguy Creek", 
    "Merrimajeel Creek", "Muggabah Creek", "Merrowie Creek")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1_a", "BF1_b", "BF2_a", "BF2_b", "BK1_P", "BK1_S", "CF", 
    "CF1_b", "CF1_c", "CF_a", "CF_b", "CF_c", "LF1", "LF1_P", "LF1_S", 
    "LF2", "OB-WL", "OB-WL1_P", "OB-WL1_S", "OB-WL2_P", "OB-WL2_S", 
    "OB-WL3_P", "OB-WL3_S", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", 
    "OB1_P", "OB1_S", "OB2", "OB3_P", "OB3_S", "OB4_P", "OB4_S", 
    "OB5", "SF1", "SF1_P", "SF1_S", "SF2", "SF3", "VF_a", "VF_b")

---

    c("SS16", "SS20")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF1", "LF1", "LF2", "OB-WL1", "OB-WL2", 
    "OB-WL3", "OB-WL4", "OB1", "OB2", "OB3", "OB4", "OB5", "SF1", 
    "SF2", "VF", "CF", "OB-WL", "OB-WM", "OB-WS3", "OB-WS4", "SF3"
    )

---

    c("SS16", "SS20")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1_a", "BF1_b", "BF2_a", "BF2_b", "BK1_P", "BK1_S", "CF", 
    "CF1_b", "CF1_c", "CF_a", "CF_b", "CF_c", "LF1", "LF1_P", "LF1_S", 
    "LF2", "OB-WL", "OB-WL1_P", "OB-WL1_S", "OB-WL2_P", "OB-WL2_S", 
    "OB-WL3_P", "OB-WL3_S", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", 
    "OB1_P", "OB1_S", "OB2", "OB3_P", "OB3_S", "OB4_P", "OB4_S", 
    "OB5", "SF1", "SF1_P", "SF1_S", "SF2", "SF3", "VF_a", "VF_b")

---

    c("Western Lachlan Watercourse inc. The Great Cumbung Swamp", 
    "Upper Lachlan River", "Lachlan River - Lake Cargelligo to Willandra Weir", 
    "Baroona to Warren Weir", "Burrendong Dam to Baroona", "Marthaguy Creek", 
    "Merrimajeel Creek", "Muggabah Creek", "Merrowie Creek")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("OB-WL1", "OB-WL2", "OB-WL3", "OB-WL4", "OB2", "OB3", "OB4", 
    "OB5", "BF1", "BF2", "BK1", "CF1", "LF1", "LF2", "SF1", "SF2", 
    "VF", "OB1", "CF", "OB-WL", "OB-WM", "OB-WS3", "OB-WS4", "SF3"
    )

---

    c("Merrimajeel Creek", "Muggabah Creek", "Western Lachlan Watercourse inc. The Great Cumbung Swamp", 
    "Upper Lachlan River", "Lachlan River - Lake Cargelligo to Willandra Weir", 
    "Merrowie Creek", "Baroona to Warren Weir", "Burrendong Dam to Baroona", 
    "Marthaguy Creek")

---

    structure(c(16071, 16801, 17532), class = "Date")

---

    c("BF1", "BF2", "BK1", "CF", "CF1", "LF1", "LF2", "OB-WL", "OB-WL1", 
    "OB-WL2", "OB-WL3", "OB-WL4", "OB-WM", "OB-WS3", "OB-WS4", "OB1", 
    "OB2", "OB3", "OB4", "OB5", "SF1", "SF2", "SF3", "VF")

---

    c("SS16", "SS20")

# Temporal

    structure(c(16436, 17625, NA), class = "Date")

