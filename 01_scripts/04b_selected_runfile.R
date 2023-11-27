

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2023-16-01



## PURPOSE: 
#' The goal is to use the data pipeline to generate final data tibble or visual
#' that will be used in Phuse RBM QTL white paper.


## Reference: -- https://advance.phuse.global/display/WEL/Quality+Tolerance+Limits


# 1. LOAD PACKAGES ---- 
pacman::p_load(here)


## Run Files -- Selected Questions

# Read data
source(here("01_scripts/01b_selected_read_data.R"))

# Data wrangling 
source(here("01_scripts/02b_selected_data_wrangling.R"))

# Generate output (table or visual)
source(here("01_scripts/03b_selected_data_visualization.R")) 

# House keeping
rm(q3_raw, q4_raw, q5_raw, q6_raw, q7_raw, q8_raw, q8_clean, q10_raw, q23_raw, q24_raw)

rm(q3_clean, q3_others, q4_clean, q4_others, q5_clean, q5_others, q6_clean, q7_clean, 
   q8_option1, q8_option2, q10_clean, q23_clean, q24_clean)




