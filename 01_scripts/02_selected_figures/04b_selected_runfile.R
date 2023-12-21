

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2023-11-01



## PURPOSE: 
#' The goal is to use the data pipeline to generate final data tibble or visual
#' that will be used in Phuse RBM QTL white paper.


## Reference: -- https://advance.phuse.global/display/WEL/Quality+Tolerance+Limits



## Run File to generate the selected figures 

# Read data
source(here::here("01_scripts/02_selected_figures/01b_selected_read_data.R"))

# Data wrangling 
source(here::here("01_scripts/02_selected_figures/02b_selected_data_wrangling.R"))

# Generate output (table or visual)
source(here::here("01_scripts/02_selected_figures/03b_selected_data_visualization.R")) 

# House keeping
rm(q3_raw, q4_raw, q5_raw, q6_raw, q7_raw, q8_raw,  q10_raw, q18_raw, q19_raw,
   q20.3_raw, q23_raw, q23_rate_raw, q24_raw, q24_rate_raw)

rm(q3_clean, q3_others, q4_clean, q4_others, q5_clean, q5_others, q6_clean, q7_clean, 
   q8_clean, q8_option1, q8_option2, q10_clean, q18_clean, q19_clean, q19_comments,
   q20.3_clean, q20.3_other, q23_clean, q23_comments, q23_rate_clean, q24_clean, q24_comments, q24_rate_clean)



# ------------------------------------------------------------------------------
## Do NOT RUN 
# rm(plot03, plot04, plot05, plot06_option1, plot06_option2, plot07,
#    plot08_option1, plot08_option2, plot10, plot18, plot19, plot20.3,
#    plot23_status, plot23_rate_option1, plot23_rate_option2, plot24_status,
#    plot24_rate_option1, plot24_rate_option2)



