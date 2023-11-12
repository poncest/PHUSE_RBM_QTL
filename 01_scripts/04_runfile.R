

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Author: Steven Ponce
#' Date: 2023-11-01


## PURPOSE: 
#' The goal is to use the data pipeline to generate final data tibble or visual
#' that will be used in Phuse RBM QTL white paper.


## Reference: -- https://advance.phuse.global/display/WEL/Quality+Tolerance+Limits


# 1. LOAD PACKAGES ---- 
pacman::p_load(here)

# Pipeline - Run Files

# Read data
source(here("01_scripts/01_read_data.R"))

# Data wrangling 
source(here("01_scripts/02_data_wrangling.R"))

# Generate output (table or visual)
source(here("01_scripts/03_data_visualization.R")) 

# House keeping
rm(q1_raw, q2_raw, q3_raw, q4_raw, q5_raw, q6_raw, q7_raw, q8_raw, q9_raw, q10_raw)


