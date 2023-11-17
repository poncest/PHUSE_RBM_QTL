

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2023-16-01



# 1. LOAD PACKAGES ---- 
pacman::p_load(tidyverse, here, janitor, lubridate, scales, ggtext, showtext, data.table)
pacman::p_load(ggdist,urbnthemes)



# source helper functions
source("03_functions/01_helper_functions.R")



# 2. READ IN THE DATA ----

#' Note:
#' Since the data was collected using Google Forms, the incoming raw data is
#' extremely messy. At this moment, it might be a good idea to:
#' - select the columns corresponding to a specific question,
#' - create a dataframe for each question, then proceed to clean the data,
#' - finally generate the visual. 


# loading the raw data (as is)
raw_data <- fread(file = '00_data/IN/qtl_survey_response.csv') %>% 
  clean_names()

# Question 03 ----
q3_raw <- raw_data %>%
  select(x3_which_functional_area_s_are_involved_in_trial_level_risk_based_approaches_to_quality_1_please_check_all_that_apply_1_please_see_ich_e6_r2_section_5_0_and_ich_e8_r1,
         other_2)


# Question 06 ----
q6_raw <- raw_data %>%
  select(
    x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_fih,
    x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_i_non_fih,
    x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_ii,
    x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iii_regulatory_submission,
    x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iii_follow_up,
    x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iv
    )


# Question 23 ----      



# Question 24 ----      



