

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Author: Steven Ponce
#' Date: 2023-11-01



# 1. LOAD PACKAGES ---- 
pacman::p_load(tidyverse, here, janitor, lubridate, scales, ggtext, showtext, data.table)
pacman::p_load(tidytext, MetBrewer)



# source helper functions
source("03_functions/01_helper_functions.R")



# 2. READ IN THE DATA ----

#' Note:
#' Since the data was collected using Google Forms, the incoming raw data is
#' extremely messy. At this moment, it might be a good idea to:
#' - select the columns corresponding to a specific column,
#' - create a dataframe for each question, then proceed to clean the data,
#' - finally generate the visual. 


# loading the raw data (as is)
raw_data <- fread(file = '00_data/IN/qtl_survey_response.csv') %>% 
  clean_names()


# Question 01 ----
q1_raw <- raw_data %>%
  select(x1_your_company_type_check_one)


# Question 02 ----
q2_raw <- raw_data %>%
  select(x2_your_company_size_number_of_concurrent_trials_check_one)


# Question 03 ----
q3_raw <- raw_data %>%
  select(x3_which_functional_area_s_are_involved_in_trial_level_risk_based_approaches_to_quality_1_please_check_all_that_apply_1_please_see_ich_e6_r2_section_5_0_and_ich_e8_r1,
         other_2)


# Question 04 ----
q4_raw <- raw_data %>%
  select(x4_which_functional_area_leads_trial_level_risk_based_approaches_to_quality_please_check_only_one,
         other_3)


# Question 05 ----      
q5_raw <- raw_data %>%
  select(x5_are_there_any_trial_types_where_your_company_does_not_apply_risk_based_approaches_to_quality_please_select_all_that_apply_and_provide_comments_as_to_why_this_is_the_case,
         comment_why)


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


# Question 07 ----      
q7_raw <- raw_data %>%
  select(
    x7_if_you_answered_yes_to_other_risk_based_approaches_used_in_question_6_please_identify_those_used_please_check_all_that_apply
  )


# Question 08 ----      
q8_raw <- raw_data %>%
  select(
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_identification_of_ct_qs,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_overall_implementation_of_qb_d,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_qt_ls_utilized,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_qt_ls_aligned_with_ct_qs,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_qtl_review_processes,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_frequency_of_qtl_review,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_communication_of_qtl_breaches,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_implementation_of_corrective_actions,
    x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_reporting_significant_qtl_deviations_in_csr
    )


# Question 09 ----      
q9_raw <- raw_data %>%
  select(x9_does_your_company_have_a_process_in_place_with_regard_to_completing_a_feedback_loop)

