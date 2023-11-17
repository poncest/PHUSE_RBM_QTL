

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2023-16-01



# 1. TIDY THE DATA ----

# Question 03 ----
q3_clean <- q3_raw %>% 
  
  # rename column
  rename(functional_area = x3_which_functional_area_s_are_involved_in_trial_level_risk_based_approaches_to_quality_1_please_check_all_that_apply_1_please_see_ich_e6_r2_section_5_0_and_ich_e8_r1) %>% 
  
  # select only `functional_area` col
  select(functional_area) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  
  # remove number formats (a., b.) 
  mutate(functional_area = str_remove_all(string = functional_area, pattern = "[abcdefg]\\.")) %>% 
  
  # pivot longer
  pivot_longer(
    cols = c(functional_area),
    names_to = "column_id",
    values_to = "functional_area",
    values_drop_na = TRUE, 
  ) %>% 

  # separate `functional_area`
  separate_wider_delim(cols = functional_area, 
                       delim = ", ", 
                       names = c("C1","C2","C3","C4","C5","C6","C7"),
                       too_few = "align_end") %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id, column_id),
    names_to = "temp_col",
    values_to = "functional_area",
    values_drop_na = TRUE, 
  ) %>% 
  
  # select columns
  select(response_id, functional_area) %>% 
  
  # Format `Others`
  mutate(functional_area = case_when(
    functional_area == "Other (Please specify below)" ~ "Other",
    TRUE ~ as.character(functional_area)
  )) %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    response_id     = as_factor(response_id),
    functional_area = factor(functional_area, 
                             levels = c("Clinical Operations", 
                                        "Data Management", 
                                        "Biostatistics",
                                        "Clinical Science", 
                                        "Safety Science", 
                                        "Quality Functions",
                                        "Other")
                             ))



# Question 06 ----
q6_clean <- q6_raw %>% 
  
  # rename columns
  rename(
    fih    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_fih,
    ph1    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_i_non_fih,
    ph2    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_ii,
    ph3_rs = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iii_regulatory_submission,
    ph3_fu = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iii_follow_up,
    ph4    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iv
    ) %>% 
  
  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "stage_phase",
    values_to = "rbm_approaches",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(rbm_approaches != "") %>% 
  
  # separate `rbm_approaches`
  separate_wider_delim(cols = rbm_approaches, 
                       delim = ", ",
                       names = c("1","2","3", "4","5"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = -stage_phase,
    names_to = "id",
    values_to = "rbm_approaches",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove `id` column
  select(-id) %>% 
  
  # remove parenthesis (1), (2), and (Check for YES or leave blank for NO)
  mutate(
    rbm_approaches = str_remove_all(string = rbm_approaches, pattern = "\\(\\d+\\)"), # remove (1), (2)
    rbm_approaches = str_remove_all(string = rbm_approaches, pattern = " \\(Check for YES or leave blank for NO\\)"),
    ) %>% 
  
  # summary
  group_by(stage_phase, rbm_approaches) %>% 
  bar_summary_2() %>% 
  ungroup() %>% 

  # reorder
  mutate(rbm_approaches = reorder_within(rbm_approaches, count, stage_phase)) %>% 
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ rbm_approaches } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  ) %>% 
  
  # recode stage_phase
  mutate(stage_phase = case_when(
    stage_phase == "fih"    ~ "FIH",
    stage_phase == "ph1"    ~ "Phase I Non-FIH",
    stage_phase == "ph2"    ~ "Phase II",
    stage_phase == "ph3_rs" ~ "Phase III - RS",
    stage_phase == "ph3_fu" ~ "Phase III - FU",
    stage_phase == "ph4"    ~ "Phase IV",
  )) 


# Question 07 ----
q7_clean <- q7_raw %>% 
  
  # rename column
  rename(other_rbm_approaches = x7_if_you_answered_yes_to_other_risk_based_approaches_used_in_question_6_please_identify_those_used_please_check_all_that_apply) %>%  
  
  # remove empty rows
  filter(other_rbm_approaches != "") %>% 
  
  # remove number formats (a., b.) 
  mutate(other_rbm_approaches = str_remove_all(string = other_rbm_approaches, pattern = "[abcdefg]\\.")) %>% 
  
  # separate `other_rbm_approaches`
  separate_wider_delim(cols = other_rbm_approaches, 
                       delim = ", ",
                       names = c("1","2","3"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "id",
    values_to = "other_rbm_approaches",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove `id` column
  select(-id) %>% 
  
  # summary
  bar_summary(group_col = other_rbm_approaches) %>% 
  
  # reorder
  mutate(trial_types = fct_reorder(other_rbm_approaches, count)) %>% 
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ other_rbm_approaches } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  )



# Question 08 ----
q8_clean <- q8_raw %>% 
  
  # rename columns
  rename(
    identification     = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_identification_of_ct_qs,
    implementation     = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_overall_implementation_of_qb_d,
    utilized           = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_qt_ls_utilized,
    aligned            = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_qt_ls_aligned_with_ct_qs,
    review             = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_qtl_review_processes,
    frequency          = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_frequency_of_qtl_review,
    communication      = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_communication_of_qtl_breaches,
    corrective_actions = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_implementation_of_corrective_actions,
    reporting          = x8_is_your_company_s_risk_based_approach_to_quality_applied_differently_depending_on_the_following_trial_atttributes_please_note_some_organizations_may_have_more_than_one_activity_that_is_tightly_integrated_across_multiple_elements_e_g_trial_size_and_phase_please_utilize_the_mixed_response_for_these_situations_reporting_significant_qtl_deviations_in_csr
  ) %>% 
  
  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "activity",
    values_to = "trial_attribute",
    values_drop_na = TRUE,
    ) %>% 
  
  # separate `other_rbm_approaches`
  separate_wider_delim(cols = trial_attribute, 
                       delim = ", ",
                       names = c("1","2","3","4"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = -activity,
    names_to = "id",
    values_to = "trial_attribute",
    values_drop_na = TRUE,
    ) %>% 
  
  # remove empty rows
  filter(trial_attribute != "") %>% 
  
  # remove ((Tick if YES/Leave blank if NO)
  mutate(trial_attribute = str_remove_all(string = trial_attribute, 
                                          pattern = " \\(\\(Tick if YES/Leave blank if NO\\)")) %>% 

  # remove `id` column
  select(-id) %>% 
  
  # summary
  group_by(activity, trial_attribute) %>% 
  bar_summary_2() %>%
  ungroup() %>% 
  
  # recode activity
  mutate(activity = case_when(
    activity == "identification"     ~ "Identification<br>of CTQ’s",
    activity == "implementation"     ~ "Overall<br>Implementation<br>of QbD",
    activity == "utilized"           ~ "QTL’s<br>Utilized",
    activity == "aligned"            ~ "QTL’s Aligned<br>with CTQ’s",
    activity == "review"             ~ "QTL Review<br>Processes",
    activity == "frequency"          ~ "Frequency<br>of QTL Review",
    activity == "communication"      ~ "Communication<br>of QTL Breaches",
    activity == "corrective_actions" ~ "Implementation<br>of Corrective<br>Actions",
    activity == "reporting"          ~ "Reporting<br>Significant QTL<br>Deviations in CSR"
  )) %>% 

  # reorder
  mutate(trial_attribute = reorder_within(trial_attribute, count, activity)) %>% 

  # add labels
  mutate(
    bar_axis  = str_glue("{ trial_attribute } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  ) 


# Question 09 ----
q9_clean <- q9_raw %>% 
  
  # rename column
  rename(feedback_loop_process = x9_does_your_company_have_a_process_in_place_with_regard_to_completing_a_feedback_loop) %>%   
  
  # summary
  bar_summary(group_col = feedback_loop_process) %>% 
  
  # reorder
  mutate(feedback_loop_process = fct_reorder(feedback_loop_process, count)) %>% 
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ feedback_loop_process } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  )



# Question 10 ----
q10_clean <- q10_raw %>% 
  
  # rename columns
  rename(
    identification     = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_identification_of_ct_qs,
    implementation     = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_implementation_of_qb_d,
    risk               = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_implementation_of_a_risk_strategy_3,
    utilized           = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_qt_ls_utilized,
    review             = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_qtl_review_processes,
    aligned            = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_qt_ls_aligned_with_ct_qs,
    frequency          = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_frequency_of_qtl_review,
    communication      = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_communication_of_qtl_breaches,
    corrective_actions = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_implementation_of_corrective_actions,
    reporting          = x10_how_does_your_company_document_your_risk_based_approaches_to_quality_please_check_all_that_apply_the_goal_of_this_question_and_others_that_follow_with_similar_activities_regarding_ctq_s_qbd_is_to_understand_what_and_how_your_company_makes_links_if_any_from_these_activities_to_qtl_s_reporting_significant_qtl_deviations_in_csr
  ) %>% 

  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "activity",
    values_to = "documentation_type",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(documentation_type != "") %>% 
  
  # format documentation_type
  mutate(documentation_type = case_when(
    documentation_type == "Part of a Protocol-Level Plan(e.g.,part of a Risk Mgmt Strategy Plan)"            ~ "Part of a Protocol-Level Plan",
    documentation_type == "Separate Protocol- Level Plans (example Risk Mgmt Strategy Plan,Monitoring Plan)" ~ "Separate Protocol- Level Plans",
    documentation_type == "Via Technology Solution(s)"                                                       ~ "Via Technology Solution",
    TRUE                                                                                                     ~ as.character(documentation_type)
  )) %>% 
  
  # separate `other_rbm_approaches`
  separate_wider_delim(cols = documentation_type, 
                       delim = ", ",
                       names = c("1","2","3"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = -activity,
    names_to = "id",
    values_to = "documentation_type",
    values_drop_na = TRUE,
  ) %>% 
  
  # format documentation_type
  mutate(documentation_type = case_when(
    documentation_type == "Part of a Protocol-Level Plan(e.g.,part of a Risk Mgmt Strategy Plan)"            ~ "Part of a Protocol-Level Plan",
    documentation_type == "Separate Protocol- Level Plans (example Risk Mgmt Strategy Plan,Monitoring Plan)" ~ "Separate Protocol- Level Plans",
    documentation_type == "Via Technology Solution(s)"                                                       ~ "Via Technology Solution",
    TRUE                                                                                                     ~ as.character(documentation_type)
  )) %>% 
  
  # remove `id` column
  select(-id) %>% 
  
# summary
group_by(activity, documentation_type) %>% 
  bar_summary_2() %>%
  ungroup() %>% 
    
  # recode activity
  mutate(activity = case_when(
    activity == "identification"     ~ "Identification of CTQ’s",
    activity == "implementation"     ~ "Overall Implementation<br>of QbD",
    activity == "risk"               ~ "Implementation of<br>Risk Strategy",
    activity == "utilized"           ~ "QTL’s Utilized",
    activity == "review"             ~ "QTL Review Processes",
    activity == "aligned"            ~ "QTL’s Aligned<br>with CTQ’s",
    activity == "frequency"          ~ "Frequency of<br>QTL Review",
    activity == "communication"      ~ "Communication<br>of QTL Breaches",
    activity == "corrective_actions" ~ "Implementation<br>of Corrective<br>Actions",
    activity == "reporting"          ~ "Reporting<br>Significant QTL<br>Deviations in CSR"
  )) %>% 
  
  # reorder
  mutate(documentation_type = reorder_within(documentation_type, count, activity)) %>% 
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ documentation_type } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  ) 





