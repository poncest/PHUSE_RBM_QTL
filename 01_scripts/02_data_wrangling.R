

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Author: Steven Ponce
#' Date: 2023-11-01



# 1. TIDY THE DATA ----

# Question 01 ----
q1_clean <- q1_raw %>% 
  
  # rename column
  rename(your_company_type = x1_your_company_type_check_one) %>% 
  
  # remove number formats (a., b.)
  mutate(your_company_type = str_remove_all(string = your_company_type, pattern = "[abcdefg]\\.")) %>% 
  
  # summary
  bar_summary(group_col = your_company_type) %>%
  
  # add labels
  mutate(
    # your_company_type = str_glue("{ your_company_type } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
      )


# Question 02 ----
q2_clean <- q2_raw %>% 
  
  # rename column
  rename(company_size_num_trials = x2_your_company_size_number_of_concurrent_trials_check_one) %>% 
  
  # remove number formats (a., b.)
  mutate(company_size_num_trials = str_remove_all(string = company_size_num_trials, pattern = "[abcdefg]\\.")) %>% 

  # fix operators (<=)
  mutate(company_size_num_trials = case_when(
    company_size_num_trials == "Small( (?100 Phase I-III clinical trials ongoing)"        ~ "Small (≤ 100 Phase I-III clinical trials",
    company_size_num_trials == "Medium (>100& ? 200 Phase I-III clinical trials ongoing)" ~ "Medium (> 100 - ≤ 200 Phase I-III clinical trials",
    company_size_num_trials == "Large (>200 Phase I-III clinical trials ongoing)"         ~ "Large (> 200 Phase I-III clinical trials"
    )) %>% 
  
  # separate into 2 columns
  separate_wider_delim(cols = company_size_num_trials,
                       delim = " (",
                       names = c("company_size", "number_trials"),
  ) %>% 

  # summary
  group_by(company_size, number_trials) %>% 
  bar_summary_2() %>% 
 
  # reorder
  mutate(company_size = fct_reorder(company_size, count)) %>% 

  # add labels
  mutate(
    bar_axis  = str_glue("{ company_size } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  )



# Question 03 ----
q3_clean <- q3_raw %>% 
  
  # rename column
  rename(functional_area = x3_which_functional_area_s_are_involved_in_trial_level_risk_based_approaches_to_quality_1_please_check_all_that_apply_1_please_see_ich_e6_r2_section_5_0_and_ich_e8_r1) %>% 
   
  # remove number formats (a., b.) 
  mutate(functional_area = str_remove_all(string = functional_area, pattern = "[abcdefg]\\.")) %>% 
  
  # separate `functional_area`
  separate_wider_delim(cols = functional_area, 
                       delim = ", ", 
                       names = c("1","2","3","4","5","6","7"),
                       too_few = "align_end") %>%  
  
  # separate `others_2`
  separate_wider_delim(cols = other_2, 
                       delim = "/", 
                       names = c("8","9"),
                       too_few = "align_end") %>% 
  
  # separate `others_2`
  separate_wider_delim(cols = 09, 
                       delim = ", ", 
                       names = c("10","11","12","13"),
                       too_few = "align_end") %>% 
  
  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "id",
    values_to = "functional_area",
    values_drop_na = TRUE, 
  ) %>% 
  
  # filter and drop `id`
  select(-id) %>% 
  filter(functional_area != "") %>% 
  
  # rename Others
  mutate(functional_area = case_when(
    functional_area == "Other (Please specify below)" ~ "Other",
    TRUE ~ as.character(functional_area)
  )) %>% 
  
  # trim white spaces
  mutate(functional_area = str_trim(functional_area)) %>% 
  
  # consolidate `central monitoring`
  mutate(functional_area = case_when(
    functional_area == "Central Monitor"         ~ "Central Monitoring",
    functional_area == "Centralized Monitoring"  ~ "Central Monitoring",
    TRUE ~ as.character(functional_area)
  )) %>% 
  
  # remove `Others`. They are already included: central monitoring, imaging, diagnostic, etc.
  filter(functional_area != "Other") %>% 
  
  # lump 
  mutate(functional_area = fct_lump(functional_area, 6)) %>% 

  # summary
  bar_summary(group_col = functional_area) %>%
  
  # reorder
  mutate(functional_area = fct_reorder(functional_area, count)) %>% 

  # add labels
  mutate(
    bar_axis  = str_glue("{ functional_area } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  )


# Break down of Q3 `Others`
# I don't think we need a plot here, a Table is a better option
q3_clean_others <- q3_raw %>% 
  select(other_2) %>% 
  
  # separate `others_2`
  separate_wider_delim(cols = other_2, 
                       delim = "/", 
                       names = c("1","2"),
                       too_few = "align_end") %>% 
  
  # separate `others_2`
  separate_wider_delim(cols = 02, 
                       delim = ", ", 
                       names = c("3","4","5","6"),
                       too_few = "align_end") %>% 
  
  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "id",
    values_to = "other",
    values_drop_na = TRUE, 
  ) %>% 
  
  # filter and drop `id`
  select(-id) %>% 
  filter(other != "") %>% 
  
  # trim white spaces
  mutate(other = str_trim(other)) %>% 
  
  # consolidate `central monitoring`
  mutate(other = case_when(
    other == "Central Monitor"         ~ "Central Monitoring",
    other == "Centralized Monitoring"  ~ "Central Monitoring",
    TRUE ~ as.character(other)
  )) %>% 

  # summary
  bar_summary(group_col = other) %>%
  
  # reorder
  mutate(other = fct_reorder(other, count)) %>% 
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ other } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  )



# Question 04 ----
q4_clean <- q4_raw %>% 
  
  # rename column
  rename(functional_area = x4_which_functional_area_leads_trial_level_risk_based_approaches_to_quality_please_check_only_one) %>% 
  
  # remove number formats (a., b.) 
  mutate(functional_area = str_remove_all(string = functional_area, pattern = "[abcdefg]\\.")) %>% 
  
  # separate `functional_area`
  separate_wider_delim(cols = functional_area, 
                       delim = ", ", 
                       names = c("1","2","3","4","5"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "id",
    values_to = "functional_area",
    values_drop_na = TRUE, 
  ) %>% 
  
  # filter and drop `id`
  select(-id) %>% 
  filter(functional_area != "") %>% 
  
  # remove Others
  filter(functional_area != "Other (Please specify below)") %>% 

  # consolidate `other` category
  mutate(functional_area = case_when(
    functional_area == "Centralized Mointoring"                                      ~ "Central Monitoring",
    functional_area == "Dedicated RBQM lead or Risk Manager"                         ~ "Risk Manager",
    functional_area == "Dedicated Risk Managment Specialist"                         ~ "Risk Manager",
    functional_area == "Each functional area has an oversight team to assess risk."  ~ "Oversight Team",
    functional_area == "the study team have an RBQM working group and they decide who takes the lead in the group, generally its Operations but can be one of the other functions like biostats or clinical science "  ~ "RBQM working group",
    TRUE ~ as.character(functional_area)
  )) %>% 
  
  # lump
  mutate(functional_area = fct_lump(functional_area, 6)) %>% 
  
  # summary
  bar_summary(group_col = functional_area) %>% 

  # reorder
  mutate(functional_area = fct_reorder(functional_area, count)) %>% 
 
  # add labels
  mutate(
    bar_axis  = str_glue("{ functional_area } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  )


# Question 05 ----
q5_clean <- q5_raw %>% 
  
  # rename column
  rename(trial_types = x5_are_there_any_trial_types_where_your_company_does_not_apply_risk_based_approaches_to_quality_please_select_all_that_apply_and_provide_comments_as_to_why_this_is_the_case) %>% 
  
  # select only `trial_types` col 
  select(trial_types) %>% 
  
  # remove number formats (a., b.) 
  mutate(trial_types = str_remove_all(string = trial_types, pattern = "[abcdefg]\\.")) %>% 
  
  # replace ", " with " | " in `Others`
  # so it doesn't separate in the step below
  mutate(
    trial_types = str_replace_all(string = trial_types,
                                  pattern     = "Registry, non-interventional", 
                                  replacement = "Registry | non-interventional")
    ) %>% 
  
  # separate `trial_types`
  separate_wider_delim(cols = trial_types, 
                       delim = ", ",
                       names = c("1","2","3"),
                       too_few = "align_start") %>% 

  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = "id",
    values_to = "trial_types",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove `id` column
   select(-id) %>% 
  
  # rename `comment` to `reasons`
  mutate(trial_types = case_when(
    trial_types == "Comment (Please comment why below)" ~ "Different Reasons",
    TRUE ~ as.character(trial_types)
  )) %>% 
  
  # return other to its original state
  mutate(trial_types = case_when(
    trial_types == "Other (Registry | non-interventional)" ~ "Other (Registry, non-interventional)",
    TRUE ~ as.character(trial_types)
  )) %>% 
  
  # summary
  bar_summary(group_col = trial_types) %>% 
  
  # reorder
  mutate(trial_types = fct_reorder(trial_types, count)) %>% 
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ trial_types } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  )


# Lets examine Q5 `comment why` column
# It corresponds to Q5 `Different Reasons`

q5_clean_why <- q5_raw %>% 
  
  # select `comment_why` col
  select(comment_why) %>% 
  
  # filter for only `comment_why`
  filter(comment_why != "") %>% 
  
  # format
  mutate(comment_why = str_to_sentence(comment_why))

  
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
  filter(documentation_type != "")
  
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





