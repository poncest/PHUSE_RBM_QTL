

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


# |- others ----
q3_others <- q3_raw %>% 
  
  # select only `others` col
  select(other_2) %>% 
  
  # rename column
  rename(other_functional_area = other_2) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  
  # separate `functional_area`
  separate_wider_delim(cols = other_functional_area, 
                       delim = ", ", 
                       names = c("C1","C2","C3","C4"),
                       too_few = "align_end") %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "temp_col",
    values_to = "other_functional_area",
    values_drop_na = TRUE, 
  ) %>% 
  
  # select columns
  select(response_id, other_functional_area) %>% 
  
  # trim white spaces
  mutate(other_functional_area = str_trim(other_functional_area)) %>% 
  
  # format
  mutate(other_functional_area = case_when(
    other_functional_area == "Central Monitor"         ~ "Central Monitoring",
    other_functional_area == "Centralized Monitoring"  ~ "Central Monitoring",
    other_functional_area == "Saas programmers"        ~ "SaaS programmers",
    TRUE ~ as.character(other_functional_area)
  )) 


  

# Question 04 ----
q4_clean <- q4_raw %>% 
  
  # rename column
  rename(functional_area = x4_which_functional_area_leads_trial_level_risk_based_approaches_to_quality_please_check_only_one) %>% 
  
  # select only `functional_area` col
  select(functional_area) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  
  # remove number formats (a., b.) 
  mutate(functional_area = str_remove_all(string = functional_area, pattern = "[abcdefg]\\.")) %>% 
  
  # separate `functional_area`
  separate_wider_delim(cols = functional_area, 
                       delim = ", ", 
                       names = c("C1","C2","C3","C4","C5","C6","C7"),
                       too_few = "align_end") %>% 

  # pivot longer
  pivot_longer(
    cols = -c(response_id),
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


# |- others ----
q4_others <- q4_raw %>% 
  
  # rename column
  rename(functional_area = x4_which_functional_area_leads_trial_level_risk_based_approaches_to_quality_please_check_only_one,
         other_functional_area = other_3) %>% 
  
  # select only `others` col
  select(other_functional_area) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  
  # select columns
  select(response_id, other_functional_area) %>% 
  
  # consolidate `other` category
  mutate(other_functional_area = case_when(
    other_functional_area == "Centralized Mointoring"                                      ~ "Central Monitoring",
    other_functional_area == "Dedicated RBQM lead or Risk Manager"                         ~ "Risk Manager",
    other_functional_area == "Dedicated Risk Managment Specialist"                         ~ "Risk Manager",
    other_functional_area == "Each functional area has an oversight team to assess risk."  ~ "Oversight Team",
    other_functional_area == "the study team have an RBQM working group and they decide who takes the lead in the group, generally its Operations but can be one of the other functions like biostats or clinical science "  ~ "RBQM working group",
    TRUE ~ as.character(other_functional_area)
  )) %>% 
  
  # factors
  mutate(response_id = as_factor(response_id))


# Question 05 ----
q5_clean <- q5_raw %>% 
  
  # rename column
  rename(trial_types = x5_are_there_any_trial_types_where_your_company_does_not_apply_risk_based_approaches_to_quality_please_select_all_that_apply_and_provide_comments_as_to_why_this_is_the_case) %>% 
  
  # select only `trial_types` col 
  select(trial_types) %>%  
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  
  # remove number formats (a., b.) 
  mutate(trial_types = str_remove_all(string = trial_types, pattern = "[abcdefg]\\."))  %>% 
  
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
                       names = c("C1","C2","C3"),
                       too_few = "align_end")  %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "temp_col",
    values_to = "trial_types",
    values_drop_na = TRUE, 
  ) %>% 
  
  # format
  mutate(trial_types = case_when(
    trial_types == "Other (Registry | non-interventional)"     ~ "Other",
    trial_types == "Comment (Please comment why below)"        ~ "Comment why",
    trial_types == "Post-marketing approval (interventional)"  ~ "Post-marketing approval",
    TRUE ~ as.character(trial_types)
  )) %>% 
  
  # select columns
  select(response_id, trial_types) %>% 
  
  # filter out `comment why` 
  filter(trial_types != "Comment why") %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    response_id = as_factor(response_id),
    trial_types = factor(trial_types,
                         levels = c("Phase I",
                                    "Phase II",
                                    "Biostatistics",
                                    "Complex Design",
                                    "Post-marketing approval",
                                    "Other")
    ))



# |- others ----
q5_others <- q5_raw %>% 
  
  # rename column
  rename(trial_types = x5_are_there_any_trial_types_where_your_company_does_not_apply_risk_based_approaches_to_quality_please_select_all_that_apply_and_provide_comments_as_to_why_this_is_the_case) %>% 
  
  # select only `comments` col 
  select(comment_why) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  
  # select columns
  select(response_id, comment_why) %>% 
  
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
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  select(response_id, everything()) %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "stage_phase",
    values_to = "rbm_approaches",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(rbm_approaches != "") %>% 
  
  # separate `rbm_approaches`
  separate_wider_delim(cols = rbm_approaches, 
                       delim = ", ",
                       names = c("C1","C2","C3", "C4","C5"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C5),
    names_to = "temp_col",
    values_to = "rbm_approaches",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove `id` column
  select(-temp_col) %>% 
  
  # remove parenthesis (1), (2), and (Check for YES or leave blank for NO)
  mutate(
    rbm_approaches = str_remove_all(string = rbm_approaches, pattern = "\\(\\d+\\)"), # remove (1), (2)
    rbm_approaches = str_remove_all(string = rbm_approaches, pattern = " \\(Check for YES or leave blank for NO\\)"),
    ) %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    #response_id     = as_factor(response_id),                                
    rbm_approaches  = factor(rbm_approaches, 
                             levels = c("CTQ Factors", 
                                        "QbD Processes", 
                                        "QTL'S",
                                        "Alignment of QTL'S with CTQ'S", 
                                        "Other Risk-Based Approaches"
                                        )
    )) %>% 
  
  # recode `stage_phase` column
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
  
  # rename columns
  rename(
   rbm_approach = x7_if_you_answered_yes_to_other_risk_based_approaches_used_in_question_6_please_identify_those_used_please_check_all_that_apply
  ) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  select(response_id, everything()) %>% 
  
  # remove number formats (a., b.) 
  mutate(rbm_approach = str_remove_all(string = rbm_approach, pattern = "[abcdefg]\\.")) %>% 
  
  # separate `rbm_approach`
  separate_wider_delim(cols = rbm_approach, 
                       delim = ", ",
                       names = c("C1","C2","C3"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "temp_col",
    values_to = "rbm_approach",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(rbm_approach != "") %>% 
  
  # remove `id` column
  select(-temp_col) %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    #response_id     = as_factor(response_id),                                
    rbm_approach  = factor(rbm_approach, 
                           levels = c("KRI's", 
                                      "KPI's", 
                                      "Team Tracking risk items",
                                      "Other"
                             )
    )) 



# Question 08  ----                    
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
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  select(response_id, everything()) %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "activity",
    values_to = "trial_attributes",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(trial_attributes != "") %>% 
  
  # separate `trial_attributes`
  separate_wider_delim(cols = trial_attributes, 
                       delim = ", ",
                       names = c("C1","C2","C3", "C4"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C4),
    names_to = "temp_col",
    values_to = "trial_attributes",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove ((Tick if YES/Leave blank if NO)
  mutate(trial_attributes = str_remove_all(string = trial_attributes, 
                                           pattern = " \\(\\(Tick if YES/Leave blank if NO\\)")) %>% 
  
  # remove `temp_col` column
  select(-temp_col) 


# |- option 1  ---- 
q8_option1 <- q8_clean %>% 
  
  # recode activity
  mutate(activity = case_when(
    activity == "identification"     ~ "Identification of CTQ’s",
    activity == "implementation"     ~ "Overall Implementation of QbD",
    activity == "utilized"           ~ "QTL’s Utilized",
    activity == "aligned"            ~ "QTL’s Aligned with CTQ’s",
    activity == "review"             ~ "QTL Review Processes",
    activity == "frequency"          ~ "Frequency of QTL Review",
    activity == "communication"      ~ "Communication of QTL Breaches",
    activity == "corrective_actions" ~ "Implementation of Corrective Actions",
    activity == "reporting"          ~ "Reporting Significant QTL Deviations in CSR"
  )) %>% 
  
  
  # truncate activity title (I want to use 2 cols for the plot)
  mutate(activity = str_trunc(activity, width = 15, side = "right")) %>%  
  
  # specify factors levels (as per questionnaire)
  mutate(
    activity  = factor(activity, 
                       levels = c("Identificati...",
                                  "Overall Impl...",
                                  "QTL’s Utilized",
                                  "QTL’s Aligne...",
                                  "QTL Review P...",
                                  "Frequency of...",
                                  "Communicatio...",
                                  "Implementati...",
                                  "Reporting Si..."
                       )
    ))
                       


# |- option 2 ---- 
q8_option2 <- q8_clean %>% 
  
  # recode activity
  mutate(activity = case_when(
    activity == "identification"     ~ "Identification of CTQ’s",
    activity == "implementation"     ~ "Overall Implementation of QbD",
    activity == "utilized"           ~ "QTL’s Utilized",
    activity == "aligned"            ~ "QTL’s Aligned with CTQ’s",
    activity == "review"             ~ "QTL Review Processes",
    activity == "frequency"          ~ "Frequency of QTL Review",
    activity == "communication"      ~ "Communication of QTL Breaches",
    activity == "corrective_actions" ~ "Implementation of Corrective Actions",
    activity == "reporting"          ~ "Reporting Significant QTL Deviations in CSR"
  )) %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    trial_attributes  = factor(trial_attributes, 
                       levels = c("Trial Design",
                                   "Trial Phase",
                                   "Trial Size",
                                   "Unable to Answer",
                                   "Mixed Response",
                                   "Not Utilized"
                        )
        ))


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
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  select(response_id, everything()) %>% 

  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "activity",
    values_to = "documentation_type",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(documentation_type != "") %>% 
  
  # separate `documentation_type`
  separate_wider_delim(cols = documentation_type, 
                       delim = ", ",
                       names = c("C1","C2","C3"),
                       too_few = "align_start") %>% 
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C3),
    names_to = "temp_col",
    values_to = "documentation_type",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove `temp_col` column
  select(-temp_col) %>% 
  
  # format documentation_type
  mutate(documentation_type = case_when(
    documentation_type == "Part of a Protocol-Level Plan(e.g.,part of a Risk Mgmt Strategy Plan)"            ~ "Part of a Protocol",
    documentation_type == "Separate Protocol- Level Plans (example Risk Mgmt Strategy Plan,Monitoring Plan)" ~ "Separate Protocol",
    documentation_type == "Via Technology Solution(s)"                                                       ~ "Technology Solution",
    TRUE                                                                                                     ~ as.character(documentation_type)
  )) %>% 
  
  # recode activity
  mutate(activity = case_when(
    activity == "identification"     ~ "Identification of CTQ’s",
    activity == "implementation"     ~ "Implementation of QbD",
    activity == "risk"               ~ "Implementation of Risk Strategy",
    activity == "utilized"           ~ "QTL’s Utilized",
    activity == "review"             ~ "QTL Review Processes",
    activity == "aligned"            ~ "QTL’s Aligned with CTQ’s",
    activity == "frequency"          ~ "Frequency of QTL Review",
    activity == "communication"      ~ "Communication of QTL Breaches",
    activity == "corrective_actions" ~ "Implementation of Corrective Actions",
    activity == "reporting"          ~ "Significant QTL Deviations in CSR"
  )) %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    documentation_type  = factor(documentation_type, 
                               levels = c("Technology Solution",
                                          "Separate Protocol",
                                          "Part of a Protocol",
                                          "Not Utilized",
                                          "Other"
                               )
    ))




# Question 23 ----
q23_clean <- q23_raw %>% 
  
    # rename columns
    rename(
      pd_ie      = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_protocol_deviation_inclusion_exclusion_criteria,
      pd_sc      = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_protocol_deviation_study_conduct,
      pd_other   = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_protocol_deviation_other,
      pea        = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_primary_endpoint_assessement,
      sea        = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_secondary_endpoint_assessment,
      ip_comp    = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_investigational_product_compliance,
      ip_other   = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_investigational_product_other,
      rf         = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_randomization_failure,
      lfu        = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_lost_to_follow_up,
      ic         = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_informed_consent,
      ae_sea_rep = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_ae_sae_reporting,
      cd         = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_censored_data_trial_participants_censored_for_primary_objective_statistical_analysis,
      disp       = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_disposition_early_termination_from_study_drug,
      rmt        = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_repeated_measures_timepoints_for_fih_early_phase_trials,
      strat      = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_stratification,
      other1     = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_other_1_please_specify_below,
      other2     = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_other_2_please_specify_below,
      other3     = x23_please_indicate_below_the_possible_potential_parameters_for_qt_ls_as_defined_by_trans_celerate_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_24_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_other_3_please_specify_below
    ) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  select(response_id, everything()) %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "parameters",
    values_to = "status",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(status != "") %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    response_id = as_factor(response_id),
    status      = factor(status, 
                         levels = c(
                           "Currently in Use",
                           "Planned to Use",
                           "Perceived Value",
                           "Not Considered a QTL"
                        )
    )) %>% 
  
  # recode `parameters` column
  mutate(parameters = case_when(
    parameters == "pd_ie"      ~ "Protocol Deviation – Inclusion/Exclusion Criteria",
    parameters == "pd_sc"      ~ "Protocol Deviation – Study Conduct",
    parameters == "pd_other"   ~ "Protocol Deviation - Other",
    parameters == "pea"        ~ "Primary Endpoint Assessment",
    parameters == "sea"        ~ "Secondary Endpoint Assessment",
    parameters == "ip_comp"    ~ "Investigational Product – Compliance",
    parameters == "ip_other"   ~ "Investigational Product - Other",
    parameters == "rf"         ~ "Randomization Failure",
    parameters == "lfu"        ~ "Lost to Follow Up",
    parameters == "ic"         ~ "Informed Consent",
    parameters == "ae_sea_rep" ~ "AE/SAE - Reporting",
    parameters == "cd"         ~ "Censored Data – Trial participants censored for primary objective statistical analysis",
    parameters == "disp"       ~ "Disposition – Early Termination from Study Drug",
    parameters == "rmt"        ~ "Repeated Measures Timepoints for FIH / Early Phase trials",
    parameters == "strat"      ~ "Stratification",
    parameters == "other1"     ~ "Other1",
    parameters == "other2"     ~ "Other2",
    parameters == "other3"     ~ "Other3",
  )) %>% 
  
  # drop NA
  filter(!is.na(status)) %>% 

  # specify factors levels (as per questionnaire)
  mutate(
    parameters = factor(parameters, 
                        levels = c(
                          "Protocol Deviation – Inclusion/Exclusion Criteria",
                          "Protocol Deviation – Study Conduct",
                          "Protocol Deviation - Other",
                          "Primary Endpoint Assessment",
                          "Secondary Endpoint Assessment",
                          "Investigational Product – Compliance",
                          "Investigational Product - Other",
                          "Randomization Failure",
                          "Lost to Follow Up",
                          "Informed Consent",
                          "AE/SAE - Reporting",
                          "Censored Data – Trial participants censored for primary objective statistical analysis",
                          "Disposition – Early Termination from Study Drug",
                          "Repeated Measures Timepoints for FIH / Early Phase trials",
                          "Stratification",
                          "Other1",
                          "Other2",
                          "Other3"
                         )
    )) 
 

# Question 24 ----
q24_clean <- q24_raw %>% 
  
  # rename columns
  rename(
    pd_ie      = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_protocol_deviation_inclusion_exclusion_criteria,
    pd_sc      = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_protocol_deviation_study_conduct,
    pd_other   = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_protocol_deviation_other,
    pea        = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_primary_endpoint_assessment,
    sea        = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_secondary_endpoint_assessment,
    ip_comp    = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_investigational_product_compliance,
    ip_other   = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_investigational_product_other,
    rf         = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_randomization_failure,
    lfu        = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_lost_to_follow_up,
    ic         = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_informed_consent,
    ae_sea_rep = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_ae_sae_reporting,
    cd         = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_censored_data_trial_participant_censored_for_primary_objectives_statistical_analysis,
    disp       = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_disposition_early_termination_from_study_drug,
    rmt        = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_repeated_measures_timepoints_for_fih_early_phase_trials,
    strat      = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_stratification,
    other1     = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_other_1,
    other2     = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_other_2,
    other3     = x24_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_other_3,
  ) %>% 
  
  # add `response_id` column
  mutate(response_id = row_number()) %>% 
  select(response_id, everything()) %>% 
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "parameters",
    values_to = "scale",
    values_drop_na = TRUE,
  ) %>% 
  
  # remove empty rows
  filter(scale != "") %>% 
  
  # remove digits from scale (1-, 2-)
  mutate(scale = str_remove_all(string = scale, pattern = "\\d+-")) %>% 
  
  # recode `parameters` column
  mutate(parameters = case_when(
    parameters == "pd_ie"      ~ "Protocol Deviation – Inclusion/Exclusion Criteria",
    parameters == "pd_sc"      ~ "Protocol Deviation – Study Conduct",
    parameters == "pd_other"   ~ "Protocol Deviation - Other",
    parameters == "pea"        ~ "Primary Endpoint Assessment",
    parameters == "sea"        ~ "Secondary Endpoint Assessment",
    parameters == "ip_comp"    ~ "Investigational Product – Compliance",
    parameters == "ip_other"   ~ "Investigational Product - Other",
    parameters == "rf"         ~ "Randomization Failure",
    parameters == "lfu"        ~ "Lost to Follow Up",
    parameters == "ic"         ~ "Informed Consent",
    parameters == "ae_sea_rep" ~ "AE/SAE - Reporting",
    parameters == "cd"         ~ "Censored Data – Trial participants censored for primary objective statistical analysis",
    parameters == "disp"       ~ "Disposition – Early Termination from Study Drug",
    parameters == "rmt"        ~ "Repeated Measures Timepoints for FIH / Early Phase trials",
    parameters == "strat"      ~ "Stratification",
    parameters == "other1"     ~ "Other1",
    parameters == "other2"     ~ "Other2",
    parameters == "other3"     ~ "Other3",
  )) %>% 
  
  # specify factors levels (as per questionnaire)
  mutate(
    response_id = as_factor(response_id),
    parameters = factor(parameters, 
                        levels = c(
                          "Protocol Deviation – Inclusion/Exclusion Criteria",
                          "Protocol Deviation – Study Conduct",
                          "Protocol Deviation - Other",
                          "Primary Endpoint Assessment",
                          "Secondary Endpoint Assessment",
                          "Investigational Product – Compliance",
                          "Investigational Product - Other",
                          "Randomization Failure",
                          "Lost to Follow Up",
                          "Informed Consent",
                          "AE/SAE - Reporting",
                          "Censored Data – Trial participants censored for primary objective statistical analysis",
                          "Disposition – Early Termination from Study Drug",
                          "Repeated Measures Timepoints for FIH / Early Phase trials",
                          "Stratification",
                          "Other1",
                          "Other2",
                          "Other3"
                          ),
                        ),
    scale = factor(scale, 
                        levels = c(
                          "Low",
                          "Medium",
                          "Medium, High",
                          "High"
                          ),
                    )
  )



