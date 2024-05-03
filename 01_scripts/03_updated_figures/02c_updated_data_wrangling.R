

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2024-04-02



# 1. TIDY THE DATA ----

# Question 03 ----
q3_clean <- q3_raw |>  
  
  # rename column
  rename(functional_area = x3_which_functional_area_s_are_involved_in_trial_level_risk_based_approaches_to_quality_1_please_check_all_that_apply_1_please_see_ich_e6_r2_section_5_0_and_ich_e8_r1) |>  
  
  # select only `functional_area` col
  select(functional_area) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  
  # remove number formats (a., b.) 
  mutate(functional_area = str_remove_all(string = functional_area, pattern = "[abcdefg]\\.")) |>  
  
  # pivot longer
  pivot_longer(
    cols = c(functional_area),
    names_to = "column_id",
    values_to = "functional_area",
    values_drop_na = TRUE, 
  ) |>  

  # separate `functional_area`
  separate_wider_delim(cols = functional_area, 
                       delim = ", ", 
                       names = c("C1","C2","C3","C4","C5","C6","C7"),
                       too_few = "align_end") |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id, column_id),
    names_to = "temp_col",
    values_to = "functional_area",
    values_drop_na = TRUE, 
  ) |>  
  
  # select columns
  select(response_id, functional_area) |>  
  
  # Format `Others`
  mutate(functional_area = case_when(
    functional_area == "Other (Please specify below)" ~ "Other",
    TRUE ~ as.character(functional_area)
  )) |>  
  
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
                                        "Other")))

write_csv(x = q3_clean, 
          file = "00_data/OUT/q3_clean.csv")


# |- others ----
q3_others <- q3_raw |>  
  
  # select only `others` col
  select(other_2) |>  
  
  # rename column
  rename(other_functional_area = other_2) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  
  # separate `functional_area`
  separate_wider_delim(cols = other_functional_area, 
                       delim = ", ", 
                       names = c("C1","C2","C3","C4"),
                       too_few = "align_end") |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "temp_col",
    values_to = "other_functional_area",
    values_drop_na = TRUE, 
  ) |>  
  
  # select columns
  select(response_id, other_functional_area) |>  
  
  # trim white spaces
  mutate(other_functional_area = str_trim(other_functional_area)) |>  
  
  # format
  mutate(other_functional_area = case_when(
    other_functional_area == "Central Monitor"         ~ "Central Monitoring",
    other_functional_area == "Centralized Monitoring"  ~ "Central Monitoring",
    other_functional_area == "Saas programmers"        ~ "SaaS programmers",
    TRUE ~ as.character(other_functional_area)
  )) 

write_csv(x = q3_others, 
          file = "00_data/OUT/q3_others.csv")

  

# Question 04 ----
q4_clean <- q4_raw |>  
  
  # rename column
  rename(functional_area = x4_which_functional_area_leads_trial_level_risk_based_approaches_to_quality_please_check_only_one) |>  
  
  # select only `functional_area` col
  select(functional_area) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  
  # remove number formats (a., b.) 
  mutate(functional_area = str_remove_all(string = functional_area, pattern = "[abcdefg]\\.")) |>  
  
  # separate `functional_area`
  separate_wider_delim(cols = functional_area, 
                       delim = ", ", 
                       names = c("C1","C2","C3","C4","C5","C6","C7"),
                       too_few = "align_end") |>  

  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "temp_col",
    values_to = "functional_area",
    values_drop_na = TRUE, 
  ) |>  
  
  # select columns
  select(response_id, functional_area) |>  
  
  # Format `Others`
  mutate(functional_area = case_when(
    functional_area == "Other (Please specify below)" ~ "Other",
    TRUE ~ as.character(functional_area)
  )) |>  
  
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
                                        "Other")))

write_csv(x = q4_clean, 
          file = "00_data/OUT/q4_clean.csv")


# |- others ----
q4_others <- q4_raw |>  
  
  # rename column
  rename(functional_area = x4_which_functional_area_leads_trial_level_risk_based_approaches_to_quality_please_check_only_one,
         other_functional_area = other_3) |>  
  
  # select only `others` col
  select(other_functional_area) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  
  # select columns
  select(response_id, other_functional_area) |>  
  
  # consolidate `other` category
  mutate(other_functional_area = case_when(
    other_functional_area == "Centralized Mointoring"                                      ~ "Central Monitoring",
    other_functional_area == "Dedicated RBQM lead or Risk Manager"                         ~ "Risk Manager",
    other_functional_area == "Dedicated Risk Managment Specialist"                         ~ "Risk Manager",
    other_functional_area == "Each functional area has an oversight team to assess risk."  ~ "Oversight Team",
    other_functional_area == "the study team have an RBQM working group and they decide who takes the lead in the group, generally its Operations but can be one of the other functions like biostats or clinical science "  ~ "RBQM working group",
    TRUE ~ as.character(other_functional_area)
  )) |>  
  
  # factors
  mutate(response_id = as_factor(response_id))

write_csv(x = q4_others, 
          file = "00_data/OUT/q4_others.csv")


# Question 05 ----
q5_clean <- q5_raw |>  
  
  # rename column
  rename(trial_types = x5_are_there_any_trial_types_where_your_company_does_not_apply_risk_based_approaches_to_quality_please_select_all_that_apply_and_provide_comments_as_to_why_this_is_the_case) |>  
  
  # select only `trial_types` col 
  select(trial_types) |>   
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  
  # remove number formats (a., b.) 
  mutate(trial_types = str_remove_all(string = trial_types, pattern = "[abcdefg]\\."))  |>  
  
  # replace ", " with " | " in `Others`
  # so it doesn't separate in the step below
  mutate(
    trial_types = str_replace_all(string = trial_types,
                                  pattern     = "Registry, non-interventional",
                                  replacement = "Registry | non-interventional")
  ) |>  
  
  # separate `trial_types`
  separate_wider_delim(cols = trial_types, 
                       delim = ", ", 
                       names = c("C1","C2","C3"),
                       too_few = "align_end")  |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "temp_col",
    values_to = "trial_types",
    values_drop_na = TRUE, 
  ) |>  
  
  # format
  mutate(trial_types = case_when(
    trial_types == "Other (Registry | non-interventional)"     ~ "Other",
    trial_types == "Comment (Please comment why below)"        ~ "Comment why",
    trial_types == "Post-marketing approval (interventional)"  ~ "Post-marketing approval",
    TRUE ~ as.character(trial_types)
  )) |>  
  
  # select columns
  select(response_id, trial_types) |>  
  
  # filter out `comment why` 
  filter(trial_types != "Comment why") |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    #response_id = as_factor(response_id),
    trial_types = factor(trial_types,
                         levels = c("Phase I",
                                    "Phase II",
                                    "Biostatistics",
                                    "Complex Design",
                                    "Post-marketing approval",
                                    "Other")))

write_csv(x = q5_clean, 
          file = "00_data/OUT/q5_clean.csv")



# |- others ----
q5_others <- q5_raw |>  
  
  # rename column
  rename(trial_types = x5_are_there_any_trial_types_where_your_company_does_not_apply_risk_based_approaches_to_quality_please_select_all_that_apply_and_provide_comments_as_to_why_this_is_the_case) |>  
  
  # select only `comments` col 
  select(comment_why) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  
  # select columns
  select(response_id, comment_why) |>  
  
  # format
  mutate(comment_why = str_to_sentence(comment_why))


write_csv(x = q5_others, 
          file = "00_data/OUT/q5_others.csv")



# Question 06 ----                    
q6_clean <- q6_raw |>  
  
  # rename columns
  rename(
    fih    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_fih,
    ph1    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_i_non_fih,
    ph2    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_ii,
    ph3_rs = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iii_regulatory_submission,
    ph3_fu = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iii_follow_up,
    ph4    = x6_which_aspects_of_risk_based_approaches_to_quality_does_your_company_apply_to_the_following_trial_types_please_check_all_that_apply_phase_iv
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "stage_phase",
    values_to = "rbm_approaches",
    values_drop_na = TRUE,
  ) |>  
  
  # remove empty rows
  filter(rbm_approaches != "") |>  
  
  # separate `rbm_approaches`
  separate_wider_delim(cols = rbm_approaches, 
                       delim = ", ",
                       names = c("C1","C2","C3", "C4","C5"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C5),
    names_to = "temp_col",
    values_to = "rbm_approaches",
    values_drop_na = TRUE,
  ) |>  
  
  # remove `id` column
  select(-temp_col) |>  
  
  # remove parenthesis (1), (2), and (Check for YES or leave blank for NO)
  mutate(
    rbm_approaches = str_remove_all(string = rbm_approaches, pattern = "\\(\\d+\\)"), # remove (1), (2)
    rbm_approaches = str_remove_all(string = rbm_approaches, pattern = " \\(Check for YES or leave blank for NO\\)"),
    ) |>  
  
  # format
  mutate(rbm_approaches = case_when(
    rbm_approaches == "CTQ Factors"                   ~ "CtQ Factors" ,
    rbm_approaches == "QbD Processes"                 ~ "QbD Processes",
    rbm_approaches == "QTL'S"                         ~ "QTLs",
    rbm_approaches == "Alignment of QTL'S with CTQ'S" ~ "Alignment of QTLs with CtQs",
    rbm_approaches == "Other Risk-Based Approaches"   ~ "Other Risk-Based Approaches",
    TRUE ~ as.character(rbm_approaches)
  )) |>   
  
  
  # specify factors levels (as per questionnaire)
  mutate(
    #response_id     = as_factor(response_id),                                
    rbm_approaches  = factor(rbm_approaches, 
                             levels = c("CtQ Factors", 
                                        "QbD Processes", 
                                        "QTLs",
                                        "Alignment of QTLs with CtQs", 
                                        "Other Risk-Based Approaches"
                                        )
    )) |>  
  
  # recode `stage_phase` column
  mutate(stage_phase = case_when(
    stage_phase == "fih"    ~ "FIH",
    stage_phase == "ph1"    ~ "Phase I Non–FIH",
    stage_phase == "ph2"    ~ "Phase II",
    stage_phase == "ph3_rs" ~ "Phase III – RS",
    stage_phase == "ph3_fu" ~ "Phase III – FU",
    stage_phase == "ph4"    ~ "Phase IV",
  )) 
  

write_csv(x = q6_clean, 
          file = "00_data/OUT/q6_clean.csv")


# Question 07 ----     

### |- UPDATED -----

q7_clean <- q7_raw |>  
  
  # rename columns
  rename(
   rbm_approach = x7_if_you_answered_yes_to_other_risk_based_approaches_used_in_question_6_please_identify_those_used_please_check_all_that_apply
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # remove number formats (a., b.) 
  mutate(rbm_approach = str_remove_all(string = rbm_approach, pattern = "[abcdefg]\\.")) |>  
  
  # separate `rbm_approach`
  separate_wider_delim(cols = rbm_approach, 
                       delim = ", ",
                       names = c("C1","C2","C3"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "temp_col",
    values_to = "rbm_approach",
    values_drop_na = TRUE,
  ) |>  
  
  # remove empty rows
  filter(rbm_approach != "") |>  
  
  # remove `id` column
  select(-temp_col) |>  
  
  # recode activity
  mutate(rbm_approach = case_when(
    rbm_approach == "KRI's"                    ~ "KRIs",
    rbm_approach == "KPI's"                    ~ "KPIs",
    rbm_approach == "Team Tracking risk items" ~ "Team Tracking Risk Items",
    rbm_approach == "Other"                    ~ "Other", 
  )) |>  
  
  
  # specify factors levels (as per questionnaire)
  mutate(
    #response_id     = as_factor(response_id),                                
    rbm_approach  = factor(rbm_approach, 
                           levels = c("KRIs", 
                                      "KPIs", 
                                      "Team Tracking Risk Items",    
                                      "Other"
                             ))) 

write_csv(x = q7_clean, 
          file = "00_data/OUT/q7_clean.csv")



# Question 08  ----    

### |- UPDATED -----

q8_clean <- q8_raw |>  
  
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
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "activity",
    values_to = "trial_attributes",
    values_drop_na = TRUE,
  ) |>  
  
  # remove empty rows
  filter(trial_attributes != "") |>  
  
  # separate `trial_attributes`
  separate_wider_delim(cols = trial_attributes, 
                       delim = ", ",
                       names = c("C1","C2","C3", "C4"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C4),
    names_to = "temp_col",
    values_to = "trial_attributes",
    values_drop_na = TRUE,
  ) |>  
  
  # remove ((Tick if YES/Leave blank if NO)
  mutate(trial_attributes = str_remove_all(string = trial_attributes, 
                                           pattern = " \\(\\(Tick if YES/Leave blank if NO\\)")) |>  
  
  # remove `temp_col` column
  select(-temp_col) 


write_csv(x = q8_clean, 
          file = "00_data/OUT/q8_clean.csv")



# # |- option 1  ---- 
# q8_option1 <- q8_clean |>  
#   
#   # recode activity
#   mutate(activity = case_when(
#     activity == "identification"     ~ "Identification of CTQ’s",
#     activity == "implementation"     ~ "Overall Implementation of QbD",
#     activity == "utilized"           ~ "QTL’s Utilized",
#     activity == "aligned"            ~ "QTL’s Aligned with CTQ’s",
#     activity == "review"             ~ "QTL Review Processes",
#     activity == "frequency"          ~ "Frequency of QTL Review",
#     activity == "communication"      ~ "Communication of QTL Breaches",
#     activity == "corrective_actions" ~ "Implementation of Corrective Actions",
#     activity == "reporting"          ~ "Reporting Significant QTL Deviations in CSR"
#   )) |>  
#   
#   
#   # truncate activity title (I want to use 2 cols for the plot)
#   mutate(activity = str_trunc(activity, width = 15, side = "right")) |>   
#   
#   # specify factors levels (as per questionnaire)
#   mutate(
#     activity  = factor(activity, 
#                        levels = c("Identificati...",
#                                   "Overall Impl...",
#                                   "QTL’s Utilized",
#                                   "QTL’s Aligne...",
#                                   "QTL Review P...",
#                                   "Frequency of...",
#                                   "Communicatio...",
#                                   "Implementati...",
#                                   "Reporting Si..."
#                        )))
# 
# 
# write_csv(x = q8_option1, 
#           file = "00_data/OUT/q8_option1.csv")
                       


# |- option 2 ----      

### |- UPDATED -----

q8_option2 <- q8_clean |>  
  
  # recode activity
  mutate(activity = case_when(
    activity == "identification"     ~ "Identification of CtQs",
    activity == "implementation"     ~ "Overall Implementation of QbD",
    activity == "utilized"           ~ "QTLs Utilized",
    activity == "aligned"            ~ "QTLs Aligned with CtQ’s",
    activity == "review"             ~ "QTLs Review Processes",
    activity == "frequency"          ~ "Frequency of QTLs Review",
    activity == "communication"      ~ "Communication of QTLs Breaches",
    activity == "corrective_actions" ~ "Implementation of Corrective Actions",
    activity == "reporting"          ~ "Reporting Significant QTLs Deviations in CSRs"
  )) |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    trial_attributes  = factor(trial_attributes, 
                       levels = c("Trial Design",
                                   "Trial Phase",
                                   "Trial Size",
                                   "Unable to Answer",
                                   "Mixed Response",
                                   "Not Utilized"
                        )))


write_csv(x = q8_option2, 
          file = "00_data/OUT/q8_option2.csv")



# Question 10 ----
q10_clean <- q10_raw |>  
  
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
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  

  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "activity",
    values_to = "documentation_type",
    values_drop_na = TRUE,
  ) |>  
  
  # remove empty rows
  filter(documentation_type != "") |>  
  
  # separate `documentation_type`
  separate_wider_delim(cols = documentation_type, 
                       delim = ", ",
                       names = c("C1","C2","C3"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C3),
    names_to = "temp_col",
    values_to = "documentation_type",
    values_drop_na = TRUE,
  ) |>  
  
  # remove `temp_col` column
  select(-temp_col) |>  
  
  # format documentation_type
  mutate(documentation_type = case_when(
    documentation_type == "Part of a Protocol-Level Plan(e.g.,part of a Risk Mgmt Strategy Plan)"            ~ "Part of a Protocol",
    documentation_type == "Separate Protocol- Level Plans (example Risk Mgmt Strategy Plan,Monitoring Plan)" ~ "Separate Protocol",
    documentation_type == "Via Technology Solution(s)"                                                       ~ "Technology Solution",
    TRUE                                                                                                     ~ as.character(documentation_type)
  )) |>  
  
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
  )) |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    documentation_type  = factor(documentation_type, 
                               levels = c("Technology Solution",
                                          "Separate Protocol",
                                          "Part of a Protocol",
                                          "Not Utilized",
                                          "Other"
                               )))


write_csv(x = q10_clean, 
          file = "00_data/OUT/q10_clean.csv")



# Question 18 ----
q18_clean <- q18_raw |>  
  
  # rename columns
  rename(
    health      = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_formally_integrated_in_qms_health_authority_inspection,
    continous   = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_formally_integrated_in_qms_continuous_improvement_e_g_future_protocol_development_process_improvement_activities,
    submissions = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_formally_integrated_in_qms_submissions,
    management  = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_formally_integrated_in_qms_senior_management_review,
    oversight   = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_formally_integrated_in_qms_quality_management_oversight_e_g_gcp_quality_councils_committees,
    design      = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_formally_integrated_with_quality_by_design_processes,
    quality     = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_formally_integrated_with_critical_to_quality_processes,
    other       = x18_how_are_qtl_s_governed_utilized_at_your_company_please_check_all_that_apply_other_please_specify_below,
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "characteristic",
    values_to = "qtl_utilization",
    values_drop_na = TRUE,
  ) |>  
  
  # remove empty rows
  filter(qtl_utilization != "") |>  
  
  # separate `qtl_utilization`
  separate_wider_delim(cols = qtl_utilization, 
                       delim = ", ",
                       names = c("C1","C2"),
                       too_few = "align_start")  |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C2),
    names_to = "temp_col",
    values_to = "qtl_utilization",
    values_drop_na = TRUE,
  ) |>  
  
  # remove `temp_col` column
  select(-temp_col) |>  
  
  # recode characteristic
  mutate(characteristic = case_when(
    characteristic == "health"       ~ "QMS – Health Authority Inspection",
    characteristic == "continous"    ~ "QMS – Continuous Improvement",
    characteristic == "submissions"  ~ "QMS - Submissions",
    characteristic == "management"   ~ "QMS – Senior Management Review",
    characteristic == "oversight"    ~ "QMS – Quality Management Oversight ",
    characteristic == "design"       ~ "Quality by Design Processes",
    characteristic == "quality"      ~ "Critical to Quality Processes",
    characteristic == "other"        ~ "Other"
    )) |>    
  
  # specify factors levels (as per questionnaire)
  mutate(
    qtl_utilization = factor(qtl_utilization, 
                             levels = c("In Use",
                                        "Planned",
                                        "Not In Use and Not Planned",
                                        "Not Known",
                                        "Comments")))


write_csv(x = q18_clean, 
          file = "00_data/OUT/q18_clean.csv")



# Question 19 ----

### |- UPDATED -----

q19_clean <- q19_raw |>  
  
  # rename columns
  rename(
    protocol     = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_protocol_amendment,
    threshold    = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_change_in_qtl_threshold_s_resetting,
    parameter    = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_change_in_qtl_parameter_metric,
    distribution = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_change_in_qtl_statistical_distribution,
    methodology  = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_change_in_qtl_statistical_methodology,
    monitoring   = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_change_to_trial_monitoring_strategy,
    sites        = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_instruction_s_provided_to_sites,
    vendors      = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_instruction_s_provided_to_vendors_service_providers_or_otherwise,
    others       = x19_what_types_of_actions_has_your_company_typically_taken_or_plan_to_take_in_response_to_a_qtl_breach_please_provide_one_response_per_possible_action_other_please_specify_below,
    comments     = your_response_and_possible_action_comments
  ) |>  
  
  # deselect comments column
  select(-comments) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>   
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "possible_action",
    values_to = "action_type",
    values_drop_na = TRUE,
  ) |>  
  
  # remove empty rows
  filter(action_type != "") |>  
  
  # separate `action_type`
  separate_wider_delim(cols = action_type, 
                       delim = ", ",
                       names = c("C1","C2","C3"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C3),
    names_to = "temp_col",
    values_to = "action_type",
    values_drop_na = TRUE,
  )  |>  
  
  # remove `temp_col` column
  select(-temp_col) |>  
  
  # format 
  mutate(action_type = case_when(
    action_type == "Not Applicable(e.g.,Do not use this action)" ~ "Not Applicable",
    action_type == "For Primary Breach"                          ~ "Primary Breach",
    action_type == "For Secondary Breach"                        ~ "Secondary Breach",
    TRUE                                                         ~ as.character(action_type)
  )) |>  

  # recode possible_action
  mutate(possible_action = case_when(
    possible_action == "protocol"     ~ "Protocol Amendment",
    possible_action == "threshold"    ~ "Change in QTL Threshold",
    possible_action == "parameter"    ~ "Change in QTL Parameter Metric",
    possible_action == "distribution" ~ "Change in QTL Statistical Distribution",
    possible_action == "methodology"  ~ "Change in QTL Statistical Methodology",
    possible_action == "monitoring"   ~ "Change in Trial Monitoring Strategy",
    possible_action == "sites"        ~ "Instructions Provided to Sites",
    possible_action == "vendors"      ~ "Instructions Provided to Vendors",
    possible_action == "others"       ~ "Other"
  )) |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    action_type = factor(action_type, 
                             levels = c("Primary Breach",
                                        "Secondary Breach",
                                        "Not Applicable")))


write_csv(x = q19_clean, 
          file = "00_data/OUT/q19_clean.csv")


# |- others ----
q19_comments <- q19_raw |>  
  
  # rename columns
  rename(comments     = your_response_and_possible_action_comments) |>  
  
  # select comments column
  select(comments) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) 



write_csv(x = q19_comments, 
          file = "00_data/OUT/q19_comments.csv")




# Question 20.3 ----

### |- UPDATED -----

q20.3_clean <- q20.3_raw |>  
  
  # rename columns
  rename(
    csr   = x20_3_if_qtl_primary_limit_breaches_are_evaluated_for_importance_how_do_you_determine_as_to_whether_they_should_be_reported_in_the_csr,
    other = other_6
  ) |>  
  
  # deselect Others column
  select(-other) |>  

  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # separate `action_type`
  separate_wider_delim(cols = csr, 
                       delim = ", ",
                       names = c("C1","C2","C3","C4"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C4),
    names_to = "temp_col",
    values_to = "csr",
    values_drop_na = TRUE,
  )  |>  
  
  # remove `temp_col` column
  select(-temp_col) |>  
  
  # remove empty rows
  filter(csr != "") |>  
  
  # format 
  mutate(
    csr = case_when(
      csr == "Other (Please specify below)" ~ "Other",
      TRUE                                  ~ as.character(csr)
      )) |> 
  
  # format
  mutate(
    csr         = str_to_sentence(csr),
    #response_id = as_factor(response_id)
  ) |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    csr = factor(csr,
                 levels = c("Against pre-defined criteria",
                            "Via statistical methodology",
                            "By study team",
                            "Unknown",
                            "Other")))


write_csv(x = q20.3_clean, 
          file = "00_data/OUT/q20.3_clean.csv")


# |- others ----
q20.3_other <- q20.3_raw |>  
  
  # rename columns
  rename(
    csr   = x20_3_if_qtl_primary_limit_breaches_are_evaluated_for_importance_how_do_you_determine_as_to_whether_they_should_be_reported_in_the_csr,
    other = other_6
  ) |>  
  
  # select other column
  select(other) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) 


write_csv(x = q20.3_other, 
          file = "00_data/OUT/q20.3_other.csv")

  
# Question 23 ----

# |- status ----

### |- UPDATED -----

q23_clean <- q23_raw |>  
  
  # remove comments columns
  select(-c(other_1 , other_2_2)) |>  
  
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
    ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "transcelerate_parameters",
    values_to = "status",
    values_drop_na = TRUE,
  ) |>  
  
  # separate `status`
  separate_wider_delim(cols = status, 
                       delim = ", ",
                       names = c("C1","C2"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C2),
    names_to = "temp_col",
    values_to = "status",
    values_drop_na = TRUE,
  )  |>  
  
  # remove empty rows
  filter(status != "") |>  
  
  # remove `temp_col` column
  select(-temp_col) |>  
  
  # recode `parameters` column
  mutate(transcelerate_parameters = case_when(
    transcelerate_parameters == "pd_ie"      ~ "PD – I/E Criteria",
    transcelerate_parameters == "pd_sc"      ~ "PD – Study Conduct",
    transcelerate_parameters == "pd_other"   ~ "PD – Other",
    transcelerate_parameters == "pea"        ~ "Primary Endpoint Assessment",
    transcelerate_parameters == "sea"        ~ "Secondary Endpoint Assessment",
    transcelerate_parameters == "ip_comp"    ~ "Investigational Product – Compliance",
    transcelerate_parameters == "ip_other"   ~ "Investigational Product – Other",
    transcelerate_parameters == "rf"         ~ "Randomization Failure",
    transcelerate_parameters == "lfu"        ~ "Lost to Follow Up",
    transcelerate_parameters == "ic"         ~ "Informed Consent",
    transcelerate_parameters == "ae_sea_rep" ~ "AE/SAE – Reporting",
    transcelerate_parameters == "cd"         ~ "Censored Data – Statistical Analysis",
    transcelerate_parameters == "disp"       ~ "Disposition – Early Termination",
    transcelerate_parameters == "rmt"        ~ "Repeated Timepoints for FIH/Early Phase Trials",
    transcelerate_parameters == "strat"      ~ "Stratification",
    transcelerate_parameters == "other1"     ~ "Other1",
    transcelerate_parameters == "other2"     ~ "Other2",
    transcelerate_parameters == "other3"     ~ "Other3",
  )) |>  

  # specify factors levels (as per questionnaire)
  mutate(
    transcelerate_parameters = factor(transcelerate_parameters, 
                                      levels = c(
                                        "PD – I/E Criteria",
                                        "PD – Study Conduct",
                                        "PD – Other",
                                        "Primary Endpoint Assessment",
                                        "Secondary Endpoint Assessment",
                                        "Investigational Product – Compliance",
                                        "Investigational Product – Other",
                                        "Randomization Failure",
                                        "Lost to Follow Up",
                                        "Informed Consent",
                                        "AE/SAE – Reporting",
                                        "Censored Data – Statistical Analysis", 
                                        "Disposition – Early Termination",      
                                        "Repeated Timepoints for FIH/Early Phase Trials",
                                        "Stratification",
                                        "Other1",
                                        "Other2",
                                        "Other3"
                                       )),
    status = factor(status, 
                   levels = c(
                     "Currently in Use",
                     "Planned to Use",
                     "Under Consideration",
                     "Not Considered a QTL"
                   ))) 



write_csv(x = q23_clean, 
          file = "00_data/OUT/q23_clean.csv")
 


# |- others ----
q23_comments <- q23_raw  |>  
  
  # rename columns
  rename(
    comments_1 = other_1,
    comments_2 = other_2_2
    ) |>  
  
  # select comments column
  select(starts_with("comments_")) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # format
  mutate(
    comments_1 = str_to_sentence(comments_1),
    comments_2 = str_to_sentence(comments_2)
  )



write_csv(x = q23_comments, 
          file = "00_data/OUT/q23_comments.csv")


# |- rate ----

### |- UPDATED -----

q23_rate_clean <- q23_rate_raw |>  
  
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
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "transcelerate_parameters",
    values_to = "rate_scale",
    values_drop_na = TRUE,
  ) |>  
  
  # remove `digit-` (1-, 2-, 3-)
  mutate(rate_scale = str_remove_all(string = rate_scale, pattern = "\\d+-")) |>  
  
  # separate `status`
  separate_wider_delim(cols = rate_scale,
                       delim = ", ",
                       names = c("C1","C2"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C2),
    names_to = "temp_col",
    values_to = "rate_scale",
    values_drop_na = TRUE,
  )  |> 
  
  # remove empty rows
  filter(rate_scale != "") |>  
  
  # remove `temp_col` column
  select(-temp_col) |>  
  
  # recode `parameters` column
  mutate(transcelerate_parameters = case_when(
    transcelerate_parameters == "pd_ie"      ~ "PD – I/E Criteria",
    transcelerate_parameters == "pd_sc"      ~ "PD – Study Conduct",
    transcelerate_parameters == "pd_other"   ~ "PD – Other",
    transcelerate_parameters == "pea"        ~ "Primary Endpoint Assessment",
    transcelerate_parameters == "sea"        ~ "Secondary Endpoint Assessment",
    transcelerate_parameters == "ip_comp"    ~ "Investigational Product – Compliance",
    transcelerate_parameters == "ip_other"   ~ "Investigational Product – Other",
    transcelerate_parameters == "rf"         ~ "Randomization Failure",
    transcelerate_parameters == "lfu"        ~ "Lost to Follow Up",
    transcelerate_parameters == "ic"         ~ "Informed Consent",
    transcelerate_parameters == "ae_sea_rep" ~ "AE/SAE – Reporting",
    transcelerate_parameters == "cd"         ~ "Censored Data – Statistical Analysis",
    transcelerate_parameters == "disp"       ~ "Disposition – Early Termination",
    transcelerate_parameters == "rmt"        ~ "Repeated Timepoints for FIH/Early Phase Trials",
    transcelerate_parameters == "strat"      ~ "Stratification",
    transcelerate_parameters == "other1"     ~ "Other1",
    transcelerate_parameters == "other2"     ~ "Other2",
    transcelerate_parameters == "other3"     ~ "Other3",
  )) |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    transcelerate_parameters = factor(transcelerate_parameters, 
                                      levels = c(
                                        "PD – I/E Criteria",
                                        "PD – Study Conduct",
                                        "PD – Other",
                                        "Primary Endpoint Assessment",
                                        "Secondary Endpoint Assessment",
                                        "Investigational Product – Compliance",
                                        "Investigational Product – Other",
                                        "Randomization Failure",
                                        "Lost to Follow Up",
                                        "Informed Consent",
                                        "AE/SAE – Reporting",
                                        "Censored Data – Statistical Analysis", 
                                        "Disposition – Early Termination",      
                                        "Repeated Timepoints for FIH/Early Phase Trials",
                                        "Stratification",
                                        "Other1",
                                        "Other2",
                                        "Other3"
                                      )),
    rate_scale = factor(rate_scale, 
                        levels = c(
                          "Low",
                          "Medium",
                          "High"
                        ))) |>  
  
  # counts by parameters and rate
  group_by(transcelerate_parameters, rate_scale) |>  
  summarise(
    count   = n(),
    .groups = "drop",
  ) |>  
  ungroup() |>  
  
  # calculate percent by each parameter
  group_by(transcelerate_parameters) |>  
  mutate(
    pct = count / sum(count),
    total_count = sum(count)
  ) |>  
  ungroup() |>  
  
  # arrange
  arrange(desc(count)) |>  
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ transcelerate_parameters } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  ) 
  

write_csv(x = q23_rate_clean, 
          file = "00_data/OUT/q23_rate_clean.csv")


  
# Question 24 ----

### |- UPDATED -----

q24_clean <- q24_raw |>  
  
  # rename columns
  rename(
    crf          = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_crf_transcription_errors,
    completeness = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_tmf_completeness,
    quality      = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_tmf_quality,
    vendor       = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_vendor_oversight,
    ae_sae       = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_ae_sae_management,
    entry        = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_data_entry_timeliness,
    processing   = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_data_processing_querying,
    asset        = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_asset_compound_specific,
    ta           = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_therapeutic_area_specific,
    is           = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_indication_specific,
    ps           = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_protocol_specific,
    other_1      = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_other_1_please_specify_below,
    other_2      = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_other_2_please_specify_below,
    other_3      = x25_please_indicate_if_any_of_the_below_additional_parameters_are_also_considered_as_parameters_for_qt_ls_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_question_26_disclaimer_this_is_not_considered_a_definitive_list_of_potential_parameters_and_may_be_interpreted_differently_between_responders_other_3_please_specify_below,
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "additional_parameters",
    values_to = "status",
    values_drop_na = TRUE,
  ) |>  
  
  # remove empty rows
  filter(status != "") |>  
  
  # separate `status`
  separate_wider_delim(cols = status, 
                       delim = ", ",
                       names = c("C1","C2"),
                       too_few = "align_start") |>  
  
  # pivot longer
  pivot_longer(
    cols = c(C1:C2),
    names_to = "temp_col",
    values_to = "status",
    values_drop_na = TRUE,
  )  |>  
  
  # remove `temp_col` column
  select(-temp_col) |>  
  
  # recode `parameters` column
  mutate(additional_parameters = case_when(
    additional_parameters == "crf"          ~ "CRF Transcription Errors",
    additional_parameters == "completeness" ~ "TMF – Completeness",
    additional_parameters == "quality"      ~ "TMF – Quality",
    additional_parameters == "vendor"       ~ "Vendor Oversight",
    additional_parameters == "ae_sae"       ~ "AE/SAE – Management",
    additional_parameters == "entry"        ~ "Data Entry Timeliness",
    additional_parameters == "processing"   ~ "Data Processing – Querying",
    additional_parameters == "asset"        ~ "Asset/Compound Specific",
    additional_parameters == "ta"           ~ "Therapeutic Area Specific",
    additional_parameters == "is"           ~ "Indication Specific",
    additional_parameters == "ps"           ~ "Protocol Specific",
    additional_parameters == "other_1"      ~ "Other1",
    additional_parameters == "other_2"      ~ "Other2",
    additional_parameters == "other_3"      ~ "Other3"
  )) |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    additional_parameters = factor(additional_parameters, 
                            levels = c(
                              "CRF Transcription Errors",
                              "TMF – Completeness",
                              "TMF – Quality",
                              "Vendor Oversight",
                              "AE/SAE – Management",
                              "Data Entry Timeliness",
                              "Data Processing – Querying",
                              "Asset/Compound Specific",
                              "Therapeutic Area Specific",
                              "Indication Specific",
                              "Protocol Specific",
                              "Other1",
                              "Other2",
                              "Other3"
                              )),
    status = factor(status, 
                        levels = c(
                          "Currently in Use",
                          "Planned to Use",
                          "Under Consideration",
                          "Not Considered a QTL"
                          )))


write_csv(x = q24_clean, 
          file = "00_data/OUT/q24_clean.csv")


# |- others ----
q24_comments <- q24_raw  |>  
  
  # select other column
  select(other_1_2) |>  
  
  # rename columns
  rename(comments = other_1_2) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything())  
  

write_csv(x = q24_comments, 
          file = "00_data/OUT/q24_comments.csv")



# |- rate ----

### |- UPDATED -----

q24_rate_clean <- q24_rate_raw |>  
  
  # rename columns
  rename(
    crf          = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_crf_transcription_errors,
    completeness = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_tmf_completeness,
    quality      = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_tmf_quality,
    vendor       = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_vendor_oversight,
    ae_sae       = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_ae_sae_management,
    entry        = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_data_entry_timeliness,
    processing   = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_data_processing_querying,
    asset        = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_asset_compound_specific,
    ta           = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_therapeutic_area_specific,
    is           = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_indication_specific,
    ps           = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_protocol_specific,
    other_1      = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_other_1,
    other_2      = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_other_2,
    other_3      = x26_please_rate_on_a_scale_of_1_low_2_medium_or_3_high_the_perceived_value_of_the_parameter_used_other_3
  ) |>  
  
  # add `response_id` column
  mutate(response_id = row_number()) |>  
  select(response_id, everything()) |>  
  
  # pivot longer
  pivot_longer(
    cols = -c(response_id),
    names_to = "additional_parameters",
    values_to = "rate_scale",
    values_drop_na = TRUE,
  ) |>  
  
  # remove `digit-` (1-, 2-, 3-)
  mutate(rate_scale = str_remove_all(string = rate_scale, pattern = "\\d+-")) |>  
  
  # remove empty rows
  filter(rate_scale != "") |>  
  
  # recode `parameters` column
  mutate(additional_parameters = case_when(
    additional_parameters == "crf"          ~ "CRF Transcription Errors",
    additional_parameters == "completeness" ~ "TMF – Completeness",
    additional_parameters == "quality"      ~ "TMF – Quality",
    additional_parameters == "vendor"       ~ "Vendor Oversight",
    additional_parameters == "ae_sae"       ~ "AE/SAE – Management",
    additional_parameters == "entry"        ~ "Data Entry Timeliness",
    additional_parameters == "processing"   ~ "Data Processing – Querying",
    additional_parameters == "asset"        ~ "Asset/Compound Specific",
    additional_parameters == "ta"           ~ "Therapeutic Area Specific",
    additional_parameters == "is"           ~ "Indication Specific",
    additional_parameters == "ps"           ~ "Protocol Specific",
    additional_parameters == "other_1"      ~ "Other1",
    additional_parameters == "other_2"      ~ "Other2",
    additional_parameters == "other_3"      ~ "Other3"
  )) |>  
  
  # specify factors levels (as per questionnaire)
  mutate(
    additional_parameters = factor(additional_parameters, 
                                   levels = c(
                                     "CRF Transcription Errors",
                                     "TMF – Completeness",
                                     "TMF – Quality",
                                     "Vendor Oversight",
                                     "AE/SAE – Management",
                                     "Data Entry Timeliness",
                                     "Data Processing – Querying",
                                     "Asset/Compound Specific",
                                     "Therapeutic Area Specific",
                                     "Indication Specific",
                                     "Protocol Specific",
                                     "Other1",
                                     "Other2",
                                     "Other3"
                                   )),
    rate_scale = factor(rate_scale, 
                        levels = c(
                          "Low",
                          "Medium",
                          "High"
                        ))) |>   
  
  # counts by parameters and rate
  group_by(additional_parameters, rate_scale) |>  
  
  summarise(
    count   = n(),
    .groups = "drop",
  ) |>  
  ungroup() |>  
  
  # calculate percent by each parameter
  group_by(additional_parameters) |>  
  mutate(
    pct = count / sum(count),
    total_count = sum(count)
  ) |>  
  ungroup() |>  
  
  # arrange
  arrange(desc(count)) |>  
  
  # add labels
  mutate(
    bar_axis  = str_glue("{ additional_parameters } ({ count })"),
    bar_label = str_glue("{ scales::percent(pct, accuracy = 1) }")
  ) 

write_csv(x = q24_rate_clean, 
          file = "00_data/OUT/q24_rate_clean.csv")

