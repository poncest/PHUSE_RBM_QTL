

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
    response_id     = as_factor(response_id),
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
  




# Question 23 ----



# Question 24 ----





