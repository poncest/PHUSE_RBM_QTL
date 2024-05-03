

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2024-04-02



# 1. VISUALIZATION ----

# |- figure size ----
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 5,
  units  = "in",
  dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)

### |- plot aesthetics ---- 
bkg_col         <- 'white' 
text_col        <- "gray20"
title_col       <- "gray20"

### |-  fonts ----
font_add('00_fonts/Inter-Regular.ttf', family = "title") 
font_add('00_fonts/Inter-Regular.ttf', family = "text") 
showtext_auto(enable = TRUE)  



# Question 03 ----
plot03 <- q3_clean |>  
  
  ggplot(aes(x = response_id, y = functional_area, fill = functional_area)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.6,
                    shape       = 22) +


  # scale
  scale_x_discrete() +                                
  scale_y_discrete(expand = c(0.01, 0))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  # labs
  labs(
    x = "Response Number",
    y = "Functional Area",
    title = "Q3: Which Functional Area(s) are involved in trial level Risk-Based Approaches to Quality?"
  )+
  
  # theme
  custom_theme()
  

# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot03.png", device = "png", plot = plot03,   
       width = 6, height = 4, units = 'in', dpi = 320)  



# Question 04 ----
plot04 <- q4_clean |>  
  
  ggplot(aes(x = response_id, y = functional_area, fill = functional_area)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.6,
                    shape       = 22) +
  
  
  # scale
  scale_x_discrete() +                                
  scale_y_discrete(expand = c(0.01, 0))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  # labs
  labs(
    x = "Response Number",
    y = "Functional Area",
    title = "Q4: Which Functional Area leads trial level Risk-Based Approaches to Quality?"
  )+
  
  # theme
  custom_theme()

  

# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot04.png", device = "png", plot = plot04,   
       width = 6, height = 4, units = 'in', dpi = 320)  



# Question 05 ----
# |- figure size ----
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 3,
  units  = "in",
  dpi    = 320) 


plot05 <- q5_clean |> 
  
  ggplot(aes(x = response_id, y = trial_types, fill = trial_types)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.3,
                    shape       = 22) +
  
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  # labs
  labs(
    x = "Response Number",
    y = "Trial types",
    title = "Q5: What are the trial types where your company does not apply Risk-Based Approaches\nto Quality?"
  )+
  
  # theme
  custom_theme()+
    theme(
      plot.title = element_text(
      margin     = margin(b = 25)
      ))



# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot05.png", device = "png", plot = plot05,   
       width = 6, height = 3, units = 'in', dpi = 320)  




# Question 06 (Option 1) ----

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320) 



plot06_option1 <- q6_clean |>  
  ggplot(aes(x = response_id, y = rbm_approaches, fill = rbm_approaches)) +    
  
  # geoms
  ggdist::geom_dots( 
                    layout      = "bin", 
                    orientation = "x",
                    position    = "identity", 
                    dotsize     = 0.6,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5)) +
  urbnthemes::scale_color_discrete() +
  #coord_equal(clip = 'off')+

  # facet
  facet_wrap(~ stage_phase, ncol = 1, scales = "free") +  
  
  # labs
  labs(
    x = "Response Number",
    y = "RBQM Approach",
    title = "Q6: Which aspects of Risk-Based Approaches to Quality does\nyour company apply to the following Trial?"
  ) +
  
  # theme
  custom_theme_2()
  
  

# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot06_option1.png", device = "png", plot = plot06_option1,   
       width = 8, height = 10, units = 'in', dpi = 320)  




# Question 06 (Option 2) ---- 

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320) 


plot06_option2 <- q6_clean |>  
  ggplot(aes(x = response_id, y = stage_phase, fill = stage_phase)) +
  
  # geoms
  ggdist::geom_dots( 
                    layout      = "bin", 
                    orientation = "x",
                    position    = "identity", 
                    dotsize     = 0.6,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5)) +
  urbnthemes::scale_color_discrete() +
  #coord_equal(clip = 'off')+
  
  # facet
  facet_wrap(~ rbm_approaches, ncol = 1, scales = "free_x") +  
  
  # labs
  labs(
    x = "Response Number",
    y = "Trial Type",
    title = "Q6: Which aspects of Risk-Based Approaches to Quality does your company apply to the following Trial?"
  ) +
  
  # theme
  custom_theme_2()+
    theme(
      axis.title.x = element_text(size = rel(1.2)),
      axis.title.y = element_text(size = rel(1.2)),
      axis.text   = element_text(size = rel(1.1)), 
      panel.spacing.x = unit(2, 'lines')
      )



# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot06_option2.png", device = "png", plot = plot06_option2,   
       width = 8, height = 10, units = 'in', dpi = 320)  

 


# Question 07 ----

### |- UPDATED -----

# |- figure size ----
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 5,
  units  = "in",
  dpi    = 320) 


plot07 <- q7_clean |>  
  
  ggplot(aes(x = response_id, y = rbm_approach, fill = rbm_approach)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.6,
                    shape       = 22) +
  
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +                              
  scale_y_discrete(expand = c(0.01, 0)) +
  urbnthemes::scale_color_discrete() +
  coord_equal(clip = 'off')+
  
  # labs
  labs(
    x = "Response Number",
    y = "RBQM Approach",
    title = "Q7: If you answered Yes to Other Risk Based Approaches used in Q6, please identify\nthose used?"
  )+
  
  # theme
  custom_theme()+
  theme(
    plot.title  = element_text(margin = margin(b = 25)),
    axis.title.x = element_text(size = rel(.8)),
    axis.title.y = element_text(size = rel(.8)),
    )


# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot07.png", device = "png", plot = plot07,   
       width = 6, height = 3, units = 'in', dpi = 320) 




# Question 08 ---- 

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 11,
  units  = "in",
  dpi    = 320) 

# |- color palette ---- 
col_palette <- c("#1696d2", "#fdbf11", "#000000","#d2d2d2", "#ec008b", "#55b748",
                 "#5c5859", "#db2b27", "purple", "cyan")


# ### |-  option 1 ----
# plot08_option1 <- q8_option1 |>  
#            
#   ggplot(aes(x = response_id, y = activity, fill = activity)) +
#   
#   # geoms
#   ggdist::geom_dots(layout      = "bin",
#                     orientation = "x",
#                     position    = "identity",
#                     dotsize     = 0.6,
#                     shape       = 22) +
# 
#   
#   # scale
#   scale_x_continuous(breaks = seq(1, 15, by = 1), 
#                      limits = c(1, 16)) +
#   
#   scale_y_discrete(expand = c(0.01, 0.5))+
#   scale_fill_manual(values = col_palette)+
#   
#   # facet
#   facet_wrap(~ factor(trial_attributes, levels = c("Trial Design", "Trial Phase", "Trial Size",
#                                                    "Not Utilized", "Mixed Response", "Unable to Answer")), 
#              ncol = 2, 
#              scales = "free_x")+  
#   
#   # labs
#   labs(
#     x = "Response Number",
#     y = "Activity",
#     title = "Q8: Is your company’s Risk Based Approach to quality applied differently\ndepending on the following trial attributes?"
#     )+
#   
#   # theme
#   custom_theme_2()+
#   theme(panel.spacing.x = unit(2, 'lines'))
# 
# 
# 
# # saving plot
# ggsave(path = here("02_img/02_selected_figures/"),
#        filename = "plot08_option1.png", device = "png", plot = plot08_option1,   
#        width = 10, height = 11, units = 'in', dpi = 320)  



### |-  option 2 ----

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 11,
  units  = "in",
  dpi    = 320)


plot08_option2 <- q8_option2 |>  
  
  ggplot(aes(x = response_id, y = trial_attributes, fill = trial_attributes)) +
  
  # geoms
  ggdist::geom_dots(layout      = "bin",
                    orientation = "x",
                    position    = "identity",
                    dotsize     = 0.6,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5)) +
  urbnthemes::scale_color_discrete() +
  
  # facet
  facet_wrap(~ factor(activity, 
                      levels = c("Identification of CtQs", 
                                 "Overall Implementation of QbD",
                                 "QTLs Utilized",
                                 "QTLs Aligned with CtQs",
                                 "QTLs Review Processes",
                                 "Frequency of QTLs Review",
                                 "Communication of QTLs Breaches",
                                 "Implementation of Corrective Actions",
                                 "Reporting Significant QTLs Deviations in CSRs")), 
             ncol = 2, 
             scales = "free_x") +   
  
  # labs
  labs(
    x = "Response Number",
    y = "Trial Attributes",
    title = "Q8: Is your company’s Risk Based Approach to quality applied differently depending on the following trial attributes?"
  ) +
  
  # theme
  custom_theme_2() +
  theme(
    axis.title.x = element_text(size = rel(1.3)),
    axis.title.y = element_text(size = rel(1.3)),
    axis.text   = element_text(size = rel(1)), 
    panel.spacing.x = unit(2, 'lines')
  )


# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot08_option2.png", device = "png", plot = plot08_option2,   
       width = 10, height = 11, units = 'in', dpi = 320)  



# Question 10 ---- 
# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 11,
  units  = "in",
  dpi    = 320)

plot10 <- q10_clean |>  
  
  ggplot(aes(x = response_id, y = documentation_type, fill = documentation_type)) +
  
  # geoms
  ggdist::geom_dots(#smooth = "bar",
                    layout      = "bin",
                    orientation = "x",
                    position    = "identity",
                    dotsize     = 0.6,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  
  # facet
  facet_wrap(~ factor(activity, 
                      levels = c("Identification of CTQ’s",
                                 "Implementation of QbD",
                                 "Implementation of Risk Strategy",
                                 "QTL’s Utilized",
                                 "QTL Review Processes",
                                 "QTL’s Aligned with CTQ’s",
                                 "Frequency of QTL Review",
                                 "Communication of QTL Breaches",
                                 "Implementation of Corrective Actions",
                                 "Significant QTL Deviations in CSR"
                                 )), 
             ncol = 2, 
             scales = "free_x")+   
  
  # labs
  labs(
    x = "Response Number",
    y = "Documentation Type",
    title = "Q10: How does your company document your Risk-Based Approaches to Quality?"
  )+
  
  # theme
  custom_theme_2()+
  theme(
    axis.title.x = element_text(size = rel(1.3)),
    axis.title.y = element_text(size = rel(1.3)),
    axis.text   = element_text(size = rel(1.1)), 
    panel.spacing.x = unit(2, 'lines')
  )



# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot10.png", device = "png", plot = plot10,   
       width = 11, height = 10, units = 'in', dpi = 320)  



# Question 18 ---- 

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 11,
  height = 8,
  units  = "in",
  dpi    = 320) 


plot18 <- q18_clean |> 
  
  ggplot(aes(x = response_id, y = qtl_utilization, fill = qtl_utilization)) +
  
  # geoms
  ggdist::geom_dots(layout      = "bin",
                    orientation = "x",
                    position    = "identity",
                    dotsize     = 0.6,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  
  # facet
  facet_wrap(~ factor(characteristic, 
                      levels = c("QMS – Health Authority Inspection",
                                 "QMS – Continuous Improvement",
                                 "QMS - Submissions",
                                 "QMS – Senior Management Review",
                                 "QMS – Quality Management Oversight ",
                                 "Quality by Design Processes",
                                 "Critical to Quality Processes",
                                 "Other"
                      )), 
             ncol = 2, 
             scales = "free_x")+   
    
  # labs
  labs(
    x = "Response Number",
    y = "Documentation Type",
    title = "Q18: How are QTL’s governed / utilized at your company?"
  )+
  
    # theme
    custom_theme_2()+
    theme(
      axis.title.x = element_text(size = rel(1.2)),
      axis.title.y = element_text(size = rel(1.2)),
      axis.text   = element_text(size = rel(1.)), 
      panel.spacing.x = unit(2, 'lines')
    )



# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot18.png", device = "png", plot = plot18,   
       width = 11, height = 8, units = 'in', dpi = 320)  



# Question 19 ---- 

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 11,
  height = 8,
  units  = "in",
  dpi    = 320) 

plot19 <- q19_clean |> 
  
  ggplot(aes(x = response_id, y = action_type, fill = action_type)) +
  
  # geoms
  ggdist::geom_dots(layout      = "bin",
                    orientation = "x",
                    position    = "identity",
                    dotsize     = 0.6,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  
  # facet
  facet_wrap(~ factor(possible_action, 
                    levels = c("Protocol Amendment",
                               "Change in QTL Threshold",
                               "Change in QTL Parameter Metric",
                               "Change in QTL Statistical Distribution",
                               "Change in QTL Statistical Methodology",
                               "Change in Trial Monitoring Strategy",
                               "Instructions Provided to Sites",
                               "Instructions Provided to Vendors",
                               "Other"  
                      )),
             ncol = 2, 
             scales = "free_x")+
  
  # labs
  labs(
    x = "Response Number",
    y = "Action Type",
    title = "Q19: What types of actions has your company typically taken, or plan to take, in response to a QTL breach?"
  )+
  
  # theme
  custom_theme_2()+
  theme(
    axis.title.x = element_text(size = rel(1.2)),
    axis.title.y = element_text(size = rel(1.2)),
    axis.text   = element_text(size = rel(1.)), 
    panel.spacing.x = unit(2, 'lines')
  )


# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot19.png", device = "png", plot = plot19,   
       width = 11, height = 8, units = 'in', dpi = 320)  



# Question 20.3 ----

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 5,
  units  = "in",
  dpi    = 320) 

plot20.3 <- q20.3_clean |>  
  
  ggplot(aes(x = response_id, 
             y = str_to_title(csr), 
             fill = csr)) +     
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.6,
                    shape       = 22) +
  
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5)) +
  urbnthemes::scale_color_discrete() +
  coord_equal(clip = 'off') +
  
  # labs
  labs(
    x = "Response Number",
    y = "Method",
    title = "Q20.3: If QTL Primary Limit Breaches are evaluated for importance,\nhow do you determine as to whether they should be reported in the CSR?"
  ) +
  
  # theme
  custom_theme()+
  theme(
    plot.title  = element_text(margin = margin(b = 10)),
    axis.title.x = element_text(size = rel(.9)),
    axis.title.y = element_text(size = rel(.9)),
    axis.text   = element_text(size = rel(.8))
  )


# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot20.3.png", device = "png", plot = plot20.3,   
       width = 6, height = 5, units = 'in', dpi = 320)  



# Question 23 ----

# |- status ---- 

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 12,
  height = 12,
  units  = "in",
  dpi    = 320) 


plot23_status <- q23_clean |>  
  
  # filter out Others
  filter(!str_detect(transcelerate_parameters, "Other\\d+")) |>  
  
  ggplot(aes(x = response_id, y = status, fill = status)) +                                      
  
  # geoms
  ggdist::geom_dots(orientation = "x",
                    layout      = "bin",                
                    position    = "identity",
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5)) +
  urbnthemes::scale_color_discrete() +
  
  # facet
  facet_wrap(~ factor(transcelerate_parameters, 
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
             ncol = 2, 
             scales = "free_x")+
  
  # labs
  labs(
    x = "Response Number",
    y = "Status",
    title = "Q23: Indicate the possible/potential Parameters for QTLs (as defined by TransCelerate) that are currently in use or are planned to be used.",
    fill  = "Status"
  )+
  
  # theme
  custom_theme_2() +
  theme(
    axis.title.x    = element_text(size = rel(1.2)),
    axis.title.y    = element_text(size = rel(1.2)),
    axis.text       = element_text(size = rel(1.)), 
    panel.spacing.x = unit(2, 'lines')
  )



# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot23_status.png", device = "png", plot = plot23_status,   
       width = 12, height = 12, units = 'in', dpi = 320) 



# |- rate ----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320) 


# |- option 1 ---- 
plot23_rate_option1 <- q23_rate_clean |>  
  
  # filter out Others
  filter(!str_detect(transcelerate_parameters, "Other\\d+")) |>  
  
  ggplot(aes(x = rate_scale, y = pct, fill = rate_scale)) +                                      
  
  # geoms
  geom_col()+
  geom_text(aes(label = bar_label),
            hjust  = 0.5,
            vjust  = -0.5,
            family ='text', 
            color  = "gray20",
            size   = 4) +
  
  # scale
  scale_x_discrete(expand = c(0.01, 0.5), 
                   limits = c("Low","Medium","High"),
                   ) +

  scale_y_continuous(breaks = seq(0, 1, by = .25), 
                     limits = c(0, 1), 
                     labels = percent_format()
                     ) +

  urbnthemes::scale_color_discrete() +
  
  # facet
  facet_wrap(~ factor(transcelerate_parameters, 
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
             ncol = 3, 
             scales = "free_x") +
  
  # labs
  labs(
    x = "Scale",
    y = "Percentage",
    title = "Q23: Rate on a scale of Low, Medium, or High the perceived value of the Parameter\n(as defined by TransCelerate). ",
    fill  = "Status"
  ) +
  
  # theme
  custom_theme_2() +
  theme(
    axis.title.x    = element_text(size = rel(1.2)),
    axis.title.y    = element_text(size = rel(1.2)),
    axis.text       = element_text(size = rel(1.)), 
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank(),
    panel.grid      = element_blank(),
    panel.spacing.x = unit(2, 'lines'))



# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot23_rate_option1.png", device = "png", plot = plot23_rate_option1,   
       width = 10, height = 10, units = 'in', dpi = 320) 



# |- option 2 ---- 

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320) 



plot23_rate_option2 <- q23_rate_clean |>  
  
  # filter out Others
  filter(!str_detect(transcelerate_parameters, "Other\\d+")) |>  
  
  # factors
  mutate(rate_scale = factor(rate_scale,
                             levels = c(
                               "Low",
                               "Medium",
                               "High"
                             ))) |>  
  
  # Reorder
  mutate(
    transcelerate_parameters = fct_reorder2(transcelerate_parameters, rate_scale, -pct),
  ) |>
  
  ggplot(aes(x = transcelerate_parameters, y = pct, fill = rate_scale)) +                                    
  
  # geoms
  geom_col( position  = "stack")+
  
  geom_text(aes(label = bar_label),
            #position  = position_stack(vjust = .5),
            hjust     = -0.15,
            family    = 'text',
            size      = 4,
            color     = "gray20"
  ) +
  
  geom_hline(yintercept = 0, linewidth = .4, color = 'gray80') +
  
  # scale
  scale_x_discrete(expand = c(0.01, 0.5)) +
  
  scale_y_continuous(breaks = seq(0, 1, by = .25), 
                     limits = c(0, 1), 
                     labels = percent_format()) +
  
  urbnthemes::scale_color_discrete() +
  coord_flip(clip = "off") +
  
  # facet
  facet_grid(~rate_scale) +
  
  # labs
  labs(
    x = "Transcelerate Parameters",
    y = "Percentage",
    title = "Q23: Rate on a scale of Low, Medium, or High the perceived value of the Parameter\n(as defined by TransCelerate). ",
    fill  = "Status"
  )+
  
  # theme
  custom_theme_2()+
  theme(
    axis.title.x    = element_text(size = rel(1.3)),
    axis.title.y    = element_text(size = rel(1.3)),
    axis.text.y     = element_text(size = rel(1.1)), 
    
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    axis.line.x     = element_blank(),
    panel.grid      = element_blank(),
    
    panel.spacing.x = unit(2, 'lines'),
    strip.text      = element_textbox(hjust  = 0,halign = 0 )
  )

# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot23_rate_option2.png", device = "png", plot = plot23_rate_option2,   
       width = 10, height = 10, units = 'in', dpi = 320) 



# Question 24 ----

# |- status ---- 

# |- figure size 
# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 12,
  height = 12,
  units  = "in",
  dpi    = 320) 


plot24_status <- q24_clean |>  
  
  # filter out Others
  filter(!str_detect(additional_parameters, "Other\\d+")) |>  
  
  ggplot(aes(x = response_id, y = status, fill = status))+                                      
  
  # geoms
  ggdist::geom_dots(orientation = "x",
                    layout      = "bin",                
                    position    = "identity",
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 16)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  
  # facet
  facet_wrap(~ factor(additional_parameters, 
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
             ncol = 2, 
             scales = "free_x")+
  
  # labs
  labs(
    x = "Response Number",
    y = "Status",
    title = "Q24: Please indicate if any of the below Additional Parameters are also considered\nas Parameters for QTLs, whether they are currently in use or are planned to be used.",
    fill  = "Status"
  )+
  
  # theme
  custom_theme_2()+
  theme(
    axis.title.x    = element_text(size = rel(1.3)),
    axis.title.y    = element_text(size = rel(1.3)),
    axis.text       = element_text(size = rel(1.1)), 
    panel.spacing.x = unit(2, 'lines')
  )


# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot24_status.png", device = "png", plot = plot24_status,   
       width = 12, height = 12, units = 'in', dpi = 320) 



# |- rate ----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320) 


# |- option 1 ---- 
plot24_rate_option1 <- q24_rate_clean |>  
  
  # filter out Others
  filter(!str_detect(additional_parameters, "Other\\d+")) |>  
  
  ggplot(aes(x = rate_scale, y = pct, fill = rate_scale))+                                      
  
  # geoms
  geom_col()+
  geom_text(aes(label = bar_label),
            hjust  = 0.5,
            vjust  = -0.5,
            family ='text', 
            color  = "gray20",
            size   = 4) +
  
  # scale
  scale_x_discrete(expand = c(0.01, 0.5), 
                   limits = c("Low","Medium","High"),
                   ) +
  
  scale_y_continuous(breaks = seq(0, 1, by = .25), 
                     limits = c(0, 1), 
                     labels = percent_format()
                     )+
  
  urbnthemes::scale_color_discrete()+
  
  # facet
  facet_wrap(~ factor(additional_parameters, 
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
             ncol = 3, 
             scales = "free_x")+
  
  # labs
  labs(
    x = "Scale",
    y = "Percentage",
    title = "Q24: Rate on a scale of Low, Medium, or High the perceived value of the Additional Parameter",
    fill  = "Status"
  )+
  
  # theme
  custom_theme_2()+
  theme(
    plot.title      = element_text(size = 16),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank(),
    panel.grid      = element_blank(),
    panel.spacing.x = unit(2, 'lines'))



# saving plot
ggsave(path = here("02_img/02_selected_figures/"),
       filename = "plot24_rate_option1.png", device = "png", plot = plot24_rate_option1,   
       width = 10, height = 10, units = 'in', dpi = 320) 


# |- option 2 ---- 

### |- UPDATED -----

# |- figure size 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320) 

plot24_rate_option2 <- q24_rate_clean |>  
  
  # filter out Others
  filter(!str_detect(additional_parameters, "Other\\d+")) |>  
  
  # factors
  mutate(rate_scale = factor(rate_scale,
                             levels = c(
                               "Low",
                               "Medium",
                               "High"
                             ))) |>  
  
  # Reorder
  mutate(
    additional_parameters = fct_reorder2(additional_parameters, rate_scale, -pct),
  ) |>
  
  ggplot(aes(x = additional_parameters, y = pct, fill = rate_scale))+                                    
  
  # geoms
  geom_col( position  = "stack") +
  
  geom_text(aes(label = bar_label),
            #position  = position_stack(vjust = .5),
            hjust     = -0.15,
            family    = 'text',
            size      = 4,
            color     = "gray20"
  ) + 
  
  geom_hline(yintercept = 0, linewidth = .4, color = 'gray80') +
  
  # scale
  scale_x_discrete(expand = c(0.01, 0.5)) +
  
  scale_y_continuous(breaks = seq(0, 1, by = .25), 
                     limits = c(0, 1), 
                     labels = percent_format())+
  
  urbnthemes::scale_color_discrete() +
  coord_flip(clip = "off") +
  
  # facet
  facet_grid(~rate_scale) +
  
  # labs
  labs(
    x = "Additional Parameters",
    y = "Percentage",
    title = "Q24: Rate on a scale of Low, Medium, or High the perceived value of the Additional Parameter",
    fill  = "Status"
  ) +
  
  # theme
  custom_theme_2() +
  theme(
    axis.title.x    = element_text(size = rel(1.3)),
    axis.title.y    = element_text(size = rel(1.3)),
    axis.text.y     = element_text(size = rel(1.1)), 
    
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    axis.line.x     = element_blank(),
    panel.grid      = element_blank(),
    
    panel.spacing.x = unit(2, 'lines'),
    strip.text      = element_textbox(hjust  = 0,halign = 0 )
  )


# saving plot
ggsave(path = here("02_img/03_updated_figures/"),
       filename = "plot24_rate_option2.png", device = "png", plot = plot24_rate_option2,   
       width = 10, height = 10, units = 'in', dpi = 320)




