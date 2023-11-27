

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Focus:
#' Work on selected questions/figures only. 

#' Author: Steven Ponce
#' Date: 2023-16-01



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



# Question 01 ----

### |- plot aesthetics ---- 
bkg_col         <- '#fdfdff' 
text_col        <- "gray20"
title_col       <- "gray20"

### |-  fonts ----
font_add_google("Lato", family = "title")                           
font_add_google("Lato", family ="text")  
showtext_auto(enable = TRUE)  



# Question 03 ----
plot03 <- q3_clean %>% 
  
  ggplot(aes(x = response_id, y = functional_area, fill = functional_area)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.8,
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
    title = "Q3: Which Functional Area(s) are involved in trial level\nRisk-Based Approaches to Quality?"
  )+
  
  # theme
  custom_theme()
  

# saving plot
ggsave(path = here("02_img/"),
       filename = "plot03.png", device = "png", plot = plot03,   
       width = 6, height = 4, units = 'in', dpi = 320)  



# Question 04 ----
plot04 <- q4_clean %>% 
  
  ggplot(aes(x = response_id, y = functional_area, fill = functional_area)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.8,
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
    title = "Q4: Which Functional Area leads trial level Risk-Based\nApproaches to Quality?"
  )+
  
  # theme
  custom_theme()

  

# saving plot
ggsave(path = here("02_img/"),
       filename = "plot04.png", device = "png", plot = plot04,   
       width = 6, height = 4, units = 'in', dpi = 320)  



# Question 05 ----
plot05 <- q5_clean %>%
  
  ggplot(aes(x = response_id, y = trial_types, fill = trial_types)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.4,
                    shape       = 22) +
  
  
  # scale
  scale_x_discrete()+                                 
  scale_y_discrete(expand = c(0.01, 0))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  # labs
  labs(
    x = "Response Number",
    y = "Trial types",
    title = "Q5: What are the trial types where your company does not\napply Risk-Based Approaches to Quality?"
  )+
  
  # theme
  custom_theme()



# saving plot
ggsave(path = here("02_img/"),
       filename = "plot05.png", device = "png", plot = plot05,   
       width = 6, height = 4, units = 'in', dpi = 320)  




# Question 06 (Option 1) ----

# |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320) 



plot06_option1 <- q6_clean %>% 
  ggplot(aes(x = response_id, y = rbm_approaches, fill = rbm_approaches)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "x",
                    position    = "identity", 
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  #coord_equal(clip = 'off')+

  # facet
  facet_wrap(~ stage_phase, ncol = 1, scales = "free")+  
  
  # labs
  labs(
    x = "Response Number",
    y = "Risk-Based Monitoring Approach",
    title = "Q6: Which aspects of Risk-Based Approaches to Quality does your\ncompany apply to the following Trial?"
  )+
  
  # theme
  custom_theme_2()
  
  

# saving plot
ggsave(path = here("02_img/"),
       filename = "plot06_option1.png", device = "png", plot = plot06_option1,   
       width = 8, height = 10, units = 'in', dpi = 320)  




# Question 06 (Option 2) ---- 
plot06_option2 <- q6_clean %>% 
  ggplot(aes(x = response_id, y = stage_phase, fill = stage_phase)) +
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "x",
                    position    = "identity", 
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  #coord_equal(clip = 'off')+
  
  # facet
  facet_wrap(~ rbm_approaches, ncol = 1, scales = "free")+  
  
  # labs
  labs(
    x = "Response Number",
    y = "Trial Type",
    title = "Q6: Which aspects of Risk-Based Approaches to Quality does your\ncompany apply to the following Trial?"
  )+
  
  # theme
  custom_theme_2()



# saving plot
ggsave(path = here("02_img/"),
       filename = "plot06_option2.png", device = "png", plot = plot06_option2,   
       width = 8, height = 10, units = 'in', dpi = 320)  




# Question 07 ----
plot07 <- q7_clean %>% 
  
  ggplot(aes(x = response_id, y = rbm_approach, fill = rbm_approach)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.7,
                    shape       = 22) +
  
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +                              
  scale_y_discrete(expand = c(0.01, 0))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  # labs
  labs(
    x = "Response Number",
    y = "RBM Approach",
    title = "Q7: If you answered Yes to Other Risk Based Approaches used\nin Q6, please identify those used?"
  )+
  
  # theme
  custom_theme()+
  theme(
    plot.title = element_text(
    margin     = margin(b = 25)
    ))


# saving plot
ggsave(path = here("02_img/"),
       filename = "plot07.png", device = "png", plot = plot07,   
       width = 6, height = 5, units = 'in', dpi = 320) 




# Question 08 ---- 

# |- figure size ---- 
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


### |-  option 1 ----
plot08_option1 <- q8_option1 %>% 
           
  ggplot(aes(x = response_id, y = activity, fill = activity)) +
  
  # geoms
  ggdist::geom_dots(layout      = "bin",
                    orientation = "x",
                    position    = "identity",
                    dotsize     = 0.5,
                    shape       = 22) +

  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  scale_fill_manual(values = col_palette)+
  
  # facet
  facet_wrap(~ factor(trial_attributes, levels = c("Trial Design", "Trial Phase", "Trial Size",
                                                   "Not Utilized", "Mixed Response", "Unable to Answer")), 
             ncol = 2, 
             scales = "free_x")+  
  
  # labs
  labs(
    x = "Response Number",
    y = "Activity",
    title = "Q8: Is your company’s Risk Based Approach to quality applied differently\ndepending on the following trial attributes?"
    )+
  
  # theme
  custom_theme_2()+
  theme(panel.spacing.x = unit(2, 'lines'))



# saving plot
ggsave(path = here("02_img/"),
       filename = "plot08_option1.png", device = "png", plot = plot08_option1,   
       width = 10, height = 11, units = 'in', dpi = 320)  




### |-  option 2 ----
plot08_option2 <- q8_option2 %>% 
  
  ggplot(aes(x = response_id, y = trial_attributes, fill = trial_attributes)) +
  
  # geoms
  ggdist::geom_dots(layout      = "bin",
                    orientation = "x",
                    position    = "identity",
                    dotsize     = 0.5,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +
  
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  
  # facet
  facet_wrap(~ factor(activity, 
                      levels = c("Identification of CTQ’s", 
                                 "Overall Implementation of QbD",
                                 "QTL’s Utilized",
                                 "QTL’s Aligned with CTQ’s",
                                 "QTL Review Processes",
                                 "Frequency of QTL Review",
                                 "Communication of QTL Breaches",
                                 "Implementation of Corrective Actions",
                                 "Reporting Significant QTL Deviations in CSR")), 
             ncol = 2, 
             scales = "free_x")+   
  
  # labs
  labs(
    x = "Response Number",
    y = "Trial Attributes",
    title = "Q8: Is your company’s Risk Based Approach to quality applied differently\ndepending on the following trial attributes?"
  )+
  
  # theme
  custom_theme_2()+
  theme(panel.spacing.x = unit(2, 'lines'))



# saving plot
ggsave(path = here("02_img/"),
       filename = "plot08_option2.png", device = "png", plot = plot08_option2,   
       width = 10, height = 11, units = 'in', dpi = 320)  



# Question 10 ---- 
plot10 <- q10_clean %>% 
  
  ggplot(aes(x = response_id, y = documentation_type, fill = documentation_type)) +
  
  # geoms
  ggdist::geom_dots(layout      = "bin",
                    orientation = "x",
                    position    = "identity",
                    dotsize     = 0.5,
                    shape       = 22) +
  
  # scale
  scale_x_continuous(breaks = seq(1, 15, by = 1), 
                     limits = c(1, 15)) +
  
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
    y = "Docyumentation Type",
    title = "Q10: How does your company document your Risk-Based Approaches to Quality?"
  )+
  
  # theme
  custom_theme_2()+
  theme(panel.spacing.x = unit(2, 'lines'))



# saving plot
ggsave(path = here("02_img/"),
       filename = "plot10.png", device = "png", plot = plot10,   
       width = 10, height = 11, units = 'in', dpi = 320)  
















# Question 23 ----

# |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 9,
  height = 6,
  units  = "in",
  dpi    = 320) 


p23_02 <- q23_clean %>% 
  
  # filter out Others
  filter(!str_detect(parameters, "Other\\d+")) %>% 
  
  ggplot(aes(x = response_id, 
             y = factor(parameters),
             fill = status                     
             ))+                                      
  
  # geoms
  ggdist::geom_dots(smooth      = "discrete", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_discrete() +                               
  scale_y_discrete(expand = c(0.01, 0.1))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  # facet
  #facet_wrap(~ status, ncol = 1)+
  
  # labs
  labs(
    x = "Response Number",
    y = "TransCelerate Parameters",
    title = "Q23: Indicate the possible/potential Parameters for QTLs (as defined by TransCelerate)\nthat are currently in use or are planned to be used.",
    fill  = "Status"
  )+
  
  # theme
  theme_minimal() +
  theme(
    
    plot.title.position   = "plot",              
    plot.caption.position = "plot",          
    
    legend.position       = c(0.95, 1.1),
    legend.justification  = c("right", "top"),
    legend.box.just       = "right",
    legend.margin         = margin(6, 6, 6, 6),
    legend.direction      = 'horizontal',
    legend.text           = element_text(size = 12),

    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    
    axis.title.x          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(t = 12), family = 'text'), 
    axis.title.y          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(r = 12), family = 'text'),
    
    axis.text             = element_text(size = 10, color = text_col, family = 'text'),
    
    axis.line.x           = element_line(color = "grey80", linewidth = .4),
    axis.line.y           = element_blank(),

    plot.title            = element_text(
      family              = 'title',
      color               = title_col,
      face                = "bold",
      size                = 14,  
      lineheight          = 0.9, 
      margin              = margin(b = 30)),
  )



# saving plot
ggsave(path = here("02_img/"),
       filename = "Q23b_02.png", device = "png", plot = p23_02,   
       width = 9, height = 6, units = 'in', dpi = 320)  




# Question 24 ----

## (Alternative) ---- 

# |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 9,
  height = 6,
  units  = "in",
  dpi    = 320) 


p24_02 <- q24_clean %>% 
  
  # filter out Others
  filter(!str_detect(parameters, "Other\\d+")) %>% 
  
  ggplot(aes(x = response_id, 
             y = factor(parameters),
             fill = scale                     
  ))+                                      
  
  # geoms
  ggdist::geom_dots(smooth      = "discrete", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_discrete() +                               
  scale_y_discrete(expand = c(0.01, 0.1))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  # facet
  # facet_wrap(~ scale, ncol = 1)+
  
  # labs
  labs(
    x = "Response Number",
    y = "TransCelerate Parameters",
    title = "Q24: Rate the Parameters for QTLs (as defined by TransCelerate)",
    fill  = "Scale"
  )+
  
  # theme
  theme_minimal() +
  theme(
    
    plot.title.position   = "plot",              
    plot.caption.position = "plot",          
    
    legend.position       = c(0.95, 1.1),
    legend.justification  = c("right", "top"),
    legend.box.just       = "right",
    legend.margin         = margin(6, 6, 6, 6),
    legend.direction      = 'horizontal',
    legend.text           = element_text(size = 12),
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    
    axis.title.x          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(t = 12), family = 'text'), 
    axis.title.y          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(r = 12), family = 'text'),
    
    axis.text             = element_text(size = 10, color = text_col, family = 'text'),
    
    axis.line.x           = element_line(color = "grey80", linewidth = .4),
    axis.line.y           = element_blank(),
    
    plot.title            = element_text(
      family              = 'title',
      color               = title_col,
      face                = "bold",
      size                = 14,  
      lineheight          = 0.9, 
      margin              = margin(b = 30)),
  )



# saving plot
ggsave(path = here("02_img/"),
       filename = "Q24b_02.png", device = "png", plot = p24_02,   
       width = 9, height = 6, units = 'in', dpi = 320)  


