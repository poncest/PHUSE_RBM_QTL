

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

p3b <- q3_clean %>% 
  
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
  theme_minimal()+
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
    
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
      margin              = margin(b = 10)),
  )



# saving plot
ggsave(path = here("02_img/"),
       filename = "Q03b.png", device = "png", plot = p3b,   
       width = 6, height = 4, units = 'in', dpi = 320)  






# Question 06 ----


p6 <- q6_clean %>% 
  
  # bar_plot()
  bar_plot(x_variable = rbm_approaches, y_variable = pct, label_adjust = 0.025, col_width = 0.90)+
  
  # Scales
  scale_x_reordered(expand = expansion(add = c(0.8, .3)))+ 
  scale_y_continuous(breaks = seq(0, .05, by = .025), 
                     limits = c(0, .05),
                     expand = c(0, 0.0),
                     labels = percent_format())+
  
  # Facet
  facet_wrap( ~ factor(stage_phase,
                       levels = c("FIH", "Phase I Non-FIH", "Phase II",
                                  "Phase III - RS", "Phase III - FU",
                                  "Phase IV")),
              scales = "free_y", ncol = 2) +
    
  # Labs
  labs(x = "",
       y = "Percent",
       title = "Q6: Which aspects of Risk-Based Approaches to Quality\ndoes your company apply to the following Trial?"
  )+
  
  # Theme
  bar_plot_theme()+
  theme(
    strip.text   = element_textbox(size = 12,
                                   face   = 'bold',
                                   color  = text_col,
                                   hjust  = 0,
                                   halign = 0,
                                   fill   = "transparent"),
    
    panel.spacing.x = unit(1, 'lines')
    
  )

p6


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q06.png", device = "png", plot = p6,   
       width = 8.5, height = 8, units = 'in', dpi = 320)  


# Question 08 ----

# |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 9,
  height = 9,
  units  = "in",
  dpi    = 320) 

p8 <- q8_clean %>% 
  
  # bar_plot()
  bar_plot_2(x_variable = trial_attribute, y_variable = pct, label_adjust = 0.025, col_width = 0.90)+
  
  # Scales
  scale_x_reordered(expand = expansion(add = c(0.8, .3)))+ 
  scale_y_continuous(breaks = seq(0, .05, by = .025), 
                     limits = c(0, .05),
                     expand = c(0, 0.0),
                     labels = percent_format())+
  
  # Facet
  facet_wrap( ~ factor(activity,
                       levels = c(
                         "Identification<br>of CTQ’s",
                         "Overall<br>Implementation<br>of QbD",
                         "QTL’s<br>Utilized",
                         "QTL’s Aligned<br>with CTQ’s",
                         "QTL Review<br>Processes",
                         "Frequency<br>of QTL Review",
                         "Communication<br>of QTL Breaches",
                         "Implementation<br>of Corrective<br>Actions",
                         "Reporting<br>Significant QTL<br>Deviations in CSR"
                         )),
              scales = "free_y", ncol = 3) +

  # Labs
  labs(x = "Activity",
       y = "Percent",
       title = "Q8: Is your company’s Risk Based Approach to quality applied differently\ndepending on the following trial attributes?"
  )+
  
  # Theme
  bar_plot_theme(base_size = 10)+
  theme(
    strip.text   = element_textbox(size = 12,
                                   face   = 'bold',
                                   color  = text_col,
                                   hjust  = 0,
                                   halign = 0,
                                   fill   = "transparent"),
    
    panel.spacing.x = unit(.85, 'lines')
    
  )

p8


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q08.png", device = "png", plot = p8,   
       width = 9, height = 9, units = 'in', dpi = 320)  



# Question 10 ----

# |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 9,
  height = 9,
  units  = "in",
  dpi    = 320) 

p10 <- q10_clean %>% 
  
  # bar_plot()
  bar_plot_2(x_variable = documentation_type, y_variable = pct, label_adjust = 0.028, col_width = 0.95)+
  
  # Scales
  scale_x_reordered(expand = expansion(add = c(0.8, .3)))+ 
  scale_y_continuous(breaks = seq(0, .06, by = .03), 
                     limits = c(0, .07),
                     expand = c(0, 0.0),
                     labels = percent_format())+
  
  # Facet                                                                            
  facet_wrap( ~ factor(activity,
                       levels = c(
                         "Identification of CTQ’s",
                         "Overall Implementation<br>of QbD",
                         "Implementation of<br>Risk Strategy",
                         "QTL’s Utilized",
                         "QTL Review Processes",
                         "QTL’s Aligned<br>with CTQ’s",  
                         "Frequency of<br>QTL Review",
                         "Communication<br>of QTL Breaches",
                         "Implementation<br>of Corrective<br>Actions",
                         "Reporting<br>Significant QTL<br>Deviations in CSR"
                         )),
              scales = "free_y", ncol = 2) +
    
  # Labs
  labs(x = "Activity",
       y = "Percent",
       title = "Q10: How does your company document your Risk-Based Approaches to Quality?"
  )+
  
  # Theme
  bar_plot_theme(base_size = 10)+
  theme(
    strip.text   = element_textbox(size = 12,
                                   face   = 'bold',
                                   color  = text_col,
                                   hjust  = 0,
                                   halign = 0,
                                   fill   = "transparent"),
    
    panel.spacing.x = unit(.85, 'lines')
    
  )

p10


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q10.png", device = "png", plot = p10,   
       width = 9, height = 9, units = 'in', dpi = 320)  



