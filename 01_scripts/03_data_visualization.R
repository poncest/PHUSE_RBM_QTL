

#' PHUSE RBM Working Group
#' Assessing the use of Quality Tolerance Limits (QTL) in the Pharma Industry

#' Goal:
#' 1. Clean the data from the QTL survey
#' 2. Generate the corresponding visual to support the 'whitepaper'.

#' Author: Steven Ponce
#' Date: 2023-11-01



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
caption_col     <- "gray20"
bar_label_color <- "gray20" 
bar_plot_col    <- "#004f63"
#palette_col     <- met.brewer("Cross", n = 09, type = "discrete", direction = -1)


p1 <- q1_clean %>% 
  # bar_plot()
  bar_plot(x_variable = your_company_type, y_variable = pct, label_adjust = 0.15, col_width = 0.5)+
  
  # Scales
  scale_x_discrete(expand = expansion(add = c(.4, .4)))+
  scale_y_continuous(breaks = seq(0, 1, by = .25), 
                     limits = c(0, 1),
                     expand = c(0, 0),
                     labels = percent_format())+

  # Labs
  labs(x = "",
       y = "Percent",
       title = "Q1: Company Type?",
       )+
  
  # Theme
  bar_plot_theme()

p1
  

# saving plot
ggsave(path = here("02_img/"),
       filename = "Q1.png", device = "png", plot = p1,   
       width = 6, height = 5, units = 'in', dpi = 320)  


# Question 02 ----

p2 <- q2_clean %>% 
  
  # bar_plot()
  bar_plot(x_variable = company_size, y_variable = pct, label_adjust = 0.3, col_width = 0.85)+

  # Scales
  scale_x_discrete(expand = expansion(add = c(.8, .6)))+
  scale_y_continuous(breaks = seq(0, .4, by = .1), 
                     limits = c(0, .41),
                     expand = c(0, 0),
                     labels = percent_format())+

  # Facets
  facet_wrap(~ factor(number_trials, levels = c("> 200 Phase I-III clinical trials",
                                                "≤ 100 Phase I-III clinical trials",
                                                "> 100 - ≤ 200 Phase I-III clinical trials")),
             ncol = 1, scales = "free_y") +
  
  # Labs
  labs(x = "",
       y = "Percent",
       title = "Q2: Company Size & Number of Trials?",
       )+
  
  # Theme
  bar_plot_theme()+
  theme(
    strip.text = element_text(hjust = 0,
                              margin = margin(b = 0),
                              size = rel(0.9), 
                              vjust = 0)
  )

p2


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q2.png", device = "png", plot = p2,   
       width = 6, height = 5, units = 'in', dpi = 320)  


# Question 03 ----

p3 <- q3_clean %>% 
  
  # bar_plot()
  bar_plot(x_variable = functional_area, y_variable = pct, label_adjust = 0.15, col_width = 0.85)+
  
  # Scales
  scale_x_discrete(expand = expansion(add = c(0.8, .2)))+
  scale_y_continuous(breaks = seq(0, .2, by = .1), 
                     limits = c(0, .2),
                     expand = c(0, 0),
                     labels = percent_format())+
  
  # Labs
  labs(x = "",
       y = "Percent",
       title = "Q3: Which Functional Area(s) are involved in trial level\nRisk-Based Approaches to Quality?",
       )+
  
  # Theme
  bar_plot_theme()

p3


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q3.png", device = "png", plot = p3,   
       width = 6, height = 6, units = 'in', dpi = 320)  


# Break down of Q3 `Others`  
# It might not be needed - a Table might be a better option
# p3b <- q3_clean_others %>% 
#   ggplot(aes(x = other, y = pct))+
#   
#   # Geoms
#   geom_col(width = .85, fill = bar_plot_col)+
#   
#   geom_textbox(aes(label = paste0("<span style='font-size:14pt'><br>",
#                                   bar_label,"</span>",
#                                   " (n = ", count, ")"),
#                    
#                    hjust =  case_when(pct < .15 ~ 0,
#                                       TRUE ~ 1),
#                    halign = case_when(pct < .15 ~ 0,
#                                       TRUE ~ 1),
#                    colour = case_when(pct < .15 ~ "gray20",
#                                       TRUE ~ "#fdfdff")),
#                fill = NA,
#                box.colour = NA,
#                size = 3.5,
#                fontface = "bold") +
#   
#   # Scales
#   scale_x_discrete(expand = expansion(add = c(0.8, .2)))+
#   scale_y_continuous(breaks = seq(0, .3, by = .15), 
#                      limits = c(0, .3),
#                      expand = c(0, 0),
#                      labels = percent_format())+
#   scale_colour_identity() +                                   # bar label color
#   coord_flip()+
#   
#   # Labs
#   labs(x = "",
#        y = "Percent",
#        title = "Q3: Other category breakdown",
#        # caption = "Figure X: caption placeholder "
#   )+
#   
#   # Theme
#   theme_bar_plot()
# 
# 
# # saving plot
# ggsave(path = here("02_img/"),
#        filename = "Q3b.png", device = "png", plot = p3b,   
#        width = 6, height = 6, units = 'in', dpi = 320)  



# Question 04 ----

p4 <- q4_clean %>% 
  
  # bar_plot()
  bar_plot(x_variable = functional_area, y_variable = pct, label_adjust = 0.15, col_width = 0.85)+
  
  # Scales
  scale_x_discrete(expand = expansion(add = c(0.8, .3)))+
  scale_y_continuous(breaks = seq(0, .3, by = .1), 
                     limits = c(0, .32),
                     expand = c(0, 0),
                     labels = percent_format())+

  # Labs
  labs(x = "",
       y = "Percent",
       title = "Q4: Which Functional Area(s) are involved in trial level\nRisk-Based Approaches to Quality?"
       )+
  
  # Theme
  bar_plot_theme()

p4


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q4.png", device = "png", plot = p4,   
       width = 6, height = 5, units = 'in', dpi = 320)  



# Question 05 ----

p5 <- q5_clean %>% 
  
  # bar_plot()
  bar_plot(x_variable = trial_types, y_variable = pct, label_adjust = 0.25, col_width = 0.85)+
  
  # Scales
  scale_x_discrete(expand = expansion(add = c(0.8, .3)))+
  scale_y_continuous(breaks = seq(0, .5, by = .25), 
                     limits = c(0, .5),
                     expand = c(0, 0),
                     labels = percent_format())+
  
  # Labs
  labs(x = "",
       y = "Percent",
       title = "Q5: What are the trial types where your company does not apply Risk-Based Approaches to Quality?"
      )+
  
  # Theme
  bar_plot_theme()

p5


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q5.png", device = "png", plot = p5,   
       width = 6, height = 5, units = 'in', dpi = 320)  



# Question 06 ----

# |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8.5,
  height = 8,
  units  = "in",
  dpi    = 320) 

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
                                   hjust  = 0.5,
                                   halign = 0.5,
                                   fill   = "transparent"),
    
    panel.spacing.x = unit(1, 'lines')
    
  )

p6


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q6.png", device = "png", plot = p6,   
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
                         "Reporting<br>Significant QTL<br>Deviations in CSR")
                       ),
              scales = "free_y", ncol = 3) +

  # Labs
  labs(x = "Activity",
       y = "Percent",
       title = "Q8: Is your company’s Risk Based Approach to quality applied differently\ndepending on the following trial attributes?"
  )+
  
  # Theme
  bar_plot_theme_2()+
  theme(
    strip.text   = element_textbox(size = 12,
                                   face   = 'bold',
                                   color  = text_col,
                                   hjust  = 0.5,
                                   halign = 0.5,
                                   fill   = "transparent"),
    
    panel.spacing.x = unit(.85, 'lines')
    
  )

p8


# saving plot
ggsave(path = here("02_img/"),
       filename = "Q8.png", device = "png", plot = p8,   
       width = 9, height = 9, units = 'in', dpi = 320)  





