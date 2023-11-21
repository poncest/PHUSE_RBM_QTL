

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

# |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320) 


p6b <- q6_clean %>% 
  ggplot(aes(x = response_id, y = rbm_approaches, fill = rbm_approaches)) +    
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_discrete() +                                
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  #facet
  facet_wrap(~ stage_phase, ncol = 1)+
  
  # labs
  labs(
    x = "Response Number",
    y = "RBM Approaches",
    title = "Q6: Which aspects of Risk-Based Approaches to Quality\ndoes your company apply to the following Trial?"
  )+
  
  # theme
  theme_minimal()+
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    
    axis.title.x          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(t = 12), family = 'text'), 
    axis.title.y          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(r = 12), family = 'text'),
    
    axis.text             = element_text(size = 10, color = text_col, family = 'text'),
    
    axis.line.x           = element_line(color = "grey80", linewidth = .4),
    axis.line.y           = element_blank(),
    
    strip.text            = element_textbox(size = 12,
                                            face   = 'bold',
                                            color  = text_col,
                                            hjust  = 0.5,
                                            halign = 0.5,
                                            fill   = "transparent"),

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
       filename = "Q06b.png", device = "png", plot = p6b,   
       width = 8, height = 10, units = 'in', dpi = 320)  




## # Question 06 (Alternative) ---- 
## This one might be a better option

p6b_02 <- q6_clean %>% 
  ggplot(aes(x = response_id, y = stage_phase, fill = stage_phase))+
  
  # geoms
  ggdist::geom_dots(smooth      = "bar", 
                    layout      = "bin", 
                    orientation = "y",
                    position    = "identity", 
                    dotsize     = 0.8,
                    shape       = 22) +
  
  # scale
  scale_x_discrete() +                                
  scale_y_discrete(expand = c(0.01, 0.5))+
  urbnthemes::scale_color_discrete()+
  coord_equal(clip = 'off')+
  
  #facet
  facet_wrap(~ rbm_approaches, ncol = 1)+
  
  # labs
  labs(
    x = "Response Number",
    y = "RBM Approaches",
    title = "Q6: Which aspects of Risk-Based Approaches to Quality\ndoes your company apply to the following Trial?"
  )+
  
  # theme
  theme_minimal()+
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    
    axis.title.x          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(t = 12), family = 'text'), 
    axis.title.y          = element_text(size = 12, face = 'bold', color = text_col, margin = margin(r = 12), family = 'text'),
    
    axis.text             = element_text(size = 10, color = text_col, family = 'text'),
    
    axis.line.x           = element_line(color = "grey80", linewidth = .4),
    axis.line.y           = element_blank(),
    
    strip.text            = element_textbox(size = 12,
                                            face   = 'bold',
                                            color  = text_col,
                                            hjust  = 0.5,
                                            halign = 0.5,
                                            fill   = "transparent"),
    
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
       filename = "Q06b_02.png", device = "png", plot = p6b_02,   
       width = 8, height = 10, units = 'in', dpi = 320)  




# Question 23 ----
## (Alternative) ---- 
## This one might be a better option

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


