

#' Author:       Steven Ponce
#' Created Date: 2023-11-02


# Summary Functions ----

# Group by and then Summarize ----
bar_summary <- function(tbl, group_col) {
  
  tbl %>% 
    
    # summary
    group_by( {{ group_col }}) %>% 
    summarise(
      count   = n(),
      .groups = "drop",
      ) %>% 

    mutate(
      pct = count / sum(count),
      total_count = sum(count)
      ) %>% 
    
    # arrange
    arrange(desc(count))
  
}


# Summarize only ----
bar_summary_2 <- function(tbl) {
  
  tbl %>% 
    
    # summary
    summarise(
      count   = n(),
      .groups = "drop",
    ) %>% 
    
    mutate(
      pct = count / sum(count),
      total_count = sum(count)
    ) %>% 
    
    # arrange
    arrange(desc(count))
  
}


  
# Plot Function ----

# Bar Plot ----

bar_plot <- function(tbl, x_variable, y_variable, label_adjust, col_width){
  
  tbl %>% 
    ggplot(aes(x = {{ x_variable }}, y = {{ y_variable }}))+
    
    # Geoms
    geom_col(width = {{ col_width }}, fill = bar_plot_col)+
    
    geom_textbox(aes(label = paste0("<span style='font-size:14pt'><br>",
                                    bar_label,"</span>",
                                    " (n = ", count, ")"),
                     
                     hjust =  case_when(pct < {{ label_adjust }} ~ 0,
                                        TRUE ~ 1),
                     halign = case_when(pct < {{ label_adjust }} ~ 0,
                                        TRUE ~ 1),
                     colour = case_when(pct < {{ label_adjust }} ~ "gray20",
                                        TRUE ~ "#fdfdff")),
                 fill = NA,
                 box.colour = NA,
                 size = 3.5,
                 fontface = "bold") +
    
    # Scales
    scale_colour_identity() +    # bar label color
    coord_flip()
  
}

# smaller font size
bar_plot_2 <- function(tbl, x_variable, y_variable, label_adjust, col_width){
  
  tbl %>% 
    ggplot(aes(x = {{ x_variable }}, y = {{ y_variable }}))+
    
    # Geoms
    geom_col(width = {{ col_width }}, fill = bar_plot_col)+
    
    geom_textbox(aes(label = paste0("<span style='font-size:10pt'><br>",
                                    bar_label,"</span>",
                                    " (n = ", count, ")"),
                     
                     hjust =  case_when(pct < {{ label_adjust }} ~ 0,
                                        TRUE ~ 1),
                     halign = case_when(pct < {{ label_adjust }} ~ 0,
                                        TRUE ~ 1),
                     colour = case_when(pct < {{ label_adjust }} ~ "gray20",
                                        TRUE ~ "#fdfdff")),
                 fill = NA,
                 box.colour = NA,
                 size = 3.5,
                 fontface = "bold") +
    
    # Scales
    scale_colour_identity() +    # bar label color
    coord_flip()
  
}



# Bar Plot Theme ---
bar_plot_theme <- function() {
  
  # Theme
  theme_minimal(base_size = 12)+
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position       = 'plot',
      
      plot.background       = element_rect(fill = bkg_col, color = bkg_col),
      panel.background      = element_rect(fill = bkg_col, color = bkg_col),
        
      plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
      
      panel.grid.minor      = element_blank(),
      panel.grid.major      = element_blank(),
        
      axis.title.x          = element_text(size = 14, face = 'bold', color = text_col, margin = margin(t = 12)), 
      axis.title.y          = element_text(size = 14, face = 'bold', color = text_col, margin = margin(r = 12)), 
        
      axis.text             = element_text(size = 10, color = text_col),
      
      axis.line.x           = element_line(color = "grey80", linewidth = .4),
      axis.line.y           = element_blank(),
    
      axis.ticks.x          = element_line(color = "grey80", linewidth = .4),
          
      plot.title            = element_text(
        family              = 'title',
        color               = title_col,
        lineheight          = 0.8,
        face                = "bold",
        size                = 16,  
        margin              = margin(t = 10, b = 15)),
          
      plot.caption          = element_markdown(
        family              = 'caption',
        color               = caption_col,
        lineheight          = 0.6,
        size                = 10,
        hjust               = 0,
        halign              = 0,
        margin              = margin(t = 10, b = 10)),
    )
  
} 

# smaller base font size
bar_plot_theme_2 <- function() {
  
  # Theme
  theme_minimal(base_size = 10)+
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position       = 'plot',
      
      plot.background       = element_rect(fill = bkg_col, color = bkg_col),
      panel.background      = element_rect(fill = bkg_col, color = bkg_col),
      
      plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
      
      panel.grid.minor      = element_blank(),
      panel.grid.major      = element_blank(),
      
      axis.title.x          = element_text(size = 14, face = 'bold', color = text_col, margin = margin(t = 12)), 
      axis.title.y          = element_text(size = 14, face = 'bold', color = text_col, margin = margin(r = 12)), 
      
      axis.text             = element_text(size = 10, color = text_col),
      
      axis.line.x           = element_line(color = "grey80", linewidth = .4),
      axis.line.y           = element_blank(),
      
      axis.ticks.x          = element_line(color = "grey80", linewidth = .4),
      
      plot.title            = element_text(
        family              = 'title',
        color               = title_col,
        lineheight          = 0.8,
        face                = "bold",
        size                = 16,  
        margin              = margin(t = 10, b = 15)),
      
      plot.caption          = element_markdown(
        family              = 'caption',
        color               = caption_col,
        lineheight          = 0.6,
        size                = 10,
        hjust               = 0,
        halign              = 0,
        margin              = margin(t = 10, b = 10)),
    )
  
} 
