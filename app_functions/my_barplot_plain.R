# function to make a standard stacked barplot with RCCPII data for download

my_barplot_plain <- function(x, 
                             x_var = 'activity_type', 
                             group_var = 'zero', 
                             axis_label = '', 
                             legend_title = group_var){
  
  # ----- arguments: -----
  # x: dataframe or tibble
  # x_var: name of variable to be plotted on x-axis
  # group_var: name of variable whose groups will be shown stacked and coloured
  # legend_title: title for legend of stacked bars
  
  
  # ----- requires the following libraries -----
  # 'dplyr', 'randomcoloR', 'ggplot2'
  require(dplyr)
  require(ggplot2)
  require(randomcoloR)
  
  
  # ----- prepare data -----
  # (note: can use [[]] instead of $ --> returns vector, using [] returns data.frame!)
  # subset data
  x <- x[c(x_var,group_var)]
  
  # replace missing values with 'Unknown'
  x[is.na(x)] <- 'Unknown'
  
  # make x_var and group_var a factor
  x <- dplyr::as_tibble(lapply(x, factor))
  
  # make colour palette with levels according to nr of levels of group_var to plot
  set.seed(10)   # set seed to get reproducible random colours
  var_colourP <- randomcoloR::distinctColorPalette(k = length(levels(x[[group_var]])))
  
  # make static text label to show total count of x_var on top of bar
  text_label <- x %>% 
    # count per x_var
    dplyr::group_by_(x_var) %>%   # use group_by_ to evaluate content of x_var
    dplyr::tally() %>% 
    dplyr::ungroup() %>%
    dplyr::rename(n_tot = n) %>% 
    dplyr::arrange(desc(n_tot)) %>% 
    dplyr::mutate(labels_static = as.character(n_tot))
  
  # make interactive labels for each group to be shown when hovering the mouse
  plot_data <- x %>% 
    # count per x_var and group_var
    dplyr::group_by_(x_var, group_var) %>%   # use group_by_ to evaluate content of x_var/group_var
    dplyr::tally() %>% 
    dplyr::ungroup() %>% 
    merge(x, by = c(x_var, group_var)) %>% 
    merge(text_label, by = x_var)
  # make the interactive labels
  plot_data$labels_int <- paste(as.character(plot_data[['n']]), plot_data[[group_var]])
  
  # reorder data for plotting, so that biggest bar on top, except for quarter (keep time order)
  if(!(x_var %in% c('quarter'))){
    plot_data[x_var] <- reorder(plot_data[[x_var]], rowSums(plot_data['n_tot']))
  }
  
  
  # ----- make the plot -----
  ggplot2::ggplot() +
  
      # make barplot, use aes_string to evaluate content of x_var/group_var, similar to group_by_
    ggplot2::geom_bar(data = plot_data, 
                      aes_string(x = x_var, 
                                 fill = group_var), 
                      stat = "count", 
                      position = position_stack(reverse = TRUE),
                      show.legend = ifelse(group_var == 'zero', FALSE, TRUE)) +     # only shwo legend for stacked bars
    
    # use previously defined colours for filling of bars, define legend-title
    ggplot2::scale_fill_manual(values = var_colourP, 
                               guide = guide_legend(title = legend_title)) +
    
    # make static text labels to show count per bar
    ggplot2::geom_text(data = text_label, 
                       aes_string(x = x_var, 
                                  y = 'n_tot', 
                                  label = 'labels_static'), 
                       nudge_y = 0, 
                       hjust = 'inward', 
                       size = 3) +
    
    # define axis labels
    ggplot2::labs(y = axis_label, 
                  x = '') +
    
    # flip coordinates, so x_var will be on vertical (y) axis, better readability of axis labels ...
    ggplot2::coord_flip() +
    
    # add theme
    ggplot2::theme_classic()


}# end of function

# # example: 
# my_barplot(mydata, 'activity_role', 'gender')

