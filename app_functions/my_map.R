my_map <- function(x, 
                   affil, 
                   mapping_var = 'job_role', 
                   view, 
                   legend_title){
  
  # arguments
  # x: data frame with data for points
  # affil: data frame with names and locations for labels (affiliations/institutions)
  # mapping_var: which variable to plot on map (symbol-colour)
  # view: list with latitude, longitude and zoom to set initial map view
  # legend_title: title for legend of symbol colours

  
  # ----- load libraries -----
  require(tidyverse)
  require(leaflet)
  require(randomcoloR)
  
  
  # subset affil for labeling affiliations dependent on x (which ones are in the data chosen for plotting)
  affil_subset <- affil[paste(affil$affiliation, affil$campus) %in% 
                          unique(paste(x$affiliation, x$campus)),]
  
  # ----- make mapping_var a factor -----
  # replace NA values with Unknown
  x[is.na(x[mapping_var]), mapping_var] <- 'Unknown'
  x[mapping_var] <- factor(x[[mapping_var]])  # factor needs a vector as argument, x[[]] returns vector (x[] returns a tibble)
  
  
  # ----- define one colour per var level -----
  # make colour palette with levels according to nr of levels of variable to plot
  set.seed(10)   #6,7,8,9 set seed to get reproducible random colours
  var_colourP <- randomcoloR::distinctColorPalette(k = length(levels(x[[mapping_var]])))
  # make dataframe with variable levels and variable colours
  var_colourP_df <- dplyr::tibble(var_colourP, 
                                  levels(x[[mapping_var]])) 
  # add column names (using content of mapping_var as column name, only works this way)
  names(var_colourP_df) <- c('var_colourP', mapping_var)
  # join colour dataframe with mydata
  x_col <- merge(x, 
                 var_colourP_df,
                 by = mapping_var)
  
  # ----- define icons for map -----
  my_icons <- leaflet::awesomeIcons(icon = "user",
                           iconColor = x_col$var_colourP,
                           library = "fa",
                           markerColor = "white")
  
  
  # ----- make the map -----
  # define popup variable and label
  x_col$popup_var <- paste(x_col$activity_type, 
                           paste(lubridate::month(x_col$start_date, label = T), lubridate::year(x_col$start_date)), 
                           sep = '<br/>')
  if(mapping_var != 'domain'){
   x_col$label_var <- x_col[[mapping_var]]
  } else {
   x_col$label_var <- x_col$discipline
  }

    my_map <- leaflet::leaflet(x_col) %>%
      
      # create worldmap
      leaflet::addTiles() %>% 
      
      # set which aprt of map is showing and the zoom level
      leaflet::setView(lat = view$lat, 
                       lng = view$long, 
                       zoom = view$zoom) %>% 
      
      # define map pane for labels to control overlay on map
      leaflet::addMapPane("map_labels", 
                          zIndex = 450) %>%
      
      # ----- add labels for affiliations -----
      leaflet::addLabelOnlyMarkers(lng = affil_subset$long, 
                                   lat = affil_subset$lat, 
                                   label = ~paste(affil_subset$affiliation, ' (', affil_subset$campus, ')', sep=''),  
                                   labelOptions = labelOptions(noHide = T, 
                                                               direction = 'top',
                                                               textOnly = F, 
                                                               style = list(
                                                                 "color" = "gray",
                                                                 "font-family" = "sans-serif",
                                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                 "font-size" = "12px"
                                                                 ),
                                                               opacity = 0.8,
                                                               offset = c(0,-20),
                                                               # make these labels part of pane 'labels' to control overlay on map
                                                               pane = "map_labels"
                                   ), #end labelOptions
                                   clusterOptions = markerClusterOptions(
                                     showCoverageOnHover = FALSE,
                                     # making the cluster markers invisible, so that only labels become visible when zoomed into the awesomeMarkers
                                     iconCreateFunction=JS("function (cluster) { 
                                                                   c='rgb(0,0,0,0);'    
                                                                   return new L.DivIcon({ html: '<div style=\"background-color:'+ c +'\"><span>' +  '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"
                                                           ) #end iconCreate
                                     ) #end ClusterOptions
                                   ) %>% #end addLabelOnlyMarkers
      
      # add markers to map
      leaflet::addAwesomeMarkers(lng = ~long, 
                                 lat = ~lat,
                                 popup = ~popup_var,
                                 label = ~label_var,
                                 icon = my_icons, 
                                 clusterOptions = 
                                   markerClusterOptions(zoomToBoundsOnClick = TRUE, 
                                             # Change colour of cluster markers to grey and dark grey so that it doesn't confuse the palette                                                          
                                             iconCreateFunction=JS("function (cluster) {    
                                                                   var childCount = cluster.getChildCount();  
                                                                   if (childCount < 100) {  
                                                                   c = 'rgba(169, 169, 169, 1.0);'
                                                                   } else if (childCount < 1000) {  
                                                                   c = 'rgba(105, 105, 105, 1);'  
                                                                   } else { 
                                                                   c = 'rgba(241, 128, 23, 1);'  
                                                                   }    
                                                                   return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}")
                                             ) # end cluster options
                      ) # end  awesome markers
  

  # ----- add legend to my map -----
  my_map <- my_map %>% 
      leaflet::addLegend("bottomright", 
              colors = var_colourP, 
              labels = levels(x_col[[mapping_var]]),
              title = paste(legend_title, "of beneficiaries", "<br> (Click to view individuals<br> or hover for details)<br>"),
              opacity = 2)
  
  return(my_map)
  } #end of function
