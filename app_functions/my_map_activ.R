my_map_activ <- function(x, 
                         affil, 
                         mapping_var = 'activity_type', 
                         view, 
                         legend_title = mapping_var){
  
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
  
  # subset x for labeling hosts
  x_hosts <- x %>% 
    filter(participation_type != 'online') %>%   # don't label online events
    select(host, venue_lat, venue_lon)
  x_hosts <- x_hosts[!duplicated(x_hosts),]  # remove duplicated entries, otherwise labels wont show
  
  # ----- make mapping_var a factor -----
  # replace NA values with Unknown
  x[is.na(x[mapping_var]), mapping_var] <- 'Unknown'
  x[mapping_var] <- factor(x[[mapping_var]])  # factor needs a vector as argument, x[[]] returns vector (x[] returns a tibble)
  
  # define popup variable and label
  x$popup_var <- paste(paste('<b><a href=', x$web_url, ' rel="noopener noreferrer" target="_blank">', x$activity_type, '</a></b>', sep=''),
                           x$host,
                           paste(lubridate::month(x$start_date, label = T), lubridate::year(x$start_date)), 
                           ifelse(!is.na(x$news_url), 
                                  paste('<b><a href=', x$news_url, ' rel="noopener noreferrer" target="_blank">read more</a></b>', sep=''), 
                                  ""),
                           sep = '<br/>')
  x$label_var <- x$activity_type
  
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
  # subset for in person events and online events to plot them in different layers
  x_col_person <- merge(filter(x, participation_type == 'in person'), 
                        var_colourP_df, 
                        by = mapping_var)
  x_col_online <- merge(filter(x, participation_type == 'online'), 
                        var_colourP_df, 
                        by = mapping_var)
    
  
  # ----- define icons for map -----
  my_icons_person <- leaflet::awesomeIcons(icon = "users",  # for online events use 'laptop'
                                    iconColor = x_col_person$var_colourP,
                                    library = "fa",
                                    markerColor = "white")
  my_icons_online <- leaflet::awesomeIcons(icon = "laptop", 
                                           iconColor = x_col_online$var_colourP,
                                           library = "fa",
                                           markerColor = "white")
  
  # ----- make the map -----
  my_map <- leaflet::leaflet() %>%
    
    # create worldmap
    leaflet::addTiles() %>% 
    
    # set which part of map is showing and the initial zoom level
    leaflet::setView(lat = view$lat, 
                     lng = view$long, 
                     zoom = view$zoom) %>% 
    
    # define map pane for labels (affiliations) to control overlay on map
    leaflet::addMapPane("map_labels", 
                        zIndex = 450) %>%
    
    # ----- add labels for hosts -----
    leaflet::addLabelOnlyMarkers(data = x, 
                                 lng = x_hosts$venue_lon, 
                                 lat = x_hosts$venue_lat, 
                                 label = ~x_hosts$host,  
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
    
    # add markers for in person events to map
    leaflet::addAwesomeMarkers(data = x_col_person,
                               lng = ~venue_lon, 
                               lat = ~venue_lat, 
                               popup = ~popup_var,
                               label = ~label_var,
                               icon = my_icons_person, 
                               group = "in person",
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
                                                                   return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"))
                               ) %>%  #end cluster options 
    
    # add markers for online events to map
    leaflet::addAwesomeMarkers(data = x_col_online,
                               lng = ~venue_lon, 
                               lat = ~venue_lat, 
                               popup = ~popup_var,
                               label = ~label_var,
                               icon = my_icons_online, 
                               group = "online",
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
                                                                   return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"))
                               ) %>%  #end cluster options
    
    # add label for online position
    leaflet::addLabelOnlyMarkers(data = x_col_online,
                                 lng = ~venue_lon[1], 
                                 lat = ~venue_lat[1], 
                                 label = 'Online events',  
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
                                                             offset = c(0,-15),
                                                             # make these labels part of pane 'labels' to control overlay on map
                                                             pane = "map_labels"
                                 ), #end labelOptions
                                 group = 'online') %>%
    
    # ----- add layers control to map to choose which layers (online, in person) to show -----
    addLayersControl(
      overlayGroups = c("in person", "online"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    
    # ----- add legend to my map -----
    leaflet::addLegend("bottomright", 
                       colors = var_colourP, 
                       labels = levels(x[[mapping_var]]),
                       title = paste(legend_title, "<br> (Click on marker to view details)<br>"),
                       opacity = 2)
  
  return(my_map)
} #end of function
