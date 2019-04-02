#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# This app explores data of events(activities) and participants(beneficiaries) of the RCCPII
# capacity development initiative 2018/19, 
# more information about the project: https://tenet-rccpii.github.io/rccpii-2018/
#

# ----- load libraries -----
library(shiny)
library(leaflet)
library(tidyverse)
library(Cairo)
library(grid)
library(cowplot)
library(magick)
library(RCurl)

# usecairo = T from package Cairo for better quality of figures in shiny app
options(shiny.usecairo=T)

# ----- load data -----
# download from figshare
fs_url <- "https://ndownloader.figshare.com/files"

# data of beneficiaries
benef <- readr::read_csv(paste(fs_url, '14752466', sep = '/'))

# data for affiliations
affil <- readr::read_csv(paste(fs_url, '14752463', sep = '/')) %>% 
  select(-city)

# data of activities
activ <- readr::read_csv(paste(fs_url, '14752469', sep = '/')) %>% 
  # ----- make variable quarter for 2018/19 project, 5 quarters in total ---
  mutate(quarter = lubridate::quarter(start_date, 
                                      with_year = TRUE, 
                                      fiscal_start = 1))

# data of participation of beneficiaries in activities
parti <- readr::read_csv(paste(fs_url, '14752460', sep = '/')) %>% 
  select(-participation_type)

# join into one dataframe
mydata <- benef %>% 
  merge(parti, by = 'identifier', all = T) %>% 
  merge(affil, by = c('affiliation', 'campus'), all = T) %>% 
  merge(activ, by = 'activity_id', all = T)

# ----- # make general domains for disciplines ---
disciplines <- na.omit(unique(mydata$discipline))
#create function to use with lapply
fct_domain <- function(x){
  if(x %in% c('Humanities','Social Sciences', 'Libraries')){
    domain <- 'Liberal Arts'
  }else if(x %in% c('Formal Sciences', 'Applied Sciences', 'Natural Sciences')){
    domain <- 'Sciences'
  }else if(x %in% c('IT', 'Research Office', 'University Administration', 'Teaching and Learning')){
    domain <- 'Support'
  }else if(x %in% c('Industry')){
    domain <- 'Industry'
  }else if(x %in% c('Unknown','Undisclosed')){
    domain <- 'Unknown'
  }
}
domains <- unlist(lapply(disciplines, fct_domain))
# add domains variable to mydata
domains <- tibble(discipline = disciplines, 
                  domain = domains)
mydata <- mydata %>% 
  merge(domains, by = 'discipline')

# add a helping 'zero' variable to allow use of stacked barplot function for drawing simple barplot
mydata$zero <- 'zero'

# ----- load plot functions -----
source('app_functions/my_map.R')
source('app_functions/my_map_activ.R')
source('app_functions/my_barplot.R')
source('app_functions/my_barplot_plain.R')


# define names for labels of the different categories in mydata
group_names <- tibble(col_names = colnames(mydata[c(1,6:10,14,19,21,25,26)]), 
                      group_names = c('Discipline','Country based in', 'Gender', 'Race',
                                      'Job Role', 'Role in Activity', 'Type of Activity',
                                      'Type of Participation','Host','Quarter','Domain'))
# choices for beneficiary map and barplot
variables_map <- group_names$group_names
variables_bar <- group_names$group_names
# choices for activities map and barplot
variables_actm <- c('Type of Activity', 'Quarter', 'Host')
variables_actb <- c('Type of Activity', 'Quarter', 'Host', 'Type of Participation')
var_quarter <- c(2018.1,2018.2,2018.3,2018.4,2019.1)

# function to convert variable choice (input) to correct column name in mydata
choice <- function(x){
  group_names$col_names[group_names$group_names == x]
}

# ----- start with shiny app specific code ----
# ----- Define UI -----
ui <- fluidPage(
  
  # set styles for app
  tags$head(
    tags$style(HTML("
      * {
        font-family: 'Arial';    # * means Arial will be used for all text, otherwise can use h1, p ...
      }
    ")),
    
    # for responsive iframe embedding on webpage
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript")
  ),
   
  
   # Application title
   titlePanel("Activities and beneficiaries of the RCCPII Capacity Development Initiative 2018/19"),
   
   tabsetPanel(
     # ----- start first panel -----
     tabPanel('Activities Map',
              # Sidebar with a dropdown menu to choose which variable to plot 
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = 'variable_act', 
                              label = 'Choose which variable to plot', 
                              choices = variables_actm, 
                              selected = 'Type of Activity'
                  ), # end selectInput
                  # add filter for data
                  selectInput(inputId = 'var_filter1', 
                              label = 'Choose filter for data', 
                              choices = c('Quarter', 'Type of Activity')
                  ), # end selectInput
                  conditionalPanel(condition = "input.var_filter1.includes('Quarter')",
                                   checkboxGroupInput(inputId = 'quarter1', 
                                                      label = 'Choose which quarter to show',
                                                      choices = var_quarter,
                                                      selected = var_quarter)
                  ), # end conditionalPanel
                  conditionalPanel(condition = "input.var_filter1.includes('Type of Activity')",
                                   checkboxGroupInput(inputId = 'activity1', 
                                                      label = 'Choose which activity to show',
                                                      choices = unique(mydata$activity_type),
                                                      selected = unique(mydata$activity_type))
                  ), # end conditionalPanel
                  # add reset button for map view
                  actionButton("reset_button1", "Reset map view")
                ), # end sidebarPanel
                
                # Show the generated leflet map
                mainPanel(
                  # add some text on top of main part
                  h4('Who were the different hosts of activities in 2018/19? Explore the details of activities!'),
                  p("Choose", strong(em('Host')), 'from the first dropdown menu. See the new legend with all hosts in the map.'),
                  p('Look for the event at Durban University of Technology - click on the circle in KwaZulu-Natal and then in Durban, then click the icon for Durban University of Technology
                    and click the link',  strong(em("Carpentries workshop")), 'or', strong(em('read more')), 'to learn more about the event.'),
                 
                  # add the map
                  leaflet::leafletOutput(outputId = "my_map_activities",
                                         height = "65vh")  # use height argument to adjust size of map in app
                ) # end mainPanel
                
              ) # end sidebarLayout
     ), # end tabPanel activities map
     
     
     # ----- start second panel -----
     tabPanel('Activities Statistics',
              # Sidebar with a dropdown menu to choose which variable to plot 
              sidebarLayout(
                sidebarPanel(
                  # 1st input for vertical axis
                  selectInput(inputId = 'x_var_act', 
                              label = 'Choose by which variable to create the bars',
                              choices = variables_actb,
                              selected = 'Type of Activity'
                  ),  # end selectInput
                  # add checkbox whether second variable should be shown as stacked color bars
                  checkboxInput(inputId = 'sub_group_act', 
                                label = 'Show second variable as stacked bars', 
                                value = FALSE),
                  conditionalPanel(condition = "input.sub_group_act",
                                   # 2nd input for stacking of bargraphs
                                   selectInput(inputId = 'group_var_act', 
                                               label = 'Choose by which variable to stack the bars',
                                               choices = variables_actb,
                                               selected = 'Quarter'
                                   ) # end selectInput
                  ), # end conditionalPanel
                  # a download button to download plot
                  downloadButton(outputId = 'downloadPlot_act', 
                                 label = "Download plot")
                ), # end sidebarPanel
              
                # Show the generated barplot
                mainPanel(
                  # add some text
                  h4('How many and what different activities were supported by RCCPII in 2018/19?'),
                  p("Choose", strong(em('Type of Activity')), 'from the first dropdown menu, then tick 
                    box', strong(em('Show second variable as stacked bars')), 'and choose', strong(em('Type of Participation')), "from the 
                    second dropdown menu."),
                  p("Trainer Mentoring and", a(href = "https://carpentries.org", "Carpentries Workshops"), "were the most common activities. Trainer Mentoring and eConversations were facilitated online, through the vidyo platform."),
                  br(),
                  # add title for plot
                  h5(strong(textOutput(outputId = 'caption_barplot_act')), 
                     align = 'center'),
                  div(textOutput(outputId = 'description_barplot_act'), 
                      align = 'center'),
                  # add plot
                  ggiraph::ggiraphOutput(outputId = "my_barplot_act")
                ) # end mainPanel
              ) # end sidebarLayout
     ), # end tabPanel barplot activities
     
     
     # ----- start third panel -----
     tabPanel('Beneficiaries Map',
              # Sidebar with a dropdown menu to choose which variable to plot 
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = 'variable', 
                              label = 'Choose which variable to plot', 
                              choices = variables_map, 
                              selected = 'Job Role'
                  ), # end selectInput
                  # add filter for data
                  selectInput(inputId = 'var_filter', 
                              label = 'Choose filter for data', 
                              choices = c('Quarter', 'Type of Activity')
                  ), # end selectInput
                  conditionalPanel(condition = "input.var_filter.includes('Quarter')",
                                   checkboxGroupInput(inputId = 'quarter', 
                                                      label = 'Choose which quarter to show',
                                                      choices = var_quarter,
                                                      selected = var_quarter)
                  ), # end conditionalPanel
                  conditionalPanel(condition = "input.var_filter.includes('Type of Activity')",
                                   checkboxGroupInput(inputId = 'activity', 
                                                      label = 'Choose which activity to show',
                                                      choices = unique(mydata$activity_type),
                                                      selected = unique(mydata$activity_type))
                  ), # end conditionalPanel
                  # add button for reset of map view
                  actionButton(inputId = "reset_button2", 
                               label = "Reset map view")
                ), # end sidebarPanel
                
                # Show the generated leflet map
                mainPanel(
                  # add some text
                  h4('Where did beneficiaries came from for the', strong(em('Blended Learning SIG')), 'events and what were their job roles?'),
                  p("Choose", strong(em('Job Role')), 'from the first dropdown menu. Under', strong(em('Choose filter for data')), 'choose',
                  strong(em('Type of Activity')), 'then untick all checkboxes except the one for', strong(em('Blended Learning SIG'))),
                  p('The map now only shows beneficiaries from the', em('Blended Learning SIG'), 'events. Locations close to each other are grouped together.
                    To ungroup, click on a gray circle and see individual locations, click again to see individual beneficiaries at one location. Click on', 
                    strong(em('Reset map view')), 'to get back to the orginal view.'),
                  
                  # add the map
                  leaflet::leafletOutput(outputId = "my_map_beneficiaries", 
                                         height = "65vh")  # use height argument to adjust size of map in app
                ) # end mainPanel
                
              ) # end sidebarLayout
     ), # end tabPanel beneficiaries map
     
     
     # ----- start fourth panel -----
     tabPanel('Beneficiaries Statistics',
              # Sidebar with a dropdown menu to choose which variable to plot 
              sidebarLayout(
                sidebarPanel(
                  # 1st input for vertical axis
                  selectInput(inputId = 'x_var', 
                              label = 'Choose by which variable to create the bars',
                              choices = variables_bar,
                              selected = 'Type of Activity'
                  ), # end selectInput
                  
                  # add checkbox whether second variable should be shown as stacked color bars
                  checkboxInput(inputId = 'sub_group', 
                                label = 'Show second variable as stacked bars', 
                                value = FALSE),
                  conditionalPanel(condition = "input.sub_group",
                                   # 2nd input for stacking of bargraphs
                                   selectInput(inputId = 'group_var', 
                                               label = 'Choose by which variable to stack the bars',
                                               choices = variables_bar,
                                               selected = 'Job Role'
                                   ), # end selectInput
                                   # add checkbox for disciplines and filter for domains
                                   conditionalPanel(condition = "input.group_var.includes('Domain')",
                                                    checkboxInput(inputId = 'sub_domain', 
                                                                  label = 'Show individual disciplines', 
                                                                  value = FALSE),
                                                    conditionalPanel(condition = "input.sub_domain",
                                                                     checkboxGroupInput(inputId = 'domain', 
                                                                                        label = 'Choose which domains to show',
                                                                                        choices = unique(mydata$domain),
                                                                                        selected = unique(mydata$domain)
                                                                     )
                                                    ) # end conditionalPanel2
                                   ) # end conditionalPanel1
                  ), # end conditionalPanel sub_group 
                  
                  # add filter for data
                  selectInput(inputId = 'var_filter3', 
                              label = 'choose filter for data', 
                              choices = c('Quarter', 'Type of Activity')
                  ), # end selectInput
                  conditionalPanel(condition = "input.var_filter3.includes('Quarter')",
                                   checkboxGroupInput(inputId = 'quarter3', 
                                                      label = 'Choose which quarter to show',
                                                      choices = var_quarter,
                                                      selected = var_quarter)
                  ), # end conditionalPanel
                  conditionalPanel(condition = "input.var_filter3.includes('Type of Activity')",
                                   checkboxGroupInput(inputId = 'activity3', 
                                                      label = 'Choose which activity to show',
                                                      choices = unique(mydata$activity_type),
                                                      selected = unique(mydata$activity_type))
                  ), # end conditionalPanel
                  
                  # add download button for plot
                  downloadButton(outputId = 'downloadPlot', 
                                 label = "Download plot")
                ), # end sidbarPanel
                
                
                # Show the generated barplot
                mainPanel(
                  # add some text
                  h4('What was the attendance of online vs in-person events in regard to gender?'),
                  p("Choose", strong(em('Type of Participation')), 'from the first dropdown menu, then tick 
                  box', strong(em('Show second variable as stacked bars')), 'and choose', strong(em('Gender')), "from the 
                  second dropdown menu."),
                  p("Do you notice that more men than women attended in-person events, 
                  but more women than men attended online events?"),
                  br(),
                  # add title for plot
                  h5(strong(textOutput(outputId = 'caption_barplot')), 
                     align = 'center'),
                  div(textOutput(outputId = 'description_barplot'), 
                      align = 'center'),
                  # add plot
                  conditionalPanel(condition = "input.group_var != 'Domain' | (input.group_var == 'Domain' & ! input.sub_domain)",
                                   ggiraph::ggiraphOutput(outputId = "my_barplot")
                                   ), 
                  conditionalPanel(condition = "input.group_var == 'Domain' & input.sub_domain",
                                   ggiraph::ggiraphOutput(outputId = "barplot_disciplines")
                                  )
                ) # end mainPanel
                
              ) # end sidebarLayout
     ) # end tabPanel barplot beneficiaries
     
   ), # end tabsetPanel
  
  HTML('<div data-iframe-height></div>')
  
) # end fluidPage


# ----- Define server logic -----
# required to draw the leaflet map and barplots
server <- function(input, output) {
  
  # ----- define original view for maps ---
  view_orig <- list(long = 24.774610, lat = -29.038968, zoom = 5)
  
  # ----- output for first panel (activities map) -----
  output$my_map_activities <- leaflet::renderLeaflet({
    # filter the data according to input$filter
    if(input$var_filter1 == 'Quarter'){
      mydata_choice <- activ %>%
        filter(quarter %in% input$quarter1)
    }else{
      mydata_choice <- activ %>% 
        filter(activity_type %in% input$activity1)
    } #end if else
    # draw the map,
    # (input$variable corresponds to the category chosen in the drop-down menu)
    my_map_activ(x = mydata_choice, 
                 affil = affil, 
                 mapping_var = choice(input$variable_act), 
                 view = view_orig, 
                 legend_title = input$variable_act
                 )
  }) # end my_map
  
  # set reset button for activities map
  observe({
    input$reset_button1
    leafletProxy("my_map_activities") %>% 
      setView(lat = view_orig$lat, 
              lng = view_orig$long, 
              zoom = view_orig$zoom)
  })
  
  
  # ----- output for second panel (barplot) -----
  # define plot inputs as reactive elements so they can be used by renderPlot and downloadHandler ...
  # ----- caption for barplot ---
  output$caption_barplot_act <- renderText({
      ifelse(!input$sub_group_act, 
             # caption for simple barplot
             paste("Number of activities by", input$x_var_act), 
             # caption for stacked barplot
             paste("Number of activities by", input$x_var_act, 'and', input$group_var_act)
      )
  })
  # subtitle for stacked barplot
  output$description_barplot_act <- renderText({
    ifelse(!input$sub_group_act, 
           # no subtitle for simple plot
           '', 
           # subtitle for stacked plot
           '(hover over the bars to see numbers in individual categories)'
    )
  })
  
  # ----- make barplot for activities---
  output$my_barplot_act <- ggiraph::renderggiraph({
    # make data a data.frame (is tibble before) to allow for same x_var and group_var (throughs an error when tibble)
    plot_data <- data.frame(activ)
    plot_data$zero <- 'zero'  # make a zero helper variable which acts as a place holder for group_var for simple plot
    
    pl <- my_barplot(x = plot_data, 
                     x_var = choice(input$x_var_act), 
                     group_var = ifelse(input$sub_group_act, 
                                        choice(input$group_var_act), 
                                        'zero'),
                     axis_label = 'No of activities',
                     legend_title = input$group_var_act
                     )
    # set appearance of plot
    pl <- girafe_options(x = pl, 
                         opts_toolbar(saveaspng = FALSE), 
                         opts_tooltip(css = "background-color:white;font-style:bold;") 
                         #opts_zoom(min = 0.5, max = 5)
    )
    print(pl)
  }) # end my_barplot
  
  # ----- Download button for download of graphs -----
  # create non-interactive plot for download
  plotInput_act <- function(){
    # make plot
    plot_data <- data.frame(activ)
    plot_data$zero <- 'zero'
    p <- my_barplot_plain(x = plot_data, 
                          x_var = choice(input$x_var_act), 
                          group_var = ifelse(input$sub_group_act, 
                                             choice(input$group_var_act), 
                                             'zero'),
                          axis_label = 'No of activities',
                          legend_title = input$group_var_act
    ) +
      # add title for downloaded plot
      labs(title = ifelse(!input$sub_group_act, 
                          paste("Number of activities by", input$x_var_act), 
                          paste("Number of activities by", input$x_var_act, 'and', input$group_var_act)
           ))
    
    # add RCCPII logo and license to plot
    g <- ggdraw(p) + 
      draw_image(image = 'app_data/logo_license.png', 
                 scale = 0.15, 
                 x = +0.4, 
                 y = -0.35, 
                 interpolate = T)
  } # end of function
  
  # download plot
  output$downloadPlot_act <- downloadHandler(
    filename = function() {
      paste('plot_activities', Sys.Date(), '.png', sep='')  # filename with current date
    },
    content = function(file) {
      # device <- function(..., width, height) {
      #  grDevices::png(..., width = width, height = height,
      #                 res = 300, units = "in")
      # }
      ggsave(file, 
             plot = plotInput_act(), 
             width = 8, height = 5)
    }       
  ) # end downloadHandler
  
  
  
  # ----- output third panel (beneficiaries map) -----
   output$my_map_beneficiaries <- leaflet::renderLeaflet({
      # filter the data according to input$filter
     if(input$var_filter == 'Quarter'){
       mydata_choice <- mydata %>%
         filter(quarter %in% input$quarter)
     }else{
       mydata_choice <- mydata %>% 
         filter(activity_type %in% input$activity)
     } #end if else
      # draw the map,
     # (input$category corresponds to the category chosen in the drop-down menu)
      my_map(x = mydata_choice, 
             affil = affil, 
             mapping_var = choice(input$variable), 
             view = view_orig, 
             legend_title = input$variable
             ) 
   }) # end my_map
  
  # reset buttion for beneficiaries map
  observe({
    input$reset_button2
    leafletProxy("my_map_beneficiaries") %>% 
      setView(lat = view_orig$lat, 
              lng = view_orig$long, 
              zoom = view_orig$zoom)
  })
   
  
   # ----- output for fourth panel (barplot) -----
   # --- caption for barplot ---
   output$caption_barplot <- renderText({
     ifelse(!input$sub_group, 
            paste("Number of beneficiaries by", input$x_var), 
            paste("Number of beneficiaries by", input$x_var, 'and', input$group_var)
     )
   })
   # subtitle for stacked barplot
  output$description_barplot <- renderText({
    ifelse(!input$sub_group, 
           '', 
           '(hover over the bars to see numbers in individual categories)'
           )
    })
   
      
   # ----- make barplot ---
   output$my_barplot <- ggiraph::renderggiraph({
     # filter the data according to input$filter3
     if(input$var_filter3 == 'Quarter'){
       mydata_choice <- mydata %>%
         filter(quarter %in% input$quarter3)
     }else{
       mydata_choice <- mydata %>% 
         filter(activity_type %in% input$activity3)
     } #end if else
     
     pl <- my_barplot(x = mydata_choice, 
                      x_var = choice(input$x_var), 
                      group_var = ifelse(input$sub_group, 
                                         choice(input$group_var), 
                                         'zero'),
                      axis_label = 'No of beneficiaries',
                      legend_title = input$group_var
     )
     # set appearance of plot
     pl <- girafe_options(x = pl, 
                          opts_toolbar(saveaspng = FALSE), 
                          opts_tooltip(css = "background-color:white;font-style:bold;") 
                          #opts_zoom(min = 0.5, max = 5)
                          )
     print(pl)
   }) # end my_barplot
     
   # make plot for individual disciplines if chosen
  output$barplot_disciplines <- ggiraph::renderggiraph({
    # filter data when discipline is chosen
    pl_data <- mydata %>% 
      filter(domain %in% input$domain)
    
    pl <- my_barplot(x = pl_data, 
                     x_var = choice(input$x_var), 
                     group_var = 'discipline',
                     axis_label = 'No of beneficiaries',
                     legend_title = input$group_var
                     )
    # set appearance of plot
    pl <- girafe_options(x = pl, 
                         opts_toolbar(saveaspng = FALSE), 
                         opts_tooltip(css = "background-color:white;font-style:bold;") 
                         #opts_zoom(min = 0.5, max = 5)
                         )
    print(pl)
  }) #end barplot_disciplines

  # ----- Download button for download of graphs -----
  # create non-interactive plot for download
  plotInput <- function(){
    # filter the data according to input$filter3
    if(input$var_filter3 == 'quarter'){
      mydata_choice <- mydata %>%
        filter(quarter %in% input$quarter3)
    }else{
      mydata_choice <- mydata %>% 
        filter(activity_type %in% input$activity3)
    } #end if else
    
    # make plot
    p <- my_barplot_plain(x = mydata_choice, 
                          x_var = choice(input$x_var), 
                          group_var = ifelse(input$sub_group, 
                                             choice(input$group_var), 
                                             'zero'),
                          axis_label = 'No of beneficiaries',
                          legend_title = input$group_var
                          ) +
      labs(title = ifelse(!input$sub_group, 
                          paste("Number of beneficiaries by", input$x_var), 
                            paste("Number of beneficiaries by", input$x_var, 'and', input$group_var)
            ))
    # add RCCPII logo to plot
    g <- ggdraw(p) + 
      draw_image(image = 'app_data/logo_license.png', 
                 scale = 0.15, 
                 x = +0.4, y = -0.35, 
                 interpolate = T)
  }
  
  # download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('plot_beneficiaries', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      # device <- function(..., width, height) {
      #  grDevices::png(..., width = width, height = height,
      #                 res = 300, units = "in")
      # }
      ggsave(file, 
             plot = plotInput(), 
             width = 8, height = 5)
    }       
  ) # end downloadHandler
  
}

# ----- Run the application -----
shinyApp(ui = ui, server = server)