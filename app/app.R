##-----------## VT2023 EDS -- GREAT SMOKIES PHENOLOGY APP ##-----------##
## Authors: Jason Halvis, Sean Murray, Jennifer Ochs, and Xuanang Zhao ##

# THIS SECTION IS THE SETUP, IT DOWNLOADS ALL THE NECESSARY LIBRARIES 
# AND THE DATA CAN BE IMPORTED WITH THE NPN DOWNLOAD PACKAGE,
# OR VIA A CSV STORED ON A PERSONAL DEVICE
#install.packages("ggplot2")
#install.packages("ggpmisc")

library(shiny)
library(shinydashboard)
library(markdown)
library(rmarkdown)
library(rnpn)
library(dplyr)
library(lubridate)
library(styler)
library(ggplot2)
library(bslib)
library(thematic)
library(ragg)
library(showtext)
library(tidyverse)
library(rsconnect)
library(DT)

library(ggpmisc)


rm(list = ls())

## Sample Data Files: ##
cdfa <- read.csv("www/data/cdfa.csv")
cdf2 <- read.csv("www/data/cdf2.csv")
icdf2 <- read.csv("www/data/icdf2.csv")
bivar_weather <- read.csv("www/data/bivar_weather.csv")
icdfE <- read.csv("www/data/icdfE.csv")

#____________________________________________________________________________________________________________________________#
#SHINY APP STARTS HERE
# Define UI for app that draws a plot----
tab1 <- tabPanel("Phenology Observation Tracker",
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     
                     
                     selectInput(
                       inputId = "common_name",
                       label = strong("Select Species"),
                       choices = unique(cdfa$common_name),
                       selected = "red maple"
                     ),
                     
                     helpText("NOTE: Some phenophase selections may not provide output."),

                     selectInput(
                       inputId = "phenophase_description",
                       label = strong("Select Phenophase"),
                       choices = unique(cdfa$phenophase_description),
                       selected = "Leaves"
                     ),

                     selectInput(
                       inputId = "year",
                       label = strong("Select Year"),
                       choices = unique(cdfa$year),
                       selected = "2020"
                     ),
                     
                     conditionalPanel(
                       condition = "input.show_second_plot",
                       
                       selectInput(
                         inputId = "common_name2",
                         label = strong("Select Species"),
                         choices = unique(cdfa$common_name),
                         selected = "yellow buckeye"
                       ),
                       
                       helpText("NOTE: Some phenophase selections may not provide output."),
                       
                       selectInput(
                         inputId = "phenophase_description2",
                         label = strong("Select Phenophase"),
                         choices = unique(cdfa$phenophase_description),
                         selected = "Leaves"
                       ),
                       
                       selectInput(
                         inputId = "year2",
                         label = strong("Select Year"),
                         choices = unique(cdfa$year),
                         selected = "2020"
                       )
                     ),
                     
                     checkboxInput(
                       inputId = "show_second_plot",
                       label = "Show second plot"
                     ),
                     
                     conditionalPanel(
                       condition = "input.show_third_plot",
                       
                       selectInput(
                         inputId = "common_name3",
                         label = strong("Select Species"),
                         choices = unique(cdfa$common_name),
                         selected = "sugar maple"
                       ),
                       
                       helpText("NOTE: Some phenophase selections may not provide output."),
                       
                       selectInput(
                         inputId = "phenophase_description3",
                         label = strong("Select Phenophase"),
                         choices = unique(cdfa$phenophase_description),
                         selected = "Leaves"
                       ),
                       
                       selectInput(
                         inputId = "year3",
                         label = strong("Select Year"),
                         choices = unique(cdfa$year),
                         selected = "2020"
                       )
                     ),
                     
                     checkboxInput(
                       inputId = "show_third_plot",
                       label = "Show third plot"
                     )
                   ),
                    
                   mainPanel(
                     h4("Phenophase Status by Elevation Bands", align = "center"),
                     plotOutput(outputId = "plot1"),
                     plotOutput(outputId = "plot1a"),
                     plotOutput(outputId = "plot1b")
                   )
                 ))
#################################################################################################################################
tab2 <- tabPanel("Elevation Bands Time Series",
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput(
                       inputId = "common_nameE",
                       label = strong("Select Species"),
                       choices = unique(icdfE$common_name),
                       selected = "yellow birch"
                     ),
                      
                     helpText("NOTE: Some phenophase selections may not provide output."),
                     
                     selectInput(
                       inputId = "phenophase_descriptionE",
                       label = strong("Select Phenophase"),
                       choices = unique(icdfE$phenophase_description),
                       selected = "Leaves"
                     )

                   ),
                   mainPanel(
                     h4("First yes for phenophase, 95% or more, 2010-2020", align = "center"),
                     plotOutput(outputId = "plot2")
                   )
                 ))
#################################################################################################################################
tab3 <- tabPanel("Species Time Series", 
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     
                     helpText("Select one or multiple species"),
                     selectInput(
                       inputId = "common_name4",
                       label = strong("Select Species"),
                       choices = unique(icdf2$common_name),
                       selected = "red maple"
                     ),
                     
                     selectInput(
                       inputId = "elev_bands2",
                       label = strong("Select Elevation Range"),
                       choices = c("<800m", "800-1300m", ">1300m"),
                       selected = "<800m"
                     ),
                     
                     selectInput(
                       inputId = "Phenophases_species",
                       label = strong("Select Elevation Range"),
                       choices = c("Leaves", "Breaking leaf buds", "Colored leaves"),
                       selected = "Leaves"
                     ),
                   ),
                   mainPanel(
                     h4("First leaf out for selected species in elevation groups", align = "center"),
                     plotOutput(outputId = "plot3")
                   )
                 ))
#################################################################################################################################
tab4 <- tabPanel("Bivariate", 
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     helpText("NOTE: Some data selections may not provide output."),
                    
                     ##Input: weather condition: tmin spring, tmax spring, or acc. precip.
                     selectInput(
                       inputId = "weather_condition",
                       label = strong("Select Weather Condition:"),
                       choices = c("Min. T spring" = "tmin_spring",
                                   "Max. T spring" = "tmax_spring", 
                                   "Acc. prcp." = "acc_prcp"),  
                       selected = "tmin_spring"
                     ),
                     
                     ##Input: species
                     #helpText("Select one or multiple species"),
                     selectInput(
                       inputId = "common_name_bivar",
                       label = strong("Select Species"),
                       choices = unique(bivar_weather$common_name),
                       #multiple = TRUE, 
                       selected = "red maple"
                     ),
                     
                     ##Input: site name(s)
                     selectInput(
                       inputId = "site_name_bivar",
                       label = strong("Select Site"),
                       choices = unique(bivar_weather$site_name),
                       selected = "GRSM-Tremont-Marcs Trail"
                     ),
                     
                     ##input: trendline toggle
                     #...
                     
                   ),
                   mainPanel(
                     h4("Bivariate", align = "center"),
                     plotOutput(outputId = "plot4"),
                     br(),
                     div(tableOutput(outputId = "table_tab4"))
                   )
                 )
)
#################################################################################################################################

overview <- tabPanel("Overview", 
                     fluid = TRUE,
                     
                     div(
                       h1("Introduction"),
                       h3("Great Smoky Mountains National Park"),
                       p("Great Smoky Mountain National Park covers over 800 square miles of the southeastern United States, stretching along the North Carolina-Tennessee border. 
                         The park is one of the most visited in the country, with millions of visitors stopping by to take in the scenery and wildlife. Different times of year bring 
                         different sightings as the seasons change, and these changes and observed and documented through the study of phenology."),
                      br(),
                     ),
                     
                     #tags$img(src="images/SmokyMountains1.jpg", align="right", width=200, height=100),
                     
                     div(
                       h3("Phenology"),
                       p("Phenology is the study of the biological cycles observed in nature, and phenological data for Great Smoky Mountain National Park is displayed in this app.
                       While there is a great number of phenology trends that can be studied, this app primarily focuses on those that can be observed in tree species. Examples include
                         first flower, first fall leaf color and the date that 95% canopy is reached. First leaf out in spring is a particular focus of this app, as it allows the user
                         to explore whether or not spring is reaching Great Smoky Mountain National Park at an earlier time over the years.",),
                       p("There is also a great number of factors can influence phenology patterns. 
                       Three major factors that are observed and displayed in this app include elevation, precipitation and temperature. 
                       These factors and others can cause noticeable shifts in cycles from year to year, so observing phenology trends over lengths of time is necessary to find patterns of change."),
                       br(),
                       
                       h1("Tabs"),
                       h4("Phenology Observation Tracker"),
                       p("This tab allows the user to display graphs that visualize the relationships between, species, phenophase and elevation
                         for a chosen year. Up to three graphs can be displayed at once, allowing users to visually compare on a combination of factors.  "),
                       h4("Elevation Bands Time Series"),
                       p("With the elevation bands time series tab, the user can explore the changes a selected species demonstrates over the course of
                         time data has been collected for.
                         The graph divides the species data into three elevation bands and displays both points and a trendline."),
                       h4("Species Time Series"),
                       p("Similar to the elevation bands time series, this tab allows the user to select a single species of tree and view the phenophase data
                         for the duration of the study time.
                         However, only one elevation band can be selected at a time, allowing for a graph that can provide a 
                         more specific insight to a particular subset"),
                       h4("Bivariate"),
                       p("Lastly, the bivariate tab allows the user to visualize the impacts of temperature or precipitation on the phenophase onset for a species over time.
                         The user can examine minimum or maximum spring temperature as well as precipitation accumulation.
                         The user selects a particular tree species and a particular site in order to subset the information."),
                       p("Notes on weather condition data:"),
                       p("Accumulated precipitation is calculated for the first observation of the phenophase."),
                       br(),
                       
                       h3("Creators"),
                       p("This app was created as a collaboration between the National Park Service and Environmental Data Science capstone students at Virginia Tech:  
                         Jason Halvis, Sean Murray, Jennifer Ochs, and Xuanang Zhao")
                     )
)

#################################################################################################################################
#################################################################################################################################
# Setup the bslib theme object
my_theme <- bs_theme(bootswatch = "darkly",
                     base_font = font_google("Roboto Mono")) ##other fonts: Space Mono, Lato, Roboto, Source Sans Pro, Oswald, Inter
# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")


#################################################################################################################################
#################################################################################################################################

ui = fluidPage(
  title = "GRSM Phenology App",
  titlePanel(title = div(tags$img(src = "images/SmokyMountains1.jpg", width=90, height=60), 
                         "Exploring Phenology Trends in Great Smoky Mountains National Park")),

  tabsetPanel(overview, tab1, tab2, tab3, tab4),
  theme = my_theme,
  radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "darkly"))
)

#################################################################################################################################
#################################################################################################################################

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  # Plot of Day of Year by Year
  
  # Subset data
  selected_species <- reactive({
    cdfa %>%
      filter(common_name == input$common_name,
             year == input$year,
             phenophase_description == input$phenophase_description
      )
  })
  
  selected_species2 <- reactive({
    cdfa %>%
      filter(common_name == input$common_name2,
             year == input$year2,
             phenophase_description == input$phenophase_description2
      )
  })
  
  selected_species3 <- reactive({
    cdfa %>%
      filter(common_name == input$common_name3,
             year == input$year3,
             phenophase_description == input$phenophase_description3
      )
  })
  
  selected_ElevTS <- reactive({
    icdfE %>%
      filter(
        common_name == input$common_nameE,
        phenophase_description == input$phenophase_descriptionE,
      )
  })
  
  selected_status <- reactive({
    cdf2 %>%
      filter(
        common_name == input$common_name,
        phenophase_description == input$phenophase_description,
        #elev_bands == input$elev_bands,
        day_of_year < input$DOY
      )
  })
  
  selected_intensity <- reactive({
    icdf2 %>%
      filter(
        common_name == input$common_name2,
        #elev_bands == input$elev_bands2,
        day_of_year < input$DOY2
      )
  })
  
  selected_bivar <- reactive({
    bivar_weather %>%
      filter(
        site_name == input$site_name_bivar,
        #common_name %in% input$common_name_bivar
        common_name == input$common_name_bivar
      ) %>%
      select(common_name, site_name, 
             wvar=input$weather_condition, 
             year, day_of_year
      )
  })
  
  #select input for tab3
  selected_timeSeries <- reactive({
    cdf2 %>%
      filter(
        common_name == input$common_name4,
        elev_bands == input$elev_bands2,
        phenophase_description == input$Phenophases_species
      )
  })
  
  #########
  
  ##########################################
  ## OUTPUT PLOT 1: status / quick glance ##
  status_colors <- c("blue", "goldenrod")
  status_labels <- c("Not Observed", "Observed")
  elev_order <- c("<800m", "800-1300m", ">1300m")
  
  output$plot1 <- renderPlot({
    ggplot(selected_species(), aes(x=day_of_year, y=elev_bands,fill = y)) +
      geom_point(pch = 22, size = 6) +
      geom_vline(xintercept = c(79, 172, 265, 355), xlim(NA, 365), color = 'black') +
      labs(title = paste(input$phenophase_description, "of", input$common_name, "in", input$year), 
           x = "Day of Year", y = "Elevation Band", fill = "Phenophase Status",
           caption = "Vertical line indicates the start of an astronomical season") +
      scale_color_manual(name="Phenophase Status", values=status_colors) +
      scale_fill_manual(name="Phenophase Status", values=status_colors, labels=status_labels) +
      scale_x_continuous(breaks = seq(0, 365, 30)) +
      scale_y_discrete(limits = elev_order) +
      theme_classic() + 
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 28, face = "bold"),
            plot.caption = element_text(size = 14)) 
  })
  
  output$plot1a <- renderPlot({
    req(input$show_second_plot)
    if(input$show_second_plot) {
      ggplot(selected_species2(), aes(x=day_of_year, y=elev_bands,fill = y)) +
        geom_point(pch = 22, size = 6) +
        geom_vline(xintercept = c(79, 172, 265, 355), xlim(NA, 365), color = 'black') +
        labs(title = paste(input$phenophase_description2, "of", input$common_name2, "in", input$year2), 
             x = "Day of Year", y = "Elevation Band", fill = "Phenophase Status",
             caption = "Vertical line indicates the start of an astronomical season") +
        scale_color_manual(name="Phenophase Status", values=status_colors) +
        scale_fill_manual(name="Phenophase Status", values=status_colors, labels=status_labels) +
        scale_x_continuous(breaks = seq(0, 365, 30)) +
        scale_y_discrete(limits = elev_order) +
        theme_classic() + 
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 20, face = "bold"),
              plot.title = element_text(size = 28, face = "bold"),
              plot.caption = element_text(size = 14)) 
    }
  })
  
  output$plot1b <- renderPlot({
    req(input$show_third_plot)
    if(input$show_third_plot) {
      ggplot(selected_species3(), aes(x=day_of_year, y=elev_bands,fill = y)) +
        geom_point(pch = 22, size = 6) +
        geom_vline(xintercept = c(79, 172, 265, 355), xlim(NA, 365), color = 'black') +
        labs(title = paste(input$phenophase_description3, "of", input$common_name3, "in", input$year3), 
             x = "Day of Year", y = "Elevation Band", fill = "Phenophase Status",
             caption = "Vertical line indicates the start of an astronomical season") +
        scale_color_manual(name="Phenophase Status", values=status_colors) +
        scale_fill_manual(name="Phenophase Status", values=status_colors, labels=status_labels) +
        scale_x_continuous(breaks = seq(0, 365, 30)) +
        scale_y_discrete(limits = elev_order) +
        theme_classic() + 
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 20, face = "bold"),
              plot.title = element_text(size = 28, face = "bold"),
              plot.caption = element_text(size = 14)) 
    }
  })
  
  ################################
  ## OUTPUT PLOT 2: time series ##
  elev_colors <- c("blue", "goldenrod", "black")
  
  output$plot2 <- renderPlot({
    ggplot(selected_ElevTS(), aes(x=year, y=day_of_year, color=elev_bands, fill = elev_bands)) +
      geom_point(pch = 21, size = 6) +
      geom_smooth(method=lm, se=FALSE) +
      stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE, size = 5) +
      scale_color_manual(values = elev_colors) +
      scale_fill_manual(values = elev_colors) +
      scale_x_continuous(breaks = unique(selected_ElevTS()$year), labels = unique(selected_ElevTS()$year)) +
      labs(title = paste(input$phenophase_descriptionE, input$common_nameE, "by elevation band"), 
           x = "Year", y = "Day of Year", fill = "Elevation Bands") +
      theme_classic() + 
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 28, face = "bold")) +
      guides(color = FALSE)
  })
  
  ###################
  ## OUTPUT PLOT 3 ##
  output$plot3 <- renderPlot({
    ggplot(selected_timeSeries(), aes(x = year, y = day_of_year)) +
      geom_point(aes(color = common_name), pch = 21, size = 6) + 
      geom_smooth(method=lm, se=FALSE) + 
      stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE, size = 5) + 
      scale_x_continuous(breaks = unique(selected_timeSeries()$year), labels = unique(selected_timeSeries()$year)) + 
      labs(title = paste("First leaf out of", input$common_name4, "at elevation band", input$elev_bands2), 
           x = "Year", y = "Day of Year")+
      theme_classic() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 28, face = "bold"))
  })
  
  ###################
  ## OUTPUT PLOT 4 ##
  categories1 <- c("blue", "goldenrod", "black", "red","darkorange", "yellow", "green", "cyan", "purple", "magenta", "pink")
  
  output$plot4 <- renderPlot({
    ggplot(selected_bivar(), aes(x=wvar, y=day_of_year, shape=common_name)) +
      geom_point(aes(size = 20, color = as.factor(year))) +
      #geom_point(colour = "grey90", size = 4) +
      geom_smooth(method=lm, se=FALSE) +
      scale_color_manual(values = categories1) +
      scale_fill_discrete(name = "Year") +
      ggtitle(paste(input$common_name_bivar, "first leaf out vs.", input$weather_condition)) +
      xlab(input$weather_condition) +
      ylab(paste("first leaf out DOY")) +
      labs(color = "Year") +
      theme_classic() +
      guides(shape = "none", size = "none") +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 16, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            legend.position = "bottom", legend.box = "horizontal",
            legend.title = element_text(color="blue",size=14,face="bold"),
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
            legend.justification = "left", 
            #plot.margin = margin(.1, .1, .1, .1, "cm"),
            #aspect.ratio = 1
            )
    },
    height = 600, width = 700
  )

  
  #output$table_tab4 <- renderTable(selected_tab4())
  
  ############################
  ## light mode / dark mode ##
  observe({
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

#################################################################################################################################
#################################################################################################################################

thematic_shiny()
thematic_on()
####
shinyApp(ui = ui, server = server)

