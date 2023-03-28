##-----------## VT2023 EDS -- GREAT SMOKIES PHENOLOGY APP ##-----------##
## Authors: Jason Halvis, Sean Murray, Jennifer Ochs, and Xuanang Zhao ##

# THIS SECTION IS THE SETUP, IT DOWNLOADS ALL THE NECESSARY LIBRARIES 
# AND THE DATA CAN BE IMPORTED WITH THE NPN DOWNLOAD PACKAGE,
# OR VIA A CSV STORED ON A PERSONAL DEVICE
#install.packages("ggplot2")

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

rm(list = ls())

## Set your working directory: ##
#setwd("C:/Users/Sean/Documents/S23-EDS-Phenology/app")
#setwd("C:/Users/jochs/Documents/NewCapstoneFolder/S23-EDS-Phenology/app")

## Data Download Example: ##
# cdf <- npn_download_status_data(
 #  request_source = 'VT23 EDS APP DEMO', 
  # network_ids = c(72),
   #years = c(2010:2020), 
   #species_ids = c(3, 98, 61, 82, 1187, 97, 1172, 823, 100, 79, 1189), 
   #additional_fields = c("Site_Name", "Network_Name", "Phenophase_Category"),
   #climate_data = TRUE)

## Sample Data Files: ##
cdfa <- read.csv("www/data/cdfa.csv")
cdf2 <- read.csv("www/data/cdf2.csv")
icdf2 <- read.csv("www/data/icdf2.csv")
bivar_weather <- read.csv("www/data/bivar_weather.csv")

#____________________________________________________________________________________________________________________________#
#SHINY APP STARTS HERE
# Define UI for app that draws a plot----
tab1 <- tabPanel("Quick Glance",
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "common_name",
                       label = strong("Select Species"),
                       choices = unique(cdfa$common_name),
                       selected = "red maple"
                     ),
                     
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
                       selected = "2010"
                     ),
                     
                     conditionalPanel(
                       condition = "input.show_second_plot",
                       
                       selectInput(
                         inputId = "common_name2",
                         label = strong("Select Species"),
                         choices = unique(cdfa$common_name),
                         selected = "sourwood"
                       ),
                       
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
                         selected = "2010"
                       )
                     ),
                     
                     checkboxInput(
                       inputId = "show_second_plot",
                       label = "Show second plot"
                     )
                     
                     
                   ),
                   mainPanel(
                     h4("Phenophase Status by Elevation Bands", align = "center"),
                     plotOutput(outputId = "plot1"),
                     plotOutput(outputId = "plot1a")
                   )
                 ))
#################################################################################################################################
tab2 <- tabPanel("Elevation Bands Time Series",
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput(
                       inputId = "common_name3",
                       label = strong("Select Species"),
                       choices = unique(icdf2$common_name),
                       selected = "yellow birch"
                     ),
                     
                     numericInput(
                       inputId = "DOY",
                       label = strong("Drop Onsets After Day:"),
                       350,
                       min = 1,
                       max = 365
                     )
                   ),
                   mainPanel(
                     h4("First yes for Leaves, 95% or more, 2010-2020", align = "center"),
                     plotOutput(outputId = "plot2")
                   )
                 ))
#################################################################################################################################
tab3 <- tabPanel("Species Time Series", 
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "common_name4",
                       label = strong("Select Species"),
                       choices = unique(icdf2$common_name),
                       multiple = TRUE, 
                       selected = "yellow birch"
                     ),
                     
                     selectInput(
                       inputId = "elev_bands2",
                       label = strong("Select Elevation Range"),
                       choices = unique(icdf2$elev_bands),
                       selected = "<800m"
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
                     ##Input: weather condition: tmin spring, tmax spring, or acc. precip.
                     selectInput(
                       inputId = "weather_condition",
                       label = strong("Select Weather Condition:"),
                       choices = c("Min T spring" = "tmin_spring",
                                   "Max T spring" = "tmax_spring", 
                                   "Acc prcp" = "acc_prcp"),  
                       
                       selected = which(colnames(bivar_weather)=="tmin_spring"),
                       #inline = T,
                     ),
                     
                     ##Input: species
                     selectInput(
                       inputId = "common_name_bivar",
                       label = strong("Select Species"),
                       choices = unique(bivar_weather$common_name),
                       multiple = TRUE, 
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
                     plotOutput(outputId = "plot4")
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
                       The park is one of the most visited in the country, with millions of visitors stopping by to take in the scenery and wildlife. 
                       Different times of year bring different sightings as the seasons change, and these changes and observed and documented through the study of phenology."),
                       br(),
                     ),
                     
                     
                     tags$img(src="images/SmokyMountains1.jpg", align="right", width=200, height=100),
                     
                     
                     div(
                       h3("Phenology"),
                       p("Phenology is the study of the biological cycles observed in nature, and phenological data for Great Smoky Mountain National Park is displayed in this app. 
                       While there is a great number of phenology trends that can be studied, this data explores four: ",),
                       h6("1. First Leaf Out"),
                       h6("2. First Flower"),
                       h6("3. 95% Canopy"),
                       h6("4. First Fall Leaf Color"),
                       p("A great number of factors can influence phenology patterns. 
                       Three major factors that are observed and displayed in this app include elevation, precipitation and temperature. 
                       These factors and others can cause noticeable shifts in cycles from year to year, so observing phenology trends over lengths of time is necessary to find patterns of change."),
                       br(),
                       
                       h1("Tabs"),
                       h4("Quick Glance"),
                       p("This tab provides a brief look at phenophase observations for one of eleven tree species in a specific year."),
                       h4("Time Series"),
                       p("With the time series tab, you not only select a species and phenophase, but also an elevation band as this influences onset of changes. The user can select a range of years to see how the patterns of a particular phenophase changes over time."),
                       h4("Bivariate"),
                       p("Lastly, the bivariate tab allows the user to visualize the impacts of temperature or precipitation on the phenophase onset for a species over time."),
                       br(),
                       
                       h3("Creators"),
                       p("This app was created as a senior capstone project as a collaboration between the National Park Service and Virginia Tech students (Zhao, Jason, Sean, Jen)")
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
  titlePanel("Phenology @ GRSM"),
  #theme = bslib::bs_theme(bootswatch = "lux"),
  #header = customHeaderPanel(title = "title"),
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
  
  selected_ElevTS <- reactive({
    icdf2 %>%
      filter(
        common_name == input$common_name3,
        day_of_year < input$DOY
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
  
  # fl_calendar <- reactive({
  #   falling_leaves %>%
  #     filter(
  #       common_name == input$common_name,
  #       year == input$year,
  #       day_of_year == day_of_year
  #     )
  # })
  
  selected_intensity <- reactive({
    icdf2 %>%
      filter(
        common_name == input$common_name2,
        #elev_bands == input$elev_bands2,
        day_of_year < input$DOY2
      )
  })
  
  selected_tab4 <- reactive({
    bivar_weather %>%
      filter(
        site_name == input$site_name_bivar,
        common_name %in% input$common_name_bivar
      ) %>%
      select(common_name, site_name, input$weather_condition, 
             year, day_of_year
      )
  })
  
  #select input for tab3
  selected_timeSeries <- reactive({
    bivar_weather %>%
      filter(
        common_name %in% input$common_name4,
        elev_bands == input$elev_bands2,
      )
  })
  
  #########
  
  ##########################################
  ## OUTPUT PLOT 1: status / quick glance ##
  status_colors <- c("blue", "goldenrod")
  status_labels <- c("Not Observed", "Observed")
  
  output$plot1 <- renderPlot({
    ggplot(selected_species(), aes(x=day_of_year, y=elev_bands,fill = y)) +
      geom_point(pch = 22, size = 6) +
      geom_vline(xintercept = c(NA, 90, 180, 270), xlim(NA, 365)) + 
      labs(title = paste(input$phenophase_description, "of", input$common_name, "in", input$year), 
           x = "Day of Year", y = "Elevation Band", fill = "Phenophase Status") +
      scale_color_manual(name="Phenophase Status", values=status_colors) +
      scale_fill_manual(name="Phenophase Status", values=status_colors, labels=status_labels) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 28, face = "bold")) 
    
  })
  
  output$plot1a <- renderPlot({
    req(input$show_second_plot)
    if(input$show_second_plot) {
      ggplot(selected_species2(), aes(x=day_of_year, y=elev_bands,fill = y)) +
        geom_point(pch = 22, size = 6) +
        geom_vline(xintercept = c(NA, 90, 180, 270), xlim(NA, 365)) + 
        labs(title = paste(input$phenophase_description2, "of", input$common_name2, "in", input$year2), 
             x = "Day of Year", y = "Elevation Band", fill = "Phenophase Status") +
        scale_color_manual(name="Phenophase Status", values=status_colors) +
        scale_fill_manual(name="Phenophase Status", values=status_colors, labels=status_labels) +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 20, face = "bold"),
              plot.title = element_text(size = 28, face = "bold")) 
    }
    
    
  })
  
  ################################
  ## OUTPUT PLOT 2: time series ##
  elev_colors <- c("blue", "goldenrod", "black")
  
  output$plot2 <- renderPlot({
    ggplot(selected_ElevTS(), aes(x=year, y=day_of_year, color=elev_bands, fill = elev_bands)) +
      geom_point(pch = 21, size = 6) +
      geom_smooth(method=lm, se=FALSE) +
      scale_color_manual(values = elev_colors) +
      scale_fill_manual(values = elev_colors) +
      labs(title = paste("First leaf out of", input$common_name3, "by elevation band"), 
           x = "Year", y = "Day of Year", fill = "Elevation Bands")+
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
    ggplot(selected_tab4(), aes(x= year, y=day_of_year, shape = common_name)) +
      # x=tmin_spring
      # x=input$weather_condition
      geom_point(aes(color = as.factor(year)), size = 6) +
      geom_point(colour = "grey90", size = 1.5) +
      geom_smooth(method=lm, se=FALSE) +
      scale_color_manual(values = categories1) +
      scale_fill_manual(values = categories1) +
      ggtitle(paste(input$common_name_bivar, "first leaf out vs. min. spring temperature,", input$year_bivar)) +
      xlab("Minimum spring temperature (C)") +
      ylab(paste("first leaf out DOY for", input$common_name_bivar)) +
      labs(subtitle = "*need to reassess data subset...") +
      theme_classic() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 16, face = "bold"),
            plot.title = element_text(size = 20, face = "bold")) 
      }, 
      height = 600, width = 800)
  
  ############################
  ## light mode / dark mode ##
  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
  
}

#################################################################################################################################
#################################################################################################################################

thematic_shiny()
thematic_on()
#thematic_rmd()
shinyApp(ui = ui, server = server)
#runApp()
