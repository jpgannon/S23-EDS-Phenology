# VT'23 EDS PHENOLOGY APP

#THIS SECTION IS THE SETUP, IT DOWNLOADS ALL THE NECESSARY LIBRARIES AND THE DATA CAN BE IMPORTED WITH THE NPN DOWNLOAD THING OR 
#VIA A CSV STORED ON A PERSONAL DEVICE
knitr::opts_chunk$set(echo = TRUE)
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

#setwd("C:/Users/Sean/Documents/S23-EDS-Phenology/app")

rm(list = ls())
##if (!file.exists("cdf.csv")) {
#cdf <- read.csv("www/data/cdf.csv")
##}

cdf <- npn_download_status_data(
   request_source = 'VT23 EDS APP DEMO', 
   network_ids = c(72),
   years = c(2010:2020), 
   species_ids = c(3, 98, 61, 82, 1187, 97, 1172, 823, 100, 79, 1189), 
   additional_fields = c("Site_Name", "Network_Name", "Phenophase_Category"),
   climate_data = TRUE)

#################################################################################################################################
#################################################################################################################################
#THIS SECTION FORMATS THE DATES AND DIVIDES INTO ITS ELEVATION CATEGORIES
cdf = cdf %>%
  dplyr::mutate(
    year = lubridate::year(observation_date),
    month = lubridate::month(observation_date),
    day = lubridate::day(observation_date)
  )
cdf$elev_bands <-
  cut(cdf$elevation_in_meters,
      c(-Inf, 800, 1300, Inf),
      c("<800m", "800-1300m", ">1300m"))

#################################################################################################################################
#################################################################################################################################
#THIS SECTION 
#Create Intensity Dataset - icdf
#Filter to 95% Intensity and the Leaves phenophase, get correct columns
icdf <- subset(
  cdf,
  intensity_value == '95% or more' &
    phenophase_description == 'Leaves',
  select = c(
    species, species_id, phenophase_id, common_name, phenophase_description,
    intensity_value, site_name, elevation_in_meters, elev_bands, tmin_winter, 
    tmin_spring, tmax_winter, tmax_spring, daylength, individual_id, year, day_of_year
  )
)

#################################################################################################################################
#################################################################################################################################
#Select the earliest DOY of 95% canopy full by year by individual
icdf2 <- icdf %>%
  group_by(year, individual_id, common_name) %>%
  filter(day_of_year == min(day_of_year))

icdf3 <- icdf %>%
  group_by(year, common_name) %>%
  filter(day_of_year == min(day_of_year))

#################################################################################################################################
#################################################################################################################################
#Create Phenophase Status Dataset - cdf
#Filter to 1s (yes to phenophase status), get correct columns
cdf1 <- subset(
  cdf,
  phenophase_status == 1,
  select = c(
    species_id, phenophase_id, common_name,
    phenophase_description, site_name, elevation_in_meters, elev_bands,
    tmin_winter,tmin_spring, tmax_winter, tmax_spring,
    daylength, individual_id, year, day_of_year
  )
)

cdfa <- subset(
  cdf,
  phenophase_status == 1 | phenophase_status == 0,
  select = c(
    common_name,phenophase_description,phenophase_status,
    elev_bands,year,day_of_year
  )
)
cdfa$y <- cut(cdfa$phenophase_status, breaks = 2)



#################################################################################################################################
#################################################################################################################################
#Select the earliest DOY by year by individual
cdf2 <- cdf1 %>%
  group_by(year, individual_id, common_name, phenophase_description) %>%
  filter(day_of_year == min(day_of_year))

cdf3 <- cdf1 %>%
  group_by(year, individual_id, common_name) %>%
  filter(day_of_year == min(day_of_year))


#################################################################################################################################
#################################################################################################################################
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
                   ),
                   
                   mainPanel(
                     h4("Phenophase Status by Elevation Bands", align = "center"),
                     plotOutput(outputId = "plot1")
                   )
                 ))
#################################################################################################################################
tab2 <- tabPanel("Status", ##*Time Series in future...
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "common_name2",
                       label = strong("Select Species"),
                       choices = unique(icdf2$common_name),
                       selected = "yellow birch"
                     ),
                     
                     selectInput(
                       inputId = "phenophase_description",
                       label = strong("Select Phenophase"),
                       choices = unique(cdf2$phenophase_description),
                       selected = "Colored leaves"
                     ),
                     
                     selectInput(
                       inputId = "elev_bands2",
                       label = strong("Select Elevation Range"),
                       choices = unique(icdf2$elev_bands),
                       selected = "<800m"
                     ),
                     
                     numericInput(
                       inputId = "DOY2",
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
tab3 <- tabPanel("Intensity", ##Bivariate temp./climate data in future...
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "common_name2",
                       label = strong("Select Species"),
                       choices = unique(icdf2$common_name),
                       selected = "yellow birch"
                     ),
                     
                     # selectInput(
                     #   inputId = "elev_bands2",
                     #   label = strong("Select Elevation Range"),
                     #   choices = unique(icdf2$elev_bands),
                     #   selected = "<800m"
                     # ),
                     
                     numericInput(
                       inputId = "DOY2",
                       label = strong("Drop Onsets After Day:"),
                       350,
                       min = 1,
                       max = 365
                     )
                   ),
                   mainPanel(
                     h4("First yes for Leaves, 95% or more, 2010-2020", align = "center"),
                     plotOutput(outputId = "plot3")
                   )
                 ))
#################################################################################################################################
overview <- tabPanel("Overview", 
                     fluid = TRUE,
                     
                     div(
                       h1("Introduction"),
                       h3("Great Smoky Mountains National Park"),
                       p("Great Smoky Mountain National Park covers over 800 square miles of the southeastern United States, stretching along the North Carolina-Tennessee border. 
                       The park is one of the most visited in the country, with millions of visitors stopping by to take in the scenery and wildlife. 
                       Different times of year bring different sightings as the seasons change, and these changes and observed and documented through the study of phenology."),
                       br()
                     ),
                     
              
                      tags$img(src="SmokyMountains1.jpg", align="right", width=100, height=100),
                                    
                     
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
                       h1("Tabs"),
                       h4("Quick Glance"),
                       p("This tab provides a brief look at phenophase observations for one of eleven tree species in a specific year."),
                       h4("Time Series"),
                       p("With the time series tab, you not only select a species and phenophase, but also an elevation band as this influences onset of changes. The user can select a range of years to see how the patterns of a particular phenophase changes over time."),
                       h4("Bivariate"),
                       p("Lastly, the bivariate tab allows the user to visualize the impacts of temperature or precipitation on the phenophase onset for a species over time."),
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
####
#theme = bs_theme(
#bg = "#0b3d91", fg = "white", primary = "#FCC780",
#base_font = font_google("Space Mono"),
#code_font = font_google("Space Mono")
#)
#theme = bs_theme(version = 4, bootswatch = "minty"),
#theme = bslib::bs_theme(bootswatch = "solar")

#################################################################################################################################
#################################################################################################################################

ui = fluidPage(
  titlePanel("Phenology @ GRSM"),
  #theme = bslib::bs_theme(bootswatch = "lux"),
  #header = customHeaderPanel(title = "title"),
  tabsetPanel(overview, tab1, tab2, tab3),
  theme = my_theme,
  radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "darkly"))
)

# ui = fluidPage(titlePanel("Phenology @ GRSM"),
#                  tabsetPanel(customHeaderPanel, overview,
#                              tab1,
#                              tab2,
#                              tab3),
#               theme = my_theme,
#               radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "darkly"))
#) 


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
             # day_of_year < input$DOY,
             phenophase_description == input$phenophase_description
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
  
  fl_calendar <- reactive({
    falling_leaves %>%
      filter(
        common_name == input$common_name,
        year == input$year,
        day_of_year == day_of_year
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
  
  #########
  
  ## OUTPUT PLOT 1: status / quick glance ##
  output$plot1 <- renderPlot({
    ggplot(selected_species(), aes(x=day_of_year, y=elev_bands)) +
      geom_point(aes(color = y)) +
      geom_vline(xintercept = c(NA, 90, 180, 270), xlim(NA, 365)) + 
      # labs(x = "Day of Year", y = "Elevation Band")
      scale_color_manual(name = "Phenophase Status",
                         values = c("red", "green"),
                         labels = c("No", "Yes"))
    
  })
  
  ## OUTPUT PLOT 2: time series ##
  output$plot2 <- renderPlot({
    plot(
      x = selected_intensity()$year,
      y = selected_intensity()$day_of_year,
      xlab = "Year",
      ylab = "Day of Year"
    )
    abline(fit <-
             lm(
               selected_intensity()$day_of_year ~ selected_intensity()$year
             ),
           col = 'red')
    legend("topleft",
           bty = "n",
           legend = paste("R2 =", format(summary(fit)$adj.r.squared, digits = 4)))
  })
  
  ## OUTPUT PLOT 3: bivariate ##
  output$plot3 <- renderPlot({
    plot(
      x = selected_status()$year,
      y = selected_status()$day_of_year,
      xlab = "Year",
      ylab = "Day of Year"
    )
    abline(fit <-
             lm(selected_status()$day_of_year ~ selected_status()$year),
           col = 'red')
    legend("topleft",
           bty = "n",
           legend = paste("R2 =", format(summary(fit)$adj.r.squared, digits = 4)))
  })
  
  
  ### light mode / dark mode
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
thematic_rmd()
shinyApp(ui = ui, server = server)
#runApp()










