library(shiny)
library(rnpn)
library(dplyr)
library(lubridate)

#Get data and format it appropriately
cdf <- npn_download_status_data(
  request_source = 'Alyssa RShiny Demo App', 
  network_ids = c(72),
  years = c(2010:2020), 
  species_ids = c(3, 98, 61, 82, 1187, 97, 1172, 823, 100, 79, 1189), 
  additional_fields = c("Site_Name", "Network_Name", "Phenophase_Category"),
  climate_data = TRUE
)

cdf=cdf %>%
  dplyr::mutate(year = lubridate::year(observation_date), 
                month = lubridate::month(observation_date), 
                day = lubridate::day(observation_date))

cdf$elev_bands <- cut(cdf$elevation_in_meters, c(-Inf,800,1300,Inf), c("<800m", "800-1300m", ">1300m"))

#Create Intensity Dataset - icdf
#Filter to 95% Intensity and the Leaves phenophase, get correct columns
icdf <- subset(cdf, 
               intensity_value == '95% or more' & phenophase_description == 'Leaves',
               select = c(species_id, phenophase_id, common_name, phenophase_description, intensity_value, site_name,
                          elevation_in_meters, elev_bands, tmin_winter, tmin_spring, tmax_winter, 
                          tmax_spring, daylength,individual_id, year, day_of_year))

#Select the earliest DOY of 95% canopy full by year by individual
icdf2 <- icdf %>%
  group_by(year, individual_id, common_name) %>%
  filter(day_of_year == min(day_of_year))

#Create Phenophase Status Dataset - cdf
#Filter to 1s (yes to phenophase status), get correct columns
cdf1 <- subset(cdf, 
               phenophase_status == 1,
               select = c(species_id, phenophase_id, common_name, phenophase_description, site_name,
                          elevation_in_meters, elev_bands,tmin_winter, tmin_spring, tmax_winter, 
                          tmax_spring, daylength,individual_id, year, day_of_year))

#Select the earliest DOY by year by individual
cdf2 <- cdf1 %>%
  group_by(year, individual_id, common_name, phenophase_description) %>%
  filter(day_of_year == min(day_of_year))

#SHINY APP STARTS HERE
# Define UI for app that draws a plot----
ui = fluidPage(
  titlePanel("Phenology @ GRSM"),
  tabsetPanel(
    tabPanel("Status", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "common_name", label = strong("Select Species"), choices=unique(cdf2$common_name), selected="red maple"),
                            selectInput(inputId = "phenophase_description", label = strong("Select Phenophase"), choices=unique(cdf2$phenophase_description), selected="Colored leaves"),
                            selectInput(inputId = "elev_bands", label = strong("Select Elevation Range"), choices=unique(cdf2$elev_bands), selected="<800m"),
                            numericInput(inputId = "DOY", label = strong("Drop Onsets After Day:"), 350, min = 1, max = 365)
               ),
               mainPanel(h4("Phenophase Onset 2010-2020", align = "center"), plotOutput(outputId = "plot"))
             )
    ),
    
    tabPanel("Intensity", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "common_name2", label = strong("Select Species"), choices=unique(icdf2$common_name), selected="yellow birch"),
                            selectInput(inputId = "elev_bands2", label = strong("Select Elevation Range"), choices=unique(icdf2$elev_bands), selected="<800m"),
                            numericInput(inputId = "DOY2", label = strong("Drop Onsets After Day:"), 350, min = 1, max = 365)
               ),
               mainPanel(h4("First yes for Leaves, 95% or more, 2010-2020", align = "center"),plotOutput(outputId = "plot2"))
             )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Plot of Day of Year by Year
  
  # Subset data
  selected_status <- reactive({
    cdf2 %>%
      filter(
        common_name == input$common_name,
        phenophase_description == input$phenophase_description,
        elev_bands ==input$elev_bands,
        day_of_year < input$DOY
      )
  })
  
  selected_intensity <- reactive({
    icdf2 %>%
      filter(
        common_name == input$common_name2,
        elev_bands ==input$elev_bands2,
        day_of_year < input$DOY2
      )
  })
  
  output$plot <- renderPlot({
    
    plot(x = selected_status()$year, y = selected_status()$day_of_year, xlab = "Year", ylab = "Day of Year")
    abline(fit <- lm(selected_status()$day_of_year~selected_status()$year), col='red')
    legend("topleft", bty="n", legend=paste("R2 =", format(summary(fit)$adj.r.squared, digits=4)))
    
  })
  
  output$plot2 <- renderPlot({
    
    plot(x = selected_intensity()$year, y = selected_intensity()$day_of_year, xlab = "Year", ylab = "Day of Year")
    abline(fit <- lm(selected_intensity()$day_of_year~selected_intensity()$year), col='red')
    legend("topleft", bty="n", legend=paste("R2 =", format(summary(fit)$adj.r.squared, digits=4)))
    
  })
  
}

shinyApp(ui = ui, server = server)
