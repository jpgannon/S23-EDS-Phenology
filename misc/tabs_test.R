library(shiny)

ui <- navbarPage("My App",
                 tabPanel("Tab 1",
                          h1("This is tab 1")),
                 tabPanel("Tab 2",
                          h1("This is tab 2")))
                tabPanel("Tab 3",
                          h1("This is tab 3")),
                tabPanel("Tab 4",
                          h1("This is tab 4")))

server <- function(input, output) {
}

shinyApp(ui, server)

## test sean murray
