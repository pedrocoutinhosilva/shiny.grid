# Generates 500 rows of data using the input settings from config.json

library(shiny)
library(shiny.grid)

# Define UI for application that draws a histogram
ui <- gridPage(
    title = "its a boy",
    # dependency = shiny.semantic:::get_dependencies(),

    grid_template_rows = "100px 1fr 1fr",
    grid_template_columns = "1fr 1fr 1fr",
    grid_template_areas = c(
      "header header header",
      "sidebar main main",
      "sidebar main main"
    ),

    gridPanel(
      grid_template_columns = "1fr 1fr 1fr",
      grid_template_areas = c(
        "... title ..."
      ),

      position = "header",
      gridPanel(position = "title", titlePanel("Old Faithful Geyser Data"))
    ),

    gridPanel(
      position = "sidebar",
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    gridPanel(
      position = "main",
      plotOutput("distPlot", height = "100%")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        test = 'darkgray'

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = test, border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
