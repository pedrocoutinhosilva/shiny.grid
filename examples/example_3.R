library(shiny)
library(shiny.grid)

ui <- gridPage(
    title = "Flex page example",

    flexPanel(
      div(
        class = "subarea-1",
        style = "background: #cacaca; border: 5px solid gray;",
        tags$label("A grid panel with a inner grid")
      ),
      div(
        class = "subarea-1",
        style = "background: #cacaca; border: 5px solid gray;",
        tags$label("A grid panel with a inner grid")
      ),
      div(
        class = "subarea-1",
        style = "background: #cacaca; border: 5px solid gray;",
        tags$label("A grid panel with a inner grid")
      )            
    )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
