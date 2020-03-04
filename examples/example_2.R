library(shiny)
library(shiny.grid)

ui <- gridPage(
    title = "Grid Panels example",
    rows = "1fr 2fr 3fr",
    areas = c(
      "area-1 area-1 area-1",
      "area-2 area-3 area-3",
      "area-2 area-3 area-3"
    ),

    div(
      class = "area-1",
      style = "background: #FFD369; border: 5px solid black;",
      tags$label("A simple div")
    ),

    gridPanel(
      class = "area-2",
      style = "background: #E8B25F; border: 5px solid black;",
      tags$label("A simple grid panel")
    ),

    gridPanel(
      class = "area-3",
      rows = "1fr 1fr 5fr",
      columns = "1fr 1fr 5fr",

      style = " border: 5px solid black;",

      areas = c(
        "subarea-1 subarea-1 subarea-3",
        "subarea-1 subarea-1 subarea-3",
        "subarea-2 subarea-2 subarea-3"
      ),

      div(
        class = "subarea-1",
        style = "background: #FFB675; border: 5px solid gray;",
        tags$label("A grid panel with a inner grid")
      ),
      div(
        class = "subarea-2",
        style = "background: #E88B5F; border: 5px solid gray;"
      ),
      gridPanel(
        class = "subarea-3",
        style = "background: #FF8369; border: 5px solid gray;",

        areas = c(
          "subsubarea-1", "subsubarea-2", "subsubarea-3"
        ),

        div(
          class = "subsubarea-1",
          style = "background: #71A5FF; border: 5px solid red;",

          tags$label("We need to go deeper")
        ),
        div(
          class = "subsubarea-2",
          style = "background: #72E7E8; border: 5px solid red;"
        ),
        div(
          class = "subsubarea-3",
          style = "background: #8AFFB1; border: 5px solid red;"
        )
      )
    )
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)