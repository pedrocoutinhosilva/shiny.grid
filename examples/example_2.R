library(shiny)
library(shiny.grid)

ui <- gridPage(
    title = "Grid Panels example",

    rows = list(
      default = "1fr 2fr 3fr",
      xs = "1fr 1fr 1fr"
    ),
    areas = list(
      default = c(
        "area-1 area-1 area-1",
        "area-2 area-3 area-3",
        "area-2 area-3 area-3"
      ),
      xs = c(
        "area-1",
        "area-2",
        "area-3"
      )
    ),
    gap = list(
      default = "0",
      md = "20px",
      xs = "5px"
    ),

    div(
      class = "area-1",
      style = "background: #FFD369; border: 5px solid #d48000;",
      tags$label("A simple div")
    ),

    gridPanel(
      class = "area-2",
      style = "background: #E8B25F; border: 5px solid #d48000;",
      tags$label("A simple grid panel")
    ),

    gridPanel(
      class = "area-3",
      rows = "1fr 1fr 5fr",
      columns = "1fr 1fr 5fr",

      style = "background: #E8B21F;  border: 5px solid #d48000;",

      areas = c(
        "subarea-1 subarea-1 subarea-3",
        "subarea-1 subarea-1 subarea-3",
        "subarea-2 subarea-2 subarea-3"
      ),

      div(
        class = "subarea-1",
        style = "background: #cacaca; border: 5px solid gray;",
        tags$label("A grid panel with a inner grid")
      ),
      `subarea-2` = div(
        style = "background: #a0a0a0; border: 5px solid gray;",
        tags$label("I dont have the right area class, but im called as a named argument")
      ),
      gridPanel(
        class = "subarea-3",
        style = "background: #4c4c4c; border: 5px solid gray;",

        areas = list(
          default = c(
            "subsubarea-1",
            "subsubarea-2",
            "subsubarea-3"
          ),
          xs = c(
            "subsubarea-1 subsubarea-1",
            "subsubarea-2 subsubarea-3"
          )
        ),

        gap = list(
          default = "0",
          xs = "5%"
        ),

        div(
          class = "subsubarea-1",
          style = "background: #71A5FF; border: 5px solid blue;",

          tags$label("We need to go deeper")
        ),
        div(
          class = "subsubarea-2",
          style = "background: #72E7E8; border: 5px solid blue;"
        ),
        gridPanel(
          class = "subsubarea-3",
          style = "background: #8AFFB1; border: 5px solid blue;",
          rows = "repeat(4, 1fr)",

          div(style = "background: #8AFF91; border: 3px solid green;"),
          div(style = "background: #8AFF81; border: 3px solid green;"),
          div(style = "background: #8AFF71; border: 3px solid green;"),
          gridPanel(
            style = "background: #8AFF61; border: 3px solid green;",
            columns = "1fr 1fr 1fr",

            div(style = "background: #8A1F91; border: 2px solid yellow;"),
            div(style = "background: #8A2F81; border: 2px solid yellow;"),
            div(style = "background: #8A3F71; border: 2px solid yellow;")
          )
        )
      )
    )
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
