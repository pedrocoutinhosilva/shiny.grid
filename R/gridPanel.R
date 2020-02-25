gridPanel <- function(
  ...,
  grid_template_areas = NULL,
  grid_template_rows = NULL,
  grid_template_columns = NULL,

  id = NULL,
  class = NULL,
  style = NULL,
  position = NULL
) {

  wrapper_style <- paste0(
    style,
    "height: 100%; width: 100%; ",
    "display: grid; ")

  if(!is.null(grid_template_areas)) {
    child_styles <- lapply(
      stringi::stri_remove_empty(unique(unlist(strsplit(grid_template_areas, split=" ")))),
      function(single){
        return(paste0(".", single, " { grid-area: ", single, ";} "))
      }
    )

    if(is.null(grid_template_columns))
      grid_template_columns <- paste0(
        "repeat(", length(stringi::stri_remove_empty(unlist(strsplit(grid_template_areas[1], split=" ")))), ", 1fr)")

    if(is.null(grid_template_rows))
      grid_template_rows <- paste0(
        "repeat(", length(grid_template_areas), ", 1fr)")

  } else {
    child_styles <- ""
  }

  wrapper_style <- paste0(
    wrapper_style,
    "grid-template-rows:", grid_template_rows, ";",
    "grid-template-columns:", grid_template_columns, ";",
    "grid-template-areas: '", paste0(grid_template_areas, collapse = "' '"), "';"
  )

  tagList(
    tags$head(tags$style(child_styles)),
    tags$div(
      id = id,
      class = paste0(class, " ", position),
      style = wrapper_style,
      list(...)
    )
  )
}
