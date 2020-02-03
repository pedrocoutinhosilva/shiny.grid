#Main page layout

gridPage <- function(
  ...,
  title = NULL,
  dependency = bootstrapLib(),
  fill_page = TRUE
) {
  tagList(
    if(fill_page) tags$head(tags$style("html, body { height: 100vh; width: 100vw; }")),
    if (!is.null(title)) tags$head(tags$title(title)),
    dependency,
    gridPanel(..., id = "grid-page-wrapper")
  )
}

gridPanel <- function(
  ...,
  grid_template_areas = NULL,
  grid_template_rows = NULL,
  grid_template_columns = NULL,

  id = NULL,
  class = NULL,
  position = NULL
) {

  style <- paste0(
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
      "repeat(", length(stringi::stri_remove_empty(unlist(strsplit(a[1], split=" ")))), ", 1fr)")

  if(is.null(grid_template_rows))
    grid_template_rows <- paste0(
      "repeat(", length(stringi::stri_remove_empty(unlist(strsplit(a[1], split=" ")))), ", 1fr)")

  } else {
    child_styles <- ""
  }

  style <- paste0(
    style,
    "grid-template-rows:", grid_template_rows, ";",
    "grid-template-columns:", grid_template_columns, ";",
    "grid-template-areas: '", paste0(grid_template_areas, collapse = "' '"), "';"
  )

  tagList(
    tags$head(tags$style(child_styles)),
    tags$div(
      id = id,
      class = paste0(class, " ", position),
      style = style,
      list(...)
    )
  )
}
