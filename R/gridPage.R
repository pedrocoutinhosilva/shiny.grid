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
