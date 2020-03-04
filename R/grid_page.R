#' Create a page with CSS grid layout
#'
#' @param ... Elements to include within the page
#' @param title The browser window title (defaults to the host URL of the page).
#' @param fill_page Flag to tell the page if it should adjust the page to
#'   adjust and fill the browser window size.
#' @param dependency The set of web dependencies. This value can be a
#'   htmlDependency, for example the shiny bootstrap one (the default)
#'   or a tagList with diferent dependencies
#'
#' @return A UI definition that can be passed to the [shinyUI] function.
#'
#' @note See \url{https://css-tricks.com/snippets/css/complete-guide-grid/}
#'   for additional details on using css grids
#'
#' @family grid functions
#' @seealso [gridPanel()]
#'
#' @examples
#' gridPage(
#'   title = "A grid page",
#'   areas = c("area-1 area-1", "area-2 area-3"),
#'   div(class = "area-1"),
#'   div(class = "area-2"),
#'   div(class = "area-3")
#' )
#'
#' @export
gridPage <- function(...,
                      title = NULL,
                      fill_page = TRUE,
                      dependency = bootstrapLib()) {
  tagList(
    if (fill_page) {
      tags$head(tags$style("html, body { height: 100vh; width: 100vw; }"))
    },
    if (!is.null(title)) {
      tags$head(tags$title(title))
    },
    dependency,
    gridPanel(..., id = "grid-page-wrapper")
  )
}
