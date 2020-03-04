#' Create a panel with a CSS grid layout
#'
#' @param ... Elements to include within the panel
#' @param areas A list of named areas. Each element should contain
#'   the names, per row, of each area of the grid. Expected values follow the
#'   convension for the grid-template-areas css attribute.
#' @param rows A string of css valid sizes separated by a space.
#'   Follows the convension for the grid-template-rows css attribute.
#'   If not provided the existing space will be split equally acording to the
#'   areas defined in areas.
#' @param columns A string of css valid sizes separated by a space.
#'   Follows the convension for the grid-template-columns css attribute.
#'   If not provided the existing space will be split equally acording to the
#'   areas defined in areas.
#' @param id The html id of the panel container.
#' @param class Custom classes to be added to the panel container.
#' @param style Custom css style attributes to be added to the panel container.
#'
#' @return An HTML tagList.
#'
#' @details Behaves similar to normal HTML div tags, but simplifies
#'   the way css grid can be used via the arguments area, rows and columns.
#'   Only areas is required, when not used rows and columns will simply split
#'   the existing space equaly between each row / column.
#'   Internally the function creates the styles for the grid positions by
#'   generating the rules for positioning the children via their classes.
#'   To position a child element simply make sure that it includes a class
#'   with the same name as the named area.
#'
#' @note See \url{https://css-tricks.com/snippets/css/complete-guide-grid/}
#'   for additional details on using css grids
#'
#' @family grid functions
#' @seealso [gridPage()]
#'
#' @examples
#' gridPanel(
#'   areas = c("area-1 area-1", "area-2 area-3"),
#'   rows = c"1fr 2fr",
#'   columns = c"2fr 100px",
#'   div(class = "area-1"),
#'   div(class = "area-2"),
#'   div(class = "area-3")
#' )
#'
#' @export
gridPanel <- function(...,
                      areas = NULL,
                      rows = NULL,
                      columns = NULL,
                      id = NULL,
                      class = NULL,
                      style = NULL
                    ) {
  id <- ifelse (!is.null(id), id, stringi::stri_rand_strings(1, 12))
  children_style <- ""

  if (!is.null(areas)) {
    if(is.null(columns)) {
      number_columns <- stringi::stri_remove_empty(
        unlist(strsplit(areas[1], split=" "))
      )
      columns <- paste0("repeat(", length(number_columns), ", 1fr)")
    }
    if(is.null(rows)) {
      rows <- paste0("repeat(", length(areas), ", 1fr)")
    }

    children_style <- lapply(
      stringi::stri_remove_empty(unique(unlist(strsplit(areas, split=" ")))),
      function(single){
        return(paste0("#", id, " .", single, " { grid-area: ", single, ";} "))
      }
    )
  }

  style <- paste0(
    style,
    "height: 100%; width: 100%;",
    "display: grid;",
    "grid-template-rows:", rows, ";",
    "grid-template-columns:", columns, ";",
    "grid-template-areas: '", paste0(areas, collapse = "' '"), "';"
  )

  tagList(
    tags$head(
      tags$style(children_style)
    ),
    tags$div(
      id = id,
      class = class,
      style = style,
      list(...)
    )
  )
}
