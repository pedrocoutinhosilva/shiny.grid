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
                      id = paste0("grid-", stringi::stri_rand_strings(1, 12)),
                      class = NULL,
                      gap = NULL,
                      style = NULL) {

  areas %<>% preprocessAttribute()

  rows %<>% getAttributeStyle(
    id, areas, "grid-template-rows", "1fr",
    function(rule, areas) {
      paste0("repeat(", length(areas[[rule]]), ", 1fr)")
    }
  )

  columns %<>% getAttributeStyle(
    id, areas, "grid-template-columns", "1fr",
    function(rule, areas) {
      paste0("repeat(", length(stringi::stri_remove_empty(
        unlist(strsplit(areas[[rule]][1], split = " "))
      )), ", 1fr)")
    }
  )

  gap %<>% getAttributeStyle(
    id, areas, "gap", "0",
    function(rule, areas) {"0"}
  )

  css_areas <- ""
  children_style <- ""

  if (!is.null(areas)) {
    if (is.list(areas)) {
      for (rule in names(areas)) {
        if (rule == "default") {
          css_areas %<>% paste0(generateGridRule(id, "grid-template-areas", collapseValues(areas[[rule]])))
        } else {
          css_areas %<>% paste0(glue(
            ruleWrapper(activeMediaRules[[rule]]),
            rules = generateGridRule(id, "grid-template-areas", collapseValues(areas[[rule]]))
          ))
        }
      }

      children_style_target <- areas$default
    } else {
      css_areas <- generateGridRule(id, "grid-template-areas", collapseValues(areas))
      children_style_target <- areas
    }

    children_style <- lapply(
      stringi::stri_remove_empty(unique(unlist(strsplit(children_style_target, split=" ")))),
      function(single){
        return(HTML(paste0("#", id, " > .", single, " { grid-area: ", single, ";} ")))
      }
    )
  }

  tags$div(
    id = id,
    class = class,
    style = paste0("height: 100%; width: 100%; display: grid;", style),
    list(...) %>% preprocessContent(),
    tags$style(css_areas),
    tags$style(rows),
    tags$style(columns),
    tags$style(gap),
    tags$style(children_style)
  )
}
