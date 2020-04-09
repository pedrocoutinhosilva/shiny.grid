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
                      gap = NULL,
                      style = NULL
                    ) {
  # If there is no id, define a random one
  id <- ifelse (!is.null(id), id, paste0("grid-", stringi::stri_rand_strings(1, 12)))

  # Preprocess missing values
  repeatRule<- function(options) {
    paste0("repeat(", length(options), ", 1fr)")
  }

  uniqueCols <- function(options) {
    stringi::stri_remove_empty(
      unlist(strsplit(options[1], split=" "))
    )
  }

  if(!is.null(areas) && !is.list(areas)) areas <- list(default = areas)
  if(!is.null(rows) && !is.list(rows)) rows <- list(default = rows)
  if(!is.null(columns) && !is.list(columns)) columns <- list(default = columns)
  if(!is.null(gap) && !is.list(gap)) gap <- list(default = gap)

  if (is.null(columns)) {
    if (!is.null(areas)) {
      columns <- list(default = "1fr")
      for (rule in names(areas)) {
        columns[[rule]] <- repeatRule(uniqueCols(areas[[rule]]))
      }
    }
  }
  if (is.null(rows)) {
    if (!is.null(areas)) {
      rows <- list(default = "1fr")
      for (rule in names(areas)) {
        rows[[rule]] <- repeatRule(areas[[rule]])
      }
    }
  }
  if (is.null(gap)) {
    if (!is.null(areas)) {
      gap <- list(default = "0")
      for (rule in names(areas)) {
        gap[[rule]] <- "0"
      }
    }
  }

  # generate areas
  css_areas <- ""
  children_style <- ""
  if (!is.null(areas)) {
    if (is.list(areas)) {
      for (rule in names(areas)) {
        if (rule == "default") {
          css_areas <- paste0(
            css_areas,
            generateRule(id, "grid-template-areas", paste0("'", paste0(areas[[rule]], collapse = "' '"), "'"))
          )
        } else {
          wrapper <- getRuleWrapper(activeMediaRules[[rule]])
          media_areas <- generateRule(id, "grid-template-areas", paste0("'", paste0(areas[[rule]], collapse = "' '"), "'"))
          css_areas <- paste0(
            css_areas,
            do.call(glue::glue, list(wrapper, rules = media_areas))
          )
        }
      }

      children_style_target <- areas$default
    } else {
      css_areas <- generateRule(id, "grid-template-areas", paste0("'", paste0(areas, collapse = "' '"), "'"))
      children_style_target <- areas
    }

    children_style <- lapply(
      stringi::stri_remove_empty(unique(unlist(strsplit(children_style_target, split=" ")))),
      function(single){
        return(HTML(paste0("#", id, " > .", single, " { grid-area: ", single, ";} ")))
      }
    )
  }

  # generate rows
  css_rows <- ""
  if (!is.null(rows)) {
    if (is.list(rows)) {
      for (rule in names(rows)) {
        if (rule == "default") {
          css_rows<- paste0(
            css_rows,
            generateRule(id, "grid-template-rows", paste0(rows[[rule]]))
          )
        } else {
          wrapper <- getRuleWrapper(activeMediaRules[[rule]])
          media_rows <- generateRule(id, "grid-template-rows", paste0(rows[[rule]]))
          css_rows <- paste0(
            css_rows,
            do.call(glue::glue, list(wrapper, rules = media_rows))
          )
        }
      }
    } else {
      css_rows <- generateRule(id, "grid-template-rows", paste0(rows))
    }
  }

  # generate columns
  css_columns <- ""
  if (!is.null(columns)) {
    if (is.list(columns)) {
      for (rule in names(columns)) {
        if (rule == "default") {
          css_columns <- paste0(
            css_columns,
            generateRule(id, "grid-template-columns", paste0(columns[[rule]]))
          )
        } else {
          wrapper <- getRuleWrapper(activeMediaRules[[rule]])
          media_columns <- generateRule(id, "grid-template-columns", paste0(columns[[rule]]))
          css_columns <- paste0(
            css_columns,
            do.call(glue::glue, list(wrapper, rules = media_columns))
          )
        }
      }
    } else {
      css_columns <- generateRule(id, "grid-template-columns", paste0(columns))
    }
  }

  css_gap <- ""
  if (!is.null(gap)) {
    if (is.list(gap)) {
      for (rule in names(gap)) {
        if (rule == "default") {
          css_gap <- paste0(
            css_gap,
            generateRule(id, "gap", paste0(gap[[rule]]))
          )
        } else {
          wrapper <- getRuleWrapper(activeMediaRules[[rule]])
          media_gap <- generateRule(id, "gap", paste0(gap[[rule]]))
          css_gap <- paste0(
            css_gap,
            do.call(glue::glue, list(wrapper, rules = media_gap))
          )
        }
      }
    } else {
      css_gap <- generateRule(id, "gap", paste0(gap))
    }
  }

  tags$div(
    id = id,
    class = class,
    style = paste0(
      style,
      "height: 100%; width: 100%;",
      "display: grid;"
    ),
    list(...),
    lapply(c(css_areas, css_rows, css_columns, css_gap, children_style), function(script) {
      tags$style(script)
    })
  )
}
