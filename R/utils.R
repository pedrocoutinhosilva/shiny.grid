#' Generates a css rule for the given id element.
#'
#' @param id The target id of the css rule
#' @param attribute The css attribute the rule will target
#' @param value The css value for the given attribute
#'
#' @importFrom glue glue
#' @return A complete CSS rule.
generateGridRule <- function(id, attribute, value) {
  glue("#{id} {{ {attribute}: {value}; }}")
}

#' Takes a list of values and collapses them into a single valid css string.
#'
#' @param value The list of values to be collapsed
#'
#' @return A valid css value string.
collapseValues <- function(values) {
  paste0("'", paste0(values, collapse = "' '"), "'")
}

#' Generates a list of rules for a single grid attribute based on the given grid areas.
#'
#' @param attribute The grid attribute to generate rules for
#' @param areas The grid areas to generate rules based on
#' @param default_value The default css value for the size of one element
#' @param default_rule Function that returns a valid css value based on the
#'   default_value and the area details
#'
#' @return A list of attribute rules.
areaBasedRules <- function(attribute, areas, default_value, default_rule) {
  if (is.null(attribute) && !is.null(areas)) {
    attribute <- list(default = default_value)

    for (rule in names(areas)) {
      attribute[[rule]] <- default_rule(rule, areas)
    }
  }
  return(attribute)
}

#' Converts string type attributes into a named list.
#' Does nothing if the attribute is already a list in the correct format.
#'
#' @param attribute The value to process
#'
#' @return A named list.
preprocessAttribute <- function(attribute) {
  if (!is.null(attribute) && !is.list(attribute))
    return(list(default = attribute))
  return(attribute)
}

#' Adds a css class to any HTML elements from the content that are named.
#'
#' @param content A (named) list of HTML elements.
#'
#' @importFrom stringi stri_remove_empty
#' @return A list of HTML elements.
preprocessContent <- function(content) {
  for (name in stri_remove_empty(names(content))) {
    content[[name]] %<>% tagAppendAttributes(class = name)
  }
  content
}

#' Generates all required css for the given css attribute based on the grid
#' rules provided.
#'
#' @param attribute The attribute details to create rules for.
#' @param grid_id The id of the grid the rules will affect.
#' @param grid_areas The target grid area details.
#' @param css_target The equivalent css attribute name.
#' @param css_base_value The default css value for the size of one element
#' @param css_base_rule Function that returns a valid css value based on the
#'   css_base_value and the area details
#'
#' @importFrom stringi stri_remove_empty
#' @return A string of value css rules.
getAttributeStyle <- function(attribute,
                              grid_id,
                              grid_areas,
                              css_target,
                              css_base_value,
                              css_base_rule) {

  attribute %<>%
    preprocessAttribute() %>%
    areaBasedRules(grid_areas, css_base_value, css_base_rule)

  if (is.null(attribute))
    return("")

  if (is.list(attribute)) {
    attribute_css <- ""
    for (rule in names(attribute)) {
      if (rule == "default") {
        attribute_css %<>% paste0(
          generateGridRule(grid_id, css_target, paste0(attribute[[rule]]))
        )
      } else {
        attribute_css %<>%
          paste0(glue(
            ruleWrapper(activeMediaRules[[rule]]),
            rules = generateGridRule(grid_id, css_target, paste0(attribute[[rule]]))
          ))
      }
    }
    return(attribute_css)
  }

  return(generateGridRule(options$id, options$css_target, paste0(attribute)))
}
