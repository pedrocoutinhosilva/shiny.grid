defaultMediaRules <- list(
  xs = list(max = 575),
  sm = list(min = 576, max = 767),
  md = list(min = 768, max = 991),
  lg = list(min = 992, max = 1199),
  xl = list(min = 1200)
)
activeMediaRules <- defaultMediaRules

ruleWrapper <- function(options) {
  if (is.null(options$min) && is.null(options$max))
    return("{rules}")

  "@media all " %>%
    paste0(ifelse(is.null(options$min), "", glue("and (min-width: {options$min}px) "))) %>%
    paste0(ifelse(is.null(options$max), "", glue("and (max-width: {options$max}px) "))) %>%
    paste0(" {{", " {rules} ", "}} ")
}

unregisterMediaRule <- function(name) {
  activeMediaRules[[name]] = NULL
}

registerMediaRule <- function(name, min = NULL, max = NULL) {
  activeMediaRules[[name]] = list(min = min, max = max)
}
