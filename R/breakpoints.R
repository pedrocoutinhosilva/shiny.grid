defaultMediaRules <- list(
  xs = list(max = 575),
  sm = list(min = 576, max = 767),
  md = list(min = 768, max = 991),
  lg = list(min = 992, max = 1199),
  xl = list(min = 1200)
)
activeMediaRules <- defaultMediaRules

getRuleWrapper <- function(options) {
  if (is.null(options$min) && is.null(options$max))
    return ("{rules}")

  wrapper <- "@media all "

  if( !is.null(options$min))
    wrapper <- paste0(wrapper, "and (min-width:", options$min, "px) ")

  if( !is.null(options$max))
    wrapper <- paste0(wrapper, "and (max-width:", options$max, "px) ")

  return (paste0(wrapper, "{{", " {rules} ", "}} "))
}

unregisterMediaRule <- function(name) {
  activeMediaRules[[name]] = NULL
}

registerMediaRule <- function(name, min = NULL, max = NULL) {
  activeMediaRules[[name]] = list(
    min = min,
    max = max
  )
}
