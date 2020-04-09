generateRule <- function(id, attribute, value) {
  paste0(
    "#", id, "{",
    attribute, ": ", value, ";",
    "}"
  )
}
