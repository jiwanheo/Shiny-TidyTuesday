labeled_input <- function(id, label, input) {
  div(
    id = id,
    span(label, style = "font-size: small;"),
    input
  )
}