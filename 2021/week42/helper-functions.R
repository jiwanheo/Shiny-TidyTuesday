labeled_input <- function(id, class = NULL, label, input) {
  div(
    id = id,
    class = class,
    span(label, style = "font-size: 0.8rem;"),
    input
  )
}