hyp_testing_ui <- function(id) {
  ns <- NS(id)
  
  div(id = "statistical-tests",
      h3("Hypothesis Testing"),
      verbatimTextOutput(ns("hyp_statement")),
      verbatimTextOutput(ns("test_res"))
  )
}

hyp_testing_server <- function(id, selected_state, not_selected_state, state, type) {
  moduleServer(id, function(input, output, session) {
    
    hyp_statement <- reactive({
      req(state)
      paste0("Ho: The median weight of ", type(), " produced in ", 
             state(), " is not significantly greater than the rest of U.S.",
             
             "\nHa: The median weight of ", type(), " produced in ", 
             state(), " is significantly greater than the rest of U.S.")
    })
    
    output$hyp_statement <- renderText(hyp_statement())
    
    test_result <- reactive({
      req(state)
      wilcox.test(selected_state()$weight, not_selected_state()$weight, alternative = "greater")
    })
    test_result_text <- reactive({paste0("Mann-Whitney U test p-value = ", round(test_result()$p.value, digits = 4), 
                                         " W-statistic: ", test_result()$statistic, "\n", 
                                         "Therefore, we ", ifelse(test_result()$p.value < 0.05, "reject", "accept"),
                                         " Ho with 95% confidence level.", "\n",
                                         ifelse(test_result()$p.value < 0.05, "Giant!", "Not giant!")
                                         )})
    output$test_res <- renderText(test_result_text())
  })
}