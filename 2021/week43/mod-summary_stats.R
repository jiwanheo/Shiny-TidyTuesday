summary_stats_ui <- function(id) {
  ns <- NS(id)
  
  div(id = "weight-summary",
      h3(textOutput(ns("section_title"))),
      DTOutput(ns("weights_summary")),
      plotOutput(ns("boxplot_us_vs_state"))
  )
}

summary_stats_server <- function(id, selected_state, not_selected_state, state, type) {
  moduleServer(id, function(input, output, session) {
    
    section_title <- reactive(paste0("Summary of ", type(), " weights (", state(), " vs U.S.)"))
    output$section_title <- renderText(section_title())
    
    plot_state <- reactive({
      req(state)
      
      ggplot() +
        geom_boxplot(data = rbind(selected_state(), not_selected_state()), 
                     aes(x = weight, y = factor(state, levels = c("Rest of U.S.", state())), fill = state)) +
        scale_x_continuous(labels = scales::comma_format()) +
        labs(title = paste0("Distribution of ", type(), " weights, produced in ", state(), " vs rest of U.S."),
             x = "Weight (lbs)",
             y = "",
             fill = "Color") +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = c(0.9, 0.85),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()
        )
    })
    
    output$boxplot_us_vs_state <- renderPlot(plot_state())
    
    output$weights_summary <- renderDT(
      list(
        selected_state(),
        not_selected_state()
      ) %>% 
        map_dfr(produce_summary),
      options = list(dom = "t", autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    )
    
  })
}