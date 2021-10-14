plot_ui <- function(id) {
  plotOutput(NS(id, "plot"))
}

plot_server <- function(id, selected, not_selected, plot_all_fish, plot_world) {
  
  moduleServer(id, function(input, output, session) {
    
    world_lines <- reactive({ # Whether to draw a bunch of grey lines or not
      if(plot_world() == "On") {
        geom_line(data = not_selected(), aes(x = year, y = tonnes, group = country), color = "grey70")
      }
      else{
        NULL
      }
    })
    
    base_plot <- reactive({ # base ggplot layer, we're going to add facet or not, in the next
      ggplot() +
        world_lines() +
        scale_y_continuous(labels = scales::comma_format(), breaks = scales::pretty_breaks(n = 3)) +
        labs(x = "", y = "")
    })
    
    fish_facet <- reactive({ # If Off, group by country and facet, if so, group by fish_type
      if(plot_all_fish() == "Off") {
        base_plot() +
          geom_line(data = selected(), aes(x = year, y = tonnes, group = country), color = "red") +
          facet_wrap(~fish_type, scales = "free_y", nrow = 3)
      }
      else{
        base_plot() +
          geom_line(data = selected(), aes(x = year, y = tonnes, color = fish_type)) +
          labs(color = "Fish Type")
      }
    })
    
    output$plot <- renderPlot(fish_facet())
  })
}