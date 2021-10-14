# Load lib/mod/data ----
library(shiny)
library(tidyverse)
source(here::here("2021", "week42", "mod-plot.R"))
raw_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

# Data prep ----
df <- raw_df %>% 
  janitor::clean_names() %>% 
  filter(!is.na(code) & !str_detect(code, "^OWID|ANT")) %>% 
  rename(country = entity) %>% 
  select(-code) %>% 
  pivot_longer(3:9, names_to = "fish_type", values_to = "tonnes") %>% 
  filter(!is.na(tonnes)) %>% 
  mutate(fish_type = str_remove(fish_type, ".*equivalent_"),
         fish_type = str_remove(fish_type, "_27.*"),
         fish_type = str_remove(fish_type, "_other"),
         fish_type = snakecase::to_title_case(fish_type)) %>% 
  filter(fish_type != "Molluscs") 

# App UI ----
ui <- fluidPage(
  fluidRow(
    column(3, 
           selectInput("country", "Select Country", choices = unique(df$country), selected = "Canada"),
           sliderInput("year", "Year", min = 1960, max = 2013, value = c(1961, 2013), sep = ""),
           radioButtons("plot_all_fish", "Show all types in 1 plot", choices = c("On", "Off"), selected = "Off"),
           radioButtons("plot_world", "Compare against world", choices = c("On", "Off"), selected = "Off")),
    column(9, 
           textOutput("page_title"),  
           plot_ui("plot"))
  ),
  fluidRow(
    tableOutput("table")
  )
)

# App server ----
server <- function(input, output, session) {
  
  page_title     <- reactive(paste0("Seafood Production in ", input$country, " between 1960 - 2013 (tonnes)"))
  selected       <- reactive(df %>% filter(country == input$country, between(year, input$year[1], input$year[2])))
  not_selected   <- reactive(setdiff(df %>% filter(between(year, input$year[1], input$year[2])), selected()))
  wider_selected <- reactive(selected() %>% pivot_wider(names_from = "fish_type", values_from = "tonnes"))
  
  plot_all_fish  <- reactive(input$plot_all_fish)
  plot_world     <- reactive(input$plot_world)
  
  observeEvent(input$plot_all_fish, { # `plot_world` option is only available when `plot_all_fish` is On
    if(input$plot_all_fish == "On") {
      updateRadioButtons(session, "plot_world",
                         choices = "Off")
    }
    else {
      updateRadioButtons(session, "plot_world",
                         choices = c("On", "Off"),
                         selected = "Off")
    }
  })
  
  output$page_title <- renderText(page_title())
  plot_server("plot", selected, not_selected, plot_all_fish, plot_world)
  output$table <- renderTable(wider_selected())
  
}

shinyApp(ui, server)