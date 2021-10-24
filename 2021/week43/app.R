# Library/Module loading----
library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(thematic)
library(shinyscreenshot)

source(here::here("2021", "week43", "my-functions.R"))
source(here::here("2021", "week43", "mod-hyp_testing.R"))
source(here::here("2021", "week43", "mod-summary_stats.R"))

# Data Processing----
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>% 
  filter(country == "United States") %>% 
  filter(!str_detect(weight_lbs, "Entries")) %>%
  separate(id, into = c("year", "type"), sep = "\\-") %>% 
  select(year, type, weight = weight_lbs, state = state_prov) %>%
  mutate(
    type = case_when(
      type == "F" ~ "Field Pumpkin",
      type == "P" ~ "Giant Pumpkin",
      type == "S" ~ "Giant Squash",
      type == "W" ~ "Giant Watermelon",
      type == "L" ~ "Long Gourd",
      type == "T" ~ "Tomato"
    )
  ) %>% 
  mutate(weight = str_remove_all(weight, "\\,")) %>% 
  mutate(weight = as.numeric(weight))

# Theming----

my_theme <- bs_theme(
  bootswatch = "sandstone"
) %>% 
  bs_add_rules(sass::sass_file(here::here("2021", "week43", "style.scss")))

thematic_shiny(font = "auto")

# App----
ui <- fluidPage(
  theme = my_theme,
  div(id = "body",
      div(id = "app-title",
          titlePanel(HTML("Are they <u><em>actually</em></u> giant?"))),
      div(id = "explanation",
          p(paste0("The dataset for week 43 of #TidyTuesday deals with various weights of big pumpkins! ",
                   "Focusing on the United States, users of this app can easily test if certain states produce significatly bigger pumpkins than the rest of the country! ",
                   "Mann-Whitney U test was the method of choice, because of the non-normal distribution of the pumpkin weights."))),
      div(id = "inputs",
          selectInput("type", "Select type", choice = sort(unique(pumpkins$type)), selected = "Giant Pumpkin"),
          selectInput("state", "Select state", choices = NULL),
          actionButton("screenshot", "Take a screenshot!")),
      div(id = "to-screenshot",
          hyp_testing_ui("hyp_testing"),
          summary_stats_ui("summary_stats"))
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$type, {
    updateSelectInput(inputId = "state", choices = sort(unique(filter(pumpkins, type == input$type)$state)))
  })
  
  selected_state <- reactive({
    req(input$state)
    
    pumpkins %>% 
      filter(type == input$type,
             state == input$state)
  })
  
  not_selected_state <- reactive({
    req(input$state)
    
    pumpkins %>% 
      filter(type == input$type,
             state != input$state) %>% 
      mutate(state = "Rest of U.S.")
  })
  
  summary_stats_server("summary_stats", selected_state = selected_state, not_selected_state = not_selected_state, state = reactive(input$state), type = reactive(input$type))
  hyp_testing_server("hyp_testing", selected_state = selected_state, not_selected_state = not_selected_state, state = reactive(input$state), type = reactive(input$type))
  
  observeEvent(input$screenshot, {
    screenshot(id = "to-screenshot")
  })
  
}

shinyApp(ui, server)