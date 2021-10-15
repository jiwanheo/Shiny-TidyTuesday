# Load lib/mod/data ----
library(shiny)
library(bslib)
library(showtext)
library(thematic)
library(tidyverse)

source(here::here("2021", "week42", "mod-plot.R"))
source(here::here("2021", "week42", "helper-functions.R"))

raw_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

# Custom themes ----
my_theme <- bs_theme(
  bootswatch = "materia",
  base_font = font_google("Secular One")
) %>% 
  bs_add_rules(sass::sass_file(here::here("2021", "week42", "style.scss")))

thematic_shiny(font = "auto")

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
  theme = my_theme,
  div(
    id = "app-title",
    titlePanel("Seafood Production Explorer")
  ),
  div(
    id = "input1",
    labeled_input(id = "select-country", 
                  label = "Select Country",
                  selectInput("country", NULL, choices = unique(df$country), selected = "Canada")),
    labeled_input(id = "select-year",
                  label = "Select Year",
                  sliderInput("year", NULL, min = 1961, max = 2013, value = c(1961, 2013), sep = ""))
  ),
  div(
    id = "input2",
    labeled_input(id = "plot_world_radio_btn",
                  label = "Show all types in 1 plot",
                  radioButtons("plot_all_fish", NULL, choices = c("On", "Off"), selected = "Off")),
    labeled_input(id = "plot_world_radio_btn",
                  label = "Compare against world",
                  radioButtons("plot_world", NULL, choices = c("On", "Off"), selected = "Off"))
  ),
  div(
    id = "page-title",
    textOutput("page_title")
  ),
  plot_ui("plot"),
  tableOutput("table")
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