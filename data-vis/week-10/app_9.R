# app.R

library(shiny)
library(tidyverse)
library(viridis)

# Load and prepare the data ----------------------------------------------

# Load the wellbeing dataset from the data/ folder in your project
wellbeing_df <- readr::read_csv("data/wellbeing_dataset.csv") |>
  mutate(
    participant_group = factor(
      participant_group,
      levels = c("student", "employed", "retired")
    )
  )

# Set a simple default theme for all plots
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank()
    )
)

# UI ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Wellbeing and Sleep Dashboard"),
  
  p(
    "Use the controls on the left to explore wellbeing and sleep ",
    "across participant groups."
  ),
  
  sidebarLayout(
    
    # Sidebar with inputs
    sidebarPanel(
      # Outcome dropdown
      selectInput(
        inputId = "outcome",
        label   = "Outcome to explore:",
        choices = c(
          "Wellbeing index" = "wellbeing_index",
          "Sleep hours"     = "sleep_hours"
        ),
        selected = "wellbeing_index"
      ),
      
      # Group filter checkboxes
      checkboxGroupInput(
        inputId  = "group_filter",
        label    = "Participant groups to include:",
        choices  = c("student", "employed", "retired"),
        selected = c("student", "employed", "retired")
      )
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        
        # Tab 1: Groups
        tabPanel(
          title = "Groups",
          plotOutput("group_plot")
        ),
        
        # Tab 2: Distributions
        tabPanel(
          title = "Distributions",
          checkboxInput(
            inputId = "facet_by_group",
            label   = "Show separate panels for each group",
            value   = FALSE
          ),
          plotOutput("distribution_plot")
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive filtered dataset
  #
  # This expression returns a version of wellbeing_df that includes only
  # the participant groups that are currently selected in input$group_filter.
  # Because it's reactive(), it reruns automatically whenever the user
  # changes the group checkboxes.
  filtered_data <- reactive({
    wellbeing_df |>
      filter(participant_group %in% input$group_filter)
  })
  
  # Tab 1: Groups plot ---------------------------------------------------
  #
  # Shows the selected outcome (wellbeing_index or sleep_hours) on the y-axis
  # and participant_group on the x-axis, using boxplots. The plot uses the
  # filtered_data() reactive so only selected groups appear.
  
  output$group_plot <- renderPlot({
    
    # Choose a y-axis label based on the selected outcome
    if (input$outcome == "wellbeing_index") {
      y_lab <- "Wellbeing index"
    } else {
      y_lab <- "Sleep hours"
    }
    
    filtered_data() |>
      ggplot(
        aes(
          x    = participant_group,
          y    = .data[[input$outcome]],
          fill = participant_group
        )
      ) +
      geom_boxplot(alpha = 0.8, width = 0.6, show.legend = FALSE) +
      scale_fill_viridis_d(option = "plasma", end = 0.9) +
      labs(
        x = "Participant group",
        y = y_lab
      )
  })
  
  # Tab 2: Distributions plot -------------------------------------------
  #
  # Shows the distribution of the selected outcome, coloured by group.
  # The checkbox input$facet_by_group controls whether we facet by
  # participant_group (separate panel for each group) or overlay all groups
  # in a single panel.
  
  output$distribution_plot <- renderPlot({
    
    # Choose an x-axis label based on the selected outcome
    if (input$outcome == "wellbeing_index") {
      x_lab <- "Wellbeing index"
    } else {
      x_lab <- "Sleep hours"
    }
    
    # Base plot: distribution of the chosen outcome
    dist_plot <- filtered_data() |>
      ggplot(
        aes(
          x    = .data[[input$outcome]],
          fill = participant_group
        )
      ) +
      geom_density(alpha = 0.5) +
      scale_fill_viridis_d(option = "plasma", end = 0.9) +
      labs(
        x    = x_lab,
        y    = "Density",
        fill = "Group"
      )
    
    # Optionally facet by group if the checkbox is ticked
    if (input$facet_by_group) {
      dist_plot <- dist_plot +
        facet_wrap(~ participant_group)
    }
    
    dist_plot
  })
}

# Run the app ------------------------------------------------------------

shinyApp(ui = ui, server = server)