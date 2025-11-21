# app.R for week 10

library(shiny)
library(tidyverse)
library(viridis)

# Load and prepare the data ----------------------------------------------

# We load the same therapy dataset as in previous weeks.
# This assumes that the file "therapy_dataset.csv" is stored in a folder
# called "data" inside your project.
# As we did previously, we first read the data, then preprocess some of the
# variables to match their natural order
therapy_df <- readr::read_csv("data/therapy_dataset.csv")

therapy_df <- therapy_df |>
  mutate(
    support_level = factor(support_level, levels = c("Low", "Medium", "High")),
    therapy       = factor(therapy,       levels = c("Yes", "No"))
  )

# Set a simple base theme similar to previous weeks
theme_set(theme_minimal(base_size = 14))


# UI ---------------------------------------------------------------------

# For Week 10, we extend the Week 9 app in three main ways:
#  1. Use a sidebar layout with inputs on the left and outputs on the right.
#  2. Use tabsetPanel() and tabPanel() to create multiple tabs for our plots.
#  3. Reuse the same user-selected outcome and support filter across both tabs.

ui <- fluidPage(
  
  # App title
  titlePanel("Wellbeing and Loneliness Dashboard"),
  
  # Short description
  p("Use the controls on the left to explore factors influencing wellbeing and loneliness."),
  
  # Sidebar layout: inputs (menus, checkboxes) on the left, outputs (plots) on the right
  sidebarLayout(
    
    # Sidebar with input controls (these are global for the whole app)
    sidebarPanel(
      
      # Dropdown menu for outcome -----------------------------------------------
      #
      # A dropdown menu for choosing which outcome variable to explore.
      # As in Week 9:
      # - The values that the user chooses are stored as a a component of an input object
      # - We will set an `inputId`, which will be the name of the value in the input object
      #   (for this dropdown menu, the inputId is "outcome")
      # - We will set a `label`, which is the label of the dropdown menu shown to the user.
      # - We will set the `choices` in the menu as a named vector:
      #     * the name (e.g., "Wellbeing score") is what the user sees.
      #     * the value (e.g., "wellbeing_score") is what is stored in input$outcome.
      # - We will set the default choice using `selected`
      #
      # NOTE: This means input$outcome will be a *string* like "wellbeing_score".
      # We will use this string to control the output in a new way this week!
      selectInput(
        inputId = "outcome",
        label   = "Outcome to explore:",
        choices = c(
          "Wellbeing score"  = "wellbeing_score",
          "Loneliness score" = "loneliness_score"
        ),
        selected = "wellbeing_score"
      ),
      
      # Checkbox filter for support level -----------------------------------------
      #
      # A set of checkboxes that lets the user choose which support levels
      # to include in the plots.
      #
      # As in Week 9:
      # - The values that the user chooses are stored as a a component of an input object
      # - We will set an `inputId`, which will be the name of the value in the input object
      #   (for these checkboxes, the inputId is "support_filter")
      # - We will set a `label`, which is the label of the checkboxes shown to the user.
      # - We will set the checkbox `choices` as a vector. The values of this vector
      #   are both the names of the checkboxes that the user will see, and the value 
      #   that will be stored if checked by the user. 
      # - We will set `selected` to set the default checkboxes to be checked. Here,
      #   as the default, we set all the checkboxes to be selected (so that all support
      #   levels are shown in the plots by default)
      #
      # NOTE: this means that input$support_filter will be a *vector* of strings like
      #   c("Low", "High"). Below, we will use this vector filter the data in a
      #   reactive() expression that filters the data shown in all of the plots in
      #   the dashboard.
      checkboxGroupInput(
        inputId  = "support_filter",
        label    = "Include support levels:",
        choices  = c("Low", "Medium", "High"),
        selected = c("Low", "Medium", "High")
      )
      
    ),
    
    # Main panel with tabs for different output plots -----------------------------
    mainPanel(
      # A set of tabs using tabsetPanel().
      #
      # Each tabPanel() creates one tab. In comparison to week 9, in which we showed
      # just one output, now we specify output(s) for each tab.
      tabsetPanel(
        
        # Tab 1: Effects of support
        # This tab will show the effects of support level on the chosen outcome,
        # and include the support levels checked in the support_filter checkboxes
        tabPanel(
          title = "Support",
          plotOutput("support_plot")
        ),
        
        # Tab 2: Effects of therapy 
        # This tab shows the effects of therapy on the chosen outcome, and include 
        # the support levels checked in the support_filter checkboxes. 
        # In addition, we add another input control checkbox *just for this tab only*,
        # in which the user can choose to breakdown the plot into facets based on 
        # support
        tabPanel(
          title = "Therapy",
          
          # This checkbox is specific to this tab: it only affects the therapy
          # plot.
          checkboxInput(
            inputId = "facet_by_support",
            label   = "Show breakdown by support level",
            value   = FALSE
          ),
          
          plotOutput("therapy_plot")
        )
      )
    )
  )
)


# Server -----------------------------------------------------------------

# The server function contains the code that:
#  - reads the user inputs from the "input" object
#  - uses those inputs to create and update the outputs in the "output" object
#
# In this week 10 app, the key new ideas are:
#  - Using reactive() to create a function that will filter the dataset to include
#    only the support levels selected by the user. This filter function will be used
#    in all the plots, so that only the selected support levels are shown in any plot.
#  - Using .data[[input$outcome]] inside ggplot() to connect the user's outcome choice
#    (stored as a string like "wellbeing_score") to the actual column in the data.
#  - Using a checkbox to toggle faceting on and off.

server <- function(input, output, session) {
  
  # In the server function, we:
  # - Create a filter that filters the data to include only the support levels the 
  #   user selects using. To make this responsive to user selections, we'll create
  #   this function using a reactive() expression
  # - Create the plot to show in the Support tab
  # - Create the plot to show in the Therapy tab
  
  
  # Filter data using reactive() expression --------------------------------------
  #
  # Above, one of our input controls was a set of checkboxes called `support_filter`.
  # These checkboxes allow the user to specify which support levels to show in
  # any of the plots in the app: Low, Medium, and/or High. 
  # This means that input$support_level contains a vector containing the values 
  # "Low", "Medium", and/or "High". For example, since all support_filter checkboxes
  # are checked by default, the default value of this vector is: 
  # c("Low", "Medium", "High")
  #
  # Our goal us to use these values to filter the therapy_df dataset, so that 
  # the filtered dataset includes only the support_levels the user checked.
  #
  # Essentially, we will run *familiar tidyverse wrangling* code whenever the values 
  # of support_filter change:
  # therapy_df |>
  #   filter(support_level %in% input$suport_filter)
  #
  # How do we get this code to run any time the values of support_filter change?
  # We will do this using a *reactive expression*. 
  # Our reactive expression will use reactive() to create a *function* called
  # filtered_data(), which will run our familiar tidyverse wrangling code for us.
  #
  # Why is this useful?
  #  - We only have to write the filtering code once.
  #  - All plots can reuse the same filtered dataset.
  #
  # Without creating a filtering function using reactive(), we would need to repeat
  # our filtering code separately inside each each output plot that we create using
  # renderplot() below.
  # Having to repeat this would be more error prone (e.g., each time we repeat it, we
  # might make a typo somewhere). In addition, if this were an app we wanted to continue 
  # to use and update over time (e.g., add more tabs for more output plots), it would be
  # a pain to have to repeat the filtering code each time. 
  #
  # Here is our reactive expression. You see that we use reactive() to define a 
  # function called filtered_data, which will just contain the simple, familiar
  # tidyverse wrangling code you saw above!
  filtered_data <- reactive({
    therapy_df |>
      filter(support_level %in% input$support_filter)
  })
  
  
  
  # Tab 1: Support ---------------------------------------------------------------
  #
  # This tab shows a plot that answers the question: How does the chosen outcome 
  # (selected in the dropdown menu) vary across support levels?
  #
  # This tab shows:
  #  - support_level on the x-axis
  #  - the chosen outcome (from the dropdown menu) on the y-axis
  #  - a boxplot summarising the outcome in each support group
  #
  # Key ideas:
  #   - We will use the filtered_data() function defined above so that the plot
  #     uses the filtered data
  #   - Instead of lengthy if... else... blocks, we will quickly reference 
  #     the user's selected outcome variable to plot using .data[[input$outcome]] 
  #     in our ggplot code.
  
  output$support_plot <- renderPlot({
    
    # Use a quick if... else... block to define the y-axis label based on the 
    # user's selected outcome
    if (input$outcome == "wellbeing_score") {
      y_lab <- "Wellbeing score"
    } else {
      y_lab <- "Loneliness score"
    } 
    
    # Now we will create our plot.
    # 
    # We will start by using filtered_data() to filter the data, then pipe
    # the filtered data into the plot. 
    #
    # In our ggplot code, we will use some simple but clever code to 
    # plot the user's selected outcome variable on the y-axis. 
    # In normal ggplot code, we first specify the dataset, then reference
    # one of its columns within aes(y_axis = ...) to set the y-axis. 
    # But now, the y-axis we want will depend on the outcome variable the user
    # selects. 
    # We will use the following expression to grab the y-axis variable from the
    # user's selection:
    #
    #   .data[[input$outcome]]
    #
    # This means: "use the column from the data that matches the string stored in 
    # input$outcome"
    # So if the user selects Wellbeing, the value of input$outcome will be 
    # "wellbeing_score", and .data[[input$outcome]] will tell ggplot to use
    # the column from the data called "wellbeing_score". 
    # This means that we don't need to use lengthy if... else... blocks to create
    # different ggplots with different y-axis variables as we did in Week 9.
    filtered_data() |>
      ggplot(aes(
        x    = support_level,
        y    = .data[[input$outcome]],  # look up the chosen outcome column
        fill = support_level
      )) +
      geom_boxplot(alpha = 0.8, width = 0.6, show.legend = FALSE) +
      scale_fill_viridis_d(option = "plasma", end = 0.9) +
      labs(
        x = "Support level",
        y = y_lab
      )
  })
  
  
  # Tab 2: Therapy & support --------------------------------------------
  #
  # This tab shows a plot that answers the question: How does the chosen outcome 
  # vary across therapy status? 
  # In addition, the user can check a box within this tab to see how this pattern
  # depends on support level. 
  # This tab shows therapy (Yes/No) on the x-axis and the chosen outcome on
  # the y-axis. The checkbox inside this tab controls whether we also break
  # the plot down by support_level using facet_wrap().
  #
  # Again, we use:
  #   - filtered_data() so the support filter applies here too.
  #   - .data[[input$outcome]] to grab the user's outcome choice
  
  output$therapy_plot <- renderPlot({
    
    # Decide on a y-axis label based on the selected outcome
    if (input$outcome == "wellbeing_score") {
      y_lab <- "Wellbeing score"
    } else {
      y_lab <- "Loneliness score"
    } 
    
    # Start with an overall therapy vs outcome boxplot.
    therapy_plot <- filtered_data() |>
      ggplot(aes(
        x    = therapy,
        y    = .data[[input$outcome]],  # chosen outcome column
        fill = therapy
      )) +
      geom_boxplot(alpha = 0.8, width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = c("darkblue", "gray")) +
      labs(
        x = "Therapy",
        y = y_lab
      )
    
    # If the user checks "Show breakdown by support level", then we add
    # facet_wrap(~ support_level) to show separate panels for Low/Medium/High.
    #
    # This uses the same if (...) pattern seen in Week 9, but now we are
    # toggling faceting
    if (input$facet_by_support) {
      therapy_plot <- therapy_plot + facet_wrap(~ support_level)
    }
    
    therapy_plot
  })
}


# Run the app ------------------------------------------------------------

# shinyApp() ties together the ui and server and launches the app
# when you run app.R.
shinyApp(ui = ui, server = server)
