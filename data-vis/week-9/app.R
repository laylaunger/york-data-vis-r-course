# app.R

library(shiny)
library(tidyverse)
library(viridis)

# Load and prepare the data ----------------------------------------------

# Loading and minimal processing as we have seen in previous weeks
therapy_df <- readr::read_csv("data/therapy_dataset.csv") |>
  mutate(
    support_level = factor(support_level, levels = c("Low", "Medium", "High")),
    therapy       = factor(therapy,       levels = c("Yes", "No")),
    sex           = factor(sex)
  )

# Set a base theme as we have seen in previous weeks
theme_set(theme_minimal(base_size = 14))


# UI ---------------------------------------------------------------------

# Set up the user interface (ui)

ui <- fluidPage(
  
  # Give the webpage a title
  titlePanel("Support, Therapy, and Wellbeing: Interactive"),
  
  # Now add a subtitle with instructions for the user.
  # This is accomplished using a little function called p, which stands for "paragraph"
  p("Use the controls below to explore how outcomes vary across support levels."),
  
  # Now set up some interactive input options for the user.
  # User inputs come in a variety of types, such as dropdown menus and checkboxes.
  # We will create each interactive input option with a function: 
  # -- selectInput creates a dropdown menu as an interactive input option
  # -- checkboxInput creates a checkbox as an interactive input option
  # -- other functions create other input types
  
  # These functions will store user inputs as *parts* of one big object called *input*.
  # Later, in the server, we will reference these parts of the input object to 
  # figure out how to respond to the user input so that we show them what they want on 
  # the screen.
  # Thus, the parts stored within the input object will be updated based on the user input.
  
  # Each time we use a function to create a user input option, we will specify some arguments
  # in the function that will determine how the input object part behaves. 
  # At minimum, we wil specify:
  # -- an inputId, which will be the name of the part stored in the input object
  # -- a label, which will be shown to the user

  # Depending on the type of user input option, we will specify additional arguments.
  
  # First, we will create a dopdown menu input using the selectInput() function.
  # Later, in the server section, we will use this to determine which outcome variable
  # to plot on the y-axis of our plot.
  # For dropdown menu inputs, we will: 
  # -- use the choice argument to specify the dropdown menu choices as a vector (here,
  #    this will be two of the possible outcome variables to plot)
  # -- use the selected argument to specify the default choice
  selectInput(
    inputId = "outcome",
    label   = "Outcome to plot:",
    choices = c(
      "Wellbeing score" = "wellbeing_score",
      "Loneliness score" = "loneliness_score"
    ),
    selected = "wellbeing_score"
  ),
  
  # Second, we will create a checkbox input using the checkboxInput() function.
  # Later, in the server section, we will use this to determine whether to show
  # individual points on our plot.
  # For checkbox inputs, we will:
  # -- use the value argument to specify that the default value of the checkbox
  #    when it is unchecked is FALSE (this means that if it *is* checked by the user,
  #    the value switches to TRUE). 
  checkboxInput(
    inputId = "show_points",
    label   = "Show individual data points",
    value   = FALSE
  ),
  
  # Finally, we include a plotOutput to show a plot on the screen to the user.
  # This plot will be taken from a plot that we will create using the server
  # function below. That server function will create a plot based on user input,
  # and store it as a part of an *output* object called "support_plot".
  plotOutput("support_plot")
)


# Server -----------------------------------------------------------------

# Set up the server, which will contain code that dynamically uses the user 
# input to control what is shown in the web app

# We create the server using a function.

# The server function will take three arguments: input, output, and session.
# - input  : The input object referred to above. This is an object with multiple parts.
#            As described above, each part corresponds to a user input option. That
#            part contains the *current values* of all the user inputs defined in the UI. 
#            For example, input$outcome contains the user's choice for the outcome dropdown
#            menu, and input$show_points contains the user's choice for the show_points
#            checkbox. 
# - output : This will be another object with multiple parts that we fill with 
#            things we want to show in the app. For example, when we write 
#            output$support_plot <- renderPlot({ ... }), we are telling Shiny how to create 
#            the plot that appears in plotOutput("support_plot") above, in the ui section.
# - session: an object that contains information about the current user's session
#            and allows more advanced features (like updating inputs from the server).
#            We are not using session directly in this simple app, but Shiny includes
#            it here for completeness.
server <- function(input, output, session) {
  
  # We will have one renderPlot function that shows a plot, and uses
  # user input to choose between different options for what to plot.
  # Within this function, the plot will be stored as an object called 
  # output_plot that is shown on the screen at the end of the function. 
  output$support_plot <- renderPlot({
    
    
    # We will first read the user's input to figure out which outcome variable
    # should be shown on the y-axis - wellbeing_score, or loneliness_score.
    # We will then specify output as a boxplot using ggplot code, with the y-axis
    # chosen by the user.
    
    # If the user chose wellbeing_score...
    if(input$outcome == "wellbeing_score") {
      
      # Then create output_plot as a boxplot with wellbeing_score on the y-axis
      output_plot <- therapy_df %>%
        ggplot(aes(x = support_level, y = wellbeing_score)) +
        geom_boxplot(
          aes(fill = support_level),
          alpha       = 0.8,
          width       = 0.6,
          show.legend = FALSE
        ) +
        scale_fill_viridis_d(option = "plasma") +
        labs(
          x = "Support level",
          y = "Wellbeing score"
        ) +
        coord_cartesian(ylim = c(0, 100))
    } else {
      
      # Otherwise (i.e., if the user did NOT choose wellbeing_score), 
      # create the plot using loneliness_score as the y-axis variable
      output_plot <- therapy_df %>%
        ggplot(aes(x = support_level, y = loneliness_score)) +
        geom_boxplot(
          aes(fill = support_level),
          alpha       = 0.8,
          width       = 0.6,
          show.legend = FALSE
        ) +
        scale_fill_viridis_d(option = "plasma") +
        labs(
          x = "Support level",
          y = "Loneliness score"
        ) +
        coord_cartesian(ylim = c(0, 100))
    }
    
    # Next, check whether the user checked the box to show points.
    # If the user checks the box to show points, then input$show_points == TRUE.
    # If this is the case, add raw points overlay
    if (input$show_points) {
      output_plot <- output_plot +
        geom_point(position = position_jitter(width = 0.1),
          alpha = 0.4
        )
    }
    
    # Now the plot is fully specified.
    # All we do now is call the plot to show it on the screen
    output_plot
  })
}


# Run the app ------------------------------------------------------------

shinyApp(ui = ui, server = server)
