#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Will's predicube"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        textInput("ngram","Please enter your phrase here:","Hello"),
        sliderInput("nb_words",
                   "Number of words recommandations:",
                   min = 1,
                   max = 5,
                   value = 3)
    ),

    # Show a plot of the generated distribution
    mainPanel(
        h2("Welcome"),
        p("Welcome ! I'm Will, and I will predict your will. Please wait until the first prediction appears for the \"Hello\" phrase. Then please input your own phrase inside the text box on the side bar, and enjoy! you can ajust the number of predicted words using the slider."),
        code("Please note that the initial loading time only appears if shiny.io has unloaded the app. If the app is already running, you should get instant results!"),
        h2("Your prediction table"),
            wellPanel(
                tableOutput("predicted")
            )
        )
    )
  )
)
