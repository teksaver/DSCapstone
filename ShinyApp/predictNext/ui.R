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
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
       sliderInput("words",
                   "Number of words recommandations:",
                   min = 1,
                   max = 5,
                   value = 3),
       dataTableOutput('predict')
    ),

    # Show a plot of the generated distribution
    mainPanel(
       tableOutput("predicted")
    )
  )
))
