
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Trip Cost Calculator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Choose Car 1:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3('Main Panel text'),
      plotOutput("distPlot")
    )
  )
))