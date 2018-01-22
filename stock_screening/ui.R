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
  titlePanel("Value Investing Exploration"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

      # radio Buttons for choosing the type of regression
      radioButtons("regression", "Regression Type (Scatter plot only)",
                   choices = c("Linear", "Non-linear"),
                   selected = "Linear"),
      # radio Buttons for choosing the type of bar plots
      radioButtons("graphtype", "Graph Type",
                   choices = c("Scatter", "Histogram"),
                   selected = "Scatter"),
      # slider bars for four key parameters
      sliderInput("ROE",
                  "Return on Equity:",
                  min = -0.25,
                  max = 0.5,
                  value = c(-0.25, 0.5)),
      sliderInput("DEratio",
                  "Debt to Equity Ratio:",
                  min = -0.25,
                  max = 3,
                  value = c(-0.25, 3)),
      sliderInput("Profit_Margin",
                  "Profit Margin:",
                  min = -0.2,
                  max = 0.4,
                  value = c(-0.2, 0.4)),
      sliderInput("PERatio",
                  "Price to Earning Ratio:",
                  min = 0,
                  max = 40,
                  value = c(0, 40))
    ),
    

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
