library(shiny)
library(shinyjs)

shinyUI(fluidPage(
      # Need this for reset to work
      useShinyjs(),
      fluidRow(

# Top Row of buttons ------------------------------------------------------

        column(6,
          titlePanel("Simulation Settings"),
          wellPanel(
            fluidRow(
              column(2, offset = 3,
                     actionButton("do_sim", "Run Simulation", icon('laptop-code'))
              ),
              column(2,
                     actionButton("reset", "Reset Inputs", icon('undo'))
              )
            )
          ),

# Parameters --------------------------------------------------------------

          wellPanel(
            fluidRow(

# First column of data parameters -----------------------------------------

              column(4,
                     h4("Data parameters"),
                     numericInput("N", "Total subjects at enrollment", value = 3000, min = 0, step = 1),
                     numericInput("rratio", "Randomization ratio", value = 0.67, min = 0, max = 1),
                     numericInput("cases", "Total cases", value = 30, min = 0, step = 1),
                     numericInput("vax_cases", "Vaccine cases", value = 10, min = 0, step = 1)
              ),

# Second column of Beta priors --------------------------------------------

              column(2,
                     h4("Beta prior"),
                     numericInput("alpha", "Alpha parameter", value = .15, min = 0),
                     numericInput("beta", "Beta parameter", value = 10, min = 0)
              ),

# Third column of Gaussian mixture priors ---------------------------------

              column(6,
                     h4("Gaussian mixture prior on log(RR)"),
                     numericInput('n', 'Number of mixture components', 
                                  value = 0, 
                                  min = 0, 
                                  step = 1),
                     fluidRow(
                       column(3, uiOutput("lambda")),
                       column(3, uiOutput("mu")),
                       column(3, uiOutput("sigma"))
                     )
              )
            )
          )
        ),

# Plotting Side -----------------------------------------------------------

        column(6,
          titlePanel("Prior and Posterior Graph"),
          plotOutput("theta_prior"),
          sliderInput("theta_x", "x-axis", min=0, max=1, step=0.1, value=c(0,1)),
          plotOutput("ve_prior"),
          sliderInput("ve_x", "x-axis", min=-2, max=2, step=0.1, value=c(-.5,.5))
        )
      )
))