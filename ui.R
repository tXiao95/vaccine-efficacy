library(shiny)
library(shinyjs)
library(plotly)

shinyUI(fluidPage(
      # Need this for reset to work
      useShinyjs(),
      fluidRow(

# Top Row of buttons ------------------------------------------------------

        column(6,
          titlePanel("Simulation Settings"),
          wellPanel(
            fluidRow(
              column(2, offset=2,
                     actionButton("do_sim", "Run Simulation", icon('laptop-code'))
              ),
              column(2, 
                     actionButton("reset", "Reset Inputs", icon('undo'))
              ),
              column(2,
                     actionButton("clear_posterior", "Clear Posterior", icon('eraser'))
              )
            )
          ),

# Default Prior and Data Parameters --------------------------------------------------------------

          wellPanel(
            fluidRow(
              column(4,
                     selectInput("protocol", "Select interim analysis protocol", 
                                 c("Moderna 1", "Moderna 2", "Pfizer 1", "Custom"))
              ),
              column(6, 
                     selectInput("prior", "Select prior on Vaccine Efficacy", 
                                 c("Very Skeptical", "Skeptical", "Cautiously Optimistic", "Optimistic", "Very Optimistic", "Custom"))
              )
            ),
            
            fluidRow(

# First column of data parameters -----------------------------------------

              column(4,
                     h4("Data parameters"),
                     numericInput("N", "Total subjects at enrollment", value = 3000, min = 0, step = 1),
                     numericInput("rratio", "Randomization ratio", value = 0.67, min = 0, max = .1),
                     numericInput("cases", "Total cases", value = 30, min = 0, step = 1),
                     numericInput("vax_cases", "Vaccine cases", value = 10, min = 0, step = 1)
              ),

# Second column of Gaussian mixture priors ---------------------------------

              column(6,
                     h4("Gaussian mixture prior on log(RR)"),
                     numericInput('n', 'Number of mixture components', 
                                  value = 2, 
                                  min = 0, 
                                  step = 1),
                     fluidRow(
                       column(3, uiOutput("lambda")),
                       column(3, uiOutput("mu")),
                       column(3, uiOutput("sigma"))
                     )
              ),

# Third column of Beta priors --------------------------------------------

              column(2,
                     h4("Beta prior"),
                     numericInput("alpha", "Alpha parameter", value = 4, min = 0),
                     numericInput("beta", "Beta parameter", value = 160, min = 0)
              ),
            )
          ),
          uiOutput('ve_probs_title'),
          dataTableOutput("ve_probs")
        ), 

# Plotting Side -----------------------------------------------------------
      
        column(6,
               tabsetPanel(type = "tabs",
                           tabPanel("Plot",
                                    br(),
                                    plotlyOutput("ve_prior"),
                                    br(),
                                    br(),
                                    plotlyOutput("theta_prior")
                           ),
                           #tabPanel("Console Output"),
                           tabPanel("Documentation")
                           
               )
        )
      )

))