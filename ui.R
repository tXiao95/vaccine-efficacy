library(shiny)
library(shinydashboard)
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
              column(3, offset=2, 
                     actionButton("do_sim", "Run Simulation", icon('laptop-code'))
              ),
              column(3, 
                     actionButton("reset", "Reset Inputs", icon('undo'))
              ),
              column(3,
                     actionButton("clear_posterior", "Clear Posterior", icon('eraser')), 
                     verbatimTextOutput("Memory")
              )
            )
          ),

          wellPanel(

# Default Select options --------------------------------------------------
 
           fluidRow(
              column(6,
                     selectInput("protocol", "Select interim analysis protocol", 
                                 list(`Pfizer` = list("Pfizer Interim 1", "Pfizer Interim 2", "Pfizer Interim 3", 
                                                      "Pfizer Interim 4", "Pfizer Final"),
                                      `Moderna` = list("Moderna Interim 1", "Moderna Interim 2", "Moderna Interim 3", 
                                                      "Moderna Interim 4", "Moderna Final"),
                                      `Custom` = list("Custom")))
              ),
              column(6, 
                     selectInput("prior", "Select prior on Vaccine Efficacy", 
                                 list(`Priors` = list("Very Skeptical", "Skeptical", "Cautiously Optimistic", "Optimistic", "Very Optimistic"),
                                      `Custom` = list("VE prior", "logRR prior")))
              )
            ),

# Option inputs -----------------------------------------------------------

            fluidRow(

              column(4,
                     h4("Data parameters"),
                     numericInput("N", "Total subjects at enrollment", value = 3000, min = 0, step = 1),
                     numericInput("rratio", "Randomization ratio", value = 0.5, min = 0, max = .1),
                     numericInput("cases", "Total cases", value = 32, min = 0, step = 1),
                     numericInput("vax_cases", "Vaccine cases", value = 6, min = 0, step = 1)
              ),
              column(8, 
                     tabsetPanel(type = "tabs", id = "options",
                                 tabPanel("Basic Settings", 
                                          h4("Prior on Vaccine Efficacy (VE)"),
                                          # Make sure default values here match default setting on server side to prevent lag
                                          column(6, 
                                                 numericInput('mu_ve', 'Mean of VE',
                                                              value = 0.0,
                                                              max = 1, 
                                                              step = 0.1
                                                 )
                                          ),
                                          column(6, 
                                                 numericInput('sigma_ve', 'Standard deviation',
                                                              value = 0.1,
                                                              min = 0, 
                                                              step = 0.1
                                                 )
                                          )
                                 ),
                                 tabPanel("Custom Settings",
                                          column(8,
                                                 h4("Prior on log(RR)"),
                                                 numericInput('n', 'Number of mixture components', 
                                                              value = 2, 
                                                              min = 0, 
                                                              step = 1),
                                                 fluidRow(
                                                   column(4, uiOutput("lambda")),
                                                   column(4, uiOutput("mu")),
                                                   column(4, uiOutput("sigma"))
                                                 )
                                          ),
                                          column(4,
                                                 h4("Beta prior"),
                                                 numericInput("alpha", "Alpha parameter", value = 4, min = 0),
                                                 numericInput("beta", "Beta parameter", value = 160, min = 0),
                                                 numericInput("samples", "Number of Samples", value = 6000, min = 0, max = 20000, step = 1000)
                                          )
                                  )
                     )
              
              )

              
            )
          ),
          uiOutput('ve_probs_title'),
          dataTableOutput("ve_probs")
        ), 

# Plotting Side -----------------------------------------------------------
      
        column(6,
               tabsetPanel(type = "tabs", id = "plots",
                           tabPanel("Plot",
                                    br(),
                                    plotlyOutput("ve_prior"),
                                    fluidRow(
                                      column(6, offset = 4, 
                                             sliderInput("ve_xaxis", "VE x-axis", value = c(-.5, 1), min = -2, max = 1, step = 0.1)
                                      )
                                    ),
                                    br(),
                                    br(),
                                    plotlyOutput("theta_prior"),
                                    fluidRow(
                                      column(6, offset = 4, 
                                             sliderInput("theta0_xaxis", "Theta0 x-axis", value = c(0, 0.05), min = 0, max = 1, step = 0.01)
                                      )
                                    )
                           ),
                           tabPanel("Console Output", 
                                    verbatimTextOutput("console")),
                           tabPanel("Documentation")
                           
               )
        )
      )

))