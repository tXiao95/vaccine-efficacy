library(shiny)
library(shinyjs)
library(plotly)

ui <- htmlTemplate(
  "www/index.html"#,
  # n = numericInput("N", "Total subjects at enrollment", value = 3000, min = 0, step = 1),
  # rand_ratio = numericInput("rratio", "Randomization ratio", value = 0.67, min = 0, max = .1),
  # cases = numericInput("cases", "Total cases", value = 30, min = 0, step = 1),
  # vax_cases = numericInput("vax_cases", "Vaccine cases", value = 10, min = 0, step = 1)
)

# shinyUI(fluidPage(
#       # Need this for reset to work
#       useShinyjs(),
#       fluidRow(

# # Top Row of buttons ------------------------------------------------------

#         column(6,
#           titlePanel("Simulation Settings"),
#           wellPanel(
#             fluidRow(
#               column(2, offset=2,
#                      actionButton("do_sim", "Run Simulation", icon('laptop-code'))
#               ),
#               column(2, 
#                      actionButton("reset", "Reset Inputs", icon('undo'))
#               ),
#               column(2,
#                      actionButton("clear_posterior", "Clear Posterior", icon('eraser'))
#               )
#             )
#           ),

# # Default Prior and Data Parameters --------------------------------------------------------------

#           wellPanel(
#             fluidRow(
#               column(4,
#                      selectInput("protocol", "Select interim analysis protocol", 
#                                  c("Pfizer Interim 1", "Pfizer Interim 2", "Pfizer Interim 3", 
#                                    "Pfizer Interim 4", "Pfizer Final", "Custom"))
#               ),
#               column(6, 
#                      selectInput("prior", "Select prior on Vaccine Efficacy", 
#                                  c("Very Skeptical", "Skeptical", "Cautiously Optimistic", "Optimistic", "Very Optimistic", "Custom"))
#               )
#             ),
            
#             fluidRow(

# # First column of data parameters -----------------------------------------

#               column(4,
#                      h4("Data parameters"),
#                      numericInput("N", "Total subjects at enrollment", value = 3000, min = 0, step = 1),
#                      numericInput("rratio", "Randomization ratio", value = 0.67, min = 0, max = .1),
#                      numericInput("cases", "Total cases", value = 30, min = 0, step = 1),
#                      numericInput("vax_cases", "Vaccine cases", value = 10, min = 0, step = 1)
#               ),

# # Second column of Gaussian mixture priors ---------------------------------

              # column(6,
              #        h4("Gaussian mixture prior on log(RR)"),
              #        numericInput('n', 'Number of mixture components', 
              #                     value = 2, 
              #                     min = 0, 
              #                     step = 1),
              #        fluidRow(
              #          column(3, uiOutput("lambda")),
              #          column(3, uiOutput("mu")),
              #          column(3, uiOutput("sigma"))
              #        )
              # ),

# # Third column of Beta priors --------------------------------------------

#               column(2,
#                      h4("Beta prior"),
#                      numericInput("alpha", "Alpha parameter", value = 4, min = 0),
#                      numericInput("beta", "Beta parameter", value = 160, min = 0)
#               ),
#             )
#           ),
#           uiOutput('ve_probs_title'),
#           dataTableOutput("ve_probs")
#         ), 

# # Plotting Side -----------------------------------------------------------
      
#         column(6,
#                tabsetPanel(type = "tabs",
#                            tabPanel("Plot",
#                                     br(),
#                                     plotlyOutput("ve_prior"),
#                                     fluidRow(
#                                       column(6, offset = 4, 
#                                              sliderInput("ve_xaxis", "VE x-axis", value = c(-.5, 1), min = -2, max = 1, step = 0.1)
#                                       )
#                                     ),
#                                     br(),
#                                     br(),
#                                     plotlyOutput("theta_prior"),
#                                     fluidRow(
#                                       column(6, offset = 4, 
#                                              sliderInput("theta0_xaxis", "Theta0 x-axis", value = c(0, 0.05), min = 0, max = 1, step = 0.01)
#                                       )
#                                     )
#                            ),
#                            #tabPanel("Console Output"),
#                            tabPanel("Documentation")
                           
#                )
#         )
#       )

# ))