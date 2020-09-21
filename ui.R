library(shiny)

shinyUI(fluidPage(
  
  # Application Title
  titlePanel("Bayesian approach to Vaccine Efficacy"),
  
      fluidRow(
          column(2,
                 #h4("Data parameters"),
                 numericInput("N", "Total subjects at enrollment", value = 3000, min = 0, step = 1),
                 numericInput("rratio", "Randomization ratio", value = 0.67, min = 0, max = 1),
                 numericInput("cases", "Total cases", value = 30, min = 0, step = 1),
                 numericInput("vax_cases", "Vaccine cases", value = 10, min = 0, step = 1)
          ),
          column(1,
                 #h4("Beta prior on placebo incidence"),
                 numericInput("alpha", "Alpha parameter", value = .15, min = 0),
                 numericInput("beta", "Beta parameter", value = 10, min = 0)
          ),
          column(3,
                 #h4("Gaussian mixture prior on log(RR)"),
                 numericInput('n', 'Number of mixture components', 
                              value = 0, 
                              min = 0, 
                              step = 1),
                 fluidRow(
                   column(3, uiOutput("lambda")),
                   column(3, uiOutput("mu")),
                   column(3, uiOutput("sigma"))
                 )
                 
          ),
          column(6,
            plotOutput("theta_prior"),
            plotOutput("ve_prior")
          )
      )
))