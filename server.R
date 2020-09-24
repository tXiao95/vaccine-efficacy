library(shiny)
library(LaplacesDemon)
library(plotly)
library(purrr)

# helper functions --------------------------------------------------------

source("sim-shiny.R")

dmix <- function(x, p, mu, sigma){
    # Transform from density of logRR
    dnormm(log(1-x), p, mu, sigma) / (1 - x)
}

pmix <- function(x, p, mu, sigma){
    # Transform from CDF of logRR
    1 - pnormm(log(1-x), p, mu, sigma)
}

default_val <- function(x, value) {
    if (isTruthy(x)) {
        x
    } else {
        value
    } 
}

# Server Side -------------------------------------------------------------

shinyServer(function(input, output, session) {

    lambda_names <- reactive(paste0("p", seq_len(input$n)))
    mu_names     <- reactive(paste0("mu", seq_len(input$n)))
    sigma_names  <- reactive(paste0("sigma", seq_len(input$n)))
    
# Actual stan model run ---------------------------------------------------

    dens <- eventReactive(input$do_sim, {
        lambda <- map_dbl(lambda_names(), ~ default_val(input[[.x]], 100))
        mu     <- map_dbl(mu_names(), ~ default_val(input[[.x]], NA))
        sigma  <- map_dbl(sigma_names(), ~ default_val(input[[.x]], NA))
        withProgress(message="Running STAN model: compilation make take some time..", {
            sim    <- vaccine_sim(rratio=input$rratio, 
                                  cases=input$cases, 
                                  vax_cases=input$vax_cases, 
                                  N=input$N, 
                                  lambda=lambda, 
                                  mu=mu, 
                                  sigma=sigma, 
                                  alpha=input$alpha, beta=input$beta, iter=3000)
        })
        
        # Rhats should be close to 1
        fit <- sim$fit
        data <- sim$data
        
        samples <- extract(fit)
        ve <- samples$ve
        theta0 <- samples$theta0
        
        list(theta0=theta0, ve = ve, fit=fit)
    }, ignoreNULL = FALSE)
    


# Setting default interim analysis protocols by company -----------------------------------------------------

    observeEvent(input$protocol, {
        disable("N")
        disable("rratio")
        disable("cases")
        disable("vax_cases")
        if(input$protocol == "Pfizer Interim 1"){
            updateNumericInput(session, "N", value = 3000)
            updateNumericInput(session, "rratio", value = 0.5)
            updateNumericInput(session, "cases", value = 32)
            updateNumericInput(session, "vax_cases", value = 6)
        } else if(input$protocol == "Pfizer Interim 2"){
            updateNumericInput(session, "N", value = 3000)
            updateNumericInput(session, "rratio", value = .5)
            updateNumericInput(session, "cases", value = 62)
            updateNumericInput(session, "vax_cases", value = 15)
        } else if(input$protocol == "Pfizer Interim 3"){
            updateNumericInput(session, "N", value = 3000)
            updateNumericInput(session, "rratio", value = .5)
            updateNumericInput(session, "cases", value = 92)
            updateNumericInput(session, "vax_cases", value = 25)
        
        } else if(input$protocol == "Pfizer Interim 4"){
            updateNumericInput(session, "N", value = 3000)
            updateNumericInput(session, "rratio", value = .5)
            updateNumericInput(session, "cases", value = 120)
            updateNumericInput(session, "vax_cases", value = 35)
        } else if(input$protocol == "Pfizer Final"){
            updateNumericInput(session, "N", value = 3000)
            updateNumericInput(session, "rratio", value = .5)
            updateNumericInput(session, "cases", value = 169)
            updateNumericInput(session, "vax_cases", value = 53)
        } else if(input$protocol == "Custom"){
            enable("N")
            enable("rratio")
            enable("cases")
            enable("vax_cases")
            updateNumericInput(session, "N", value = NULL)
            updateNumericInput(session, "rratio", value = NULL)
            updateNumericInput(session, "cases", value = NULL)
            updateNumericInput(session, "vax_cases", value = NULL)
        }
    }, ignoreInit = FALSE)

# Setting default priors by type ------------------------------------------

    observeEvent(input$prior, {
        disable("n")
        if(input$prior == "Very Skeptical"){
            updateNumericInput(session, "n", value = 2)
            
            output$lambda <- renderUI(list(disabled(numericInput("p1", "p1", value = .8, min = 0, max = 1, step=.01)),
                                           disabled(numericInput("p2", "p2", value = .2, min = 0, max = 1, step=.01))))
            
            output$mu <- renderUI(list(disabled(numericInput("mu1", "mu1", value = 0, step=.01)),
                                       disabled(numericInput("mu2", "mu2", value = -.38, step=.01))))
            
            output$sigma <- renderUI(list(disabled(numericInput("sigma1", "sigma1", value = 0.1, min=0, step=.01)),
                                          disabled(numericInput("sigma2", "sigma2", value = .325, min = 0, step=.01))))
            
        } else if(input$prior == "Skeptical"){
            updateNumericInput(session, "n", value = 2)
            output$lambda <- renderUI(list(disabled(numericInput("p1", "p1", value = .4, min = 0, max = 1)),
                                           disabled(numericInput("p2", "p2", value = .6, min = 0, max = 1))))
            
            output$mu     <- renderUI(list(disabled(numericInput("mu1", "mu1", value = 0)),
                                           disabled(numericInput("mu2", "mu2", value = -.38))))
            
            output$sigma  <- renderUI(list(disabled(numericInput("sigma1", "sigma1", value = 0.325, min=0)), 
                                           disabled(numericInput("sigma2", "sigma2", value = 0.325, min=0))))
        } else if(input$prior == "Cautiously Optimistic"){
            updateNumericInput(session, "n", value = 1)
            output$lambda <- renderUI(list(disabled(numericInput("p1", "p1", value = 1, min = 0, max = 1))))
            output$mu     <- renderUI(list(disabled(numericInput("mu1", "mu1", value = -.5))))
            output$sigma  <- renderUI(list(disabled(numericInput("sigma1", "sigma1", value = 0.325, min=0))))
            
        } else if(input$prior == "Optimistic"){
            updateNumericInput(session, "n", value = 1)
            output$lambda <- renderUI(list(disabled(numericInput("p1", "p1", value = 1, min = 0, max = 1))))
            output$mu     <- renderUI(list(disabled(numericInput("mu1", "mu1", value = -.7))))
            output$sigma  <- renderUI(list(disabled(numericInput("sigma1", "sigma1", value = 0.1, min=0))))
        } else if(input$prior == "Very Optimistic"){
            updateNumericInput(session, "n", value = 1)
            output$lambda <- renderUI(list(disabled(numericInput("p1", "p1", value = 1, min = 0, max = 1))))
            output$mu     <- renderUI(list(disabled(numericInput("mu1", "mu1", value = -1.2))))
            output$sigma  <- renderUI(list(disabled(numericInput("sigma1", "sigma1", value = 0.1, min=0))))
        } else if(input$prior == "Custom"){
            enable("n")
            output$lambda <- renderUI({
                map(lambda_names(), ~ numericInput(.x, .x, value = isolate(input[[.x]]), min=0, max=1 ,step=.01) %||% 0)
            })
            
            output$mu <- renderUI({
                map(mu_names(), ~ numericInput(.x, .x, value = isolate(input[[.x]]), step=.01) %||% 0)
            })
            
            output$sigma <- renderUI({
                map(sigma_names(), ~ numericInput(.x, .x, value = isolate(input[[.x]]), min = 0, step=.01) %||% 0)
            })
        } 
    }, ignoreInit = FALSE)
    
# Reset button ------------------------------------------------------------

    observeEvent(input$reset, {
        reset("protocol")
        reset("prior")
        reset("alpha")
        reset("beta")
        reset("n")
        reset("theta0_x")
        reset("ve_x")
    })
    
# Plot of theta0 ----------------------------------------------------------

    output$theta_prior <- renderPlotly({
        x <- round(seq(0,1,.001), 3)
        y <- dbeta(x, input$alpha, input$beta)
        p_theta <- round(1-pbeta(x, input$alpha, input$beta), 2)
        
        fig <- plot_ly()
        
        fig <- fig %>%
            add_trace(
                mode = 'lines', 
                x = x, 
                y = y,
                hoverinfo = 'skip',
                fill = 'tozeroy',
                showlegend = FALSE
            ) %>%
            
            layout(title = 'Prior on Placebo Incidence')
        
        fig
    })

# Data table of posterior probs --------------------------------------------------------------
    
    probs <- eventReactive(input$do_sim, {
        ve <- dens()$ve
        df <- data.frame(x = c(0.30, 0.50, 0.70, 0.90))
        df$`P(VE > x | data)` <- round(sapply(df$x, function(x) mean(ve > x)), 2)
        df
    })
    
    probs_title <- eventReactive(input$do_sim, "Posterior Probabilities")
    
    output$ve_probs_title <- renderUI(titlePanel(probs_title()))
    
    output$ve_probs <- renderDataTable({
        probs()
    })

# Prior and Posterior densitiy graph --------------------------------------

    # CLearing posterior graph
    v <- reactiveValues(clear_posterior=TRUE)
    observeEvent(input$clear_posterior, v$clear_posterior <- TRUE)
    observeEvent(input$do_sim, v$clear_posterior <- FALSE)
    
    output$ve_prior <- renderPlotly({
        lambda <- map_dbl(lambda_names(), ~ default_val(input[[.x]], 100))
        mu     <- map_dbl(mu_names(), ~ default_val(input[[.x]], NA))
        sigma  <- map_dbl(sigma_names(), ~ default_val(input[[.x]], NA))
        
        # Validation in case mixture probabilities don't sum to 1
        validate(
            need(sum(lambda) == 1, 'Mixing probabilities p must sum to 1'),
            need(all(lambda > 0 & lambda <= 1), 'Mixing probabilities p must be in (0, 1] interval'),
            need(all(sigma > 0), 'All sigmas must be positive'), 
            need(all(!is.na(mu) & !is.na(sigma) & !is.na(lambda)), 'All prior fields must be filled out')
        )
        
        # Standardize plotting of prior and posterior densities
        xmin <- -2
        xmax <- 1
        step <- .001
        xaxis <- seq(xmin, xmax, step)
        num_x <- length(xaxis)
        
        
        prior_x <- round(xaxis, 3)
        prior_y <- dmix(prior_x, p=lambda, mu=mu, sigma=sigma)
        prior_p <- round(1 - pmix(prior_x, lambda, mu, sigma), 2)
        
        fig <- plot_ly()
        fig <- fig %>%
            add_trace(
                mode = 'lines', 
                x = prior_x, 
                y = prior_y,
                name = 'Prior',
                fill = 'tozeroy',
                hovertemplate = paste0('P(VE > ', prior_x, ') = ', prior_p)
            ) %>%
            
            layout(title = 'Vaccine Efficacy', spikedistance = -1,
                   xaxis=list(title='VE', showspikes = TRUE, spikedash = 'solid', spikesnap = 'cursor',
                              spikesides = FALSE, spikethickness = 3, showlegend = TRUE, spikemode = 'across'), 
                   yaxis=list(title='Density'), hovermode = 'x')
        
        # If on startup or clearing posterior, only plot prior. Otherwise plot both
        if(v$clear_posterior ){
            fig
        } else{
            ve <- dens()$ve
            density_ve <- density(ve, from = xmin, to = xmax, n = num_x)
            p_ve <- round(sapply(density_ve$x, function(x) mean(ve > x)), 2)
            
            ve_x <- round(density_ve$x, 3)
            ve_y <- density_ve$y
            
            fig %>%
                add_trace(
                    mode = 'lines',
                    x = ve_x,
                    y = ve_y,
                    name = 'Posterior',
                    fill = 'tozeroy',
                    hovertemplate = paste0('P(VE > ', ve_x, ')= ', p_ve),
                    showlegend = TRUE
                )
        }
    })
})
