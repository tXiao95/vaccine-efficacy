library(shiny)
library(LaplacesDemon)
library(ggplot2)
library(plotly)
library(purrr)

# helper functions --------------------------------------------------------

source("sim-shiny.R")

dmix <- function(x, p, mu, sigma){
    (1-x) * dnormm(log(1-x), p, mu, sigma)
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
        lambda <- map_dbl(lambda_names(), ~ default_val(input[[.x]], NA))
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
        
        dens_ve <- density(ve)
        dens_theta0 <- density(theta0)
        list(dens_ve=dens_ve, dens_theta0=dens_theta0)
    }, ignoreNULL = FALSE)


# Setting default interim analysis protocols by company -----------------------------------------------------

    observeEvent(input$protocol, {
        disable("N")
        disable("rratio")
        disable("cases")
        disable("vax_cases")
        if(input$protocol == "Moderna 1"){
            updateNumericInput(session, "N", value = 1000)
            updateNumericInput(session, "rratio", value = 0.5)
            updateNumericInput(session, "cases", value = 26)
            updateNumericInput(session, "vax_cases", value = 6)
        } else if(input$protocol == "Moderna 2"){
            updateNumericInput(session, "N", value = 2000)
            updateNumericInput(session, "rratio", value = .67)
            updateNumericInput(session, "cases", value = 30)
            updateNumericInput(session, "vax_cases", value = 10)
        } else if(input$protocol == "Pfizer 1"){
            updateNumericInput(session, "N", value = 30000)
            updateNumericInput(session, "rratio", value = .6)
            updateNumericInput(session, "cases", value = 100)
            updateNumericInput(session, "vax_cases", value = 30)
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
        x <- seq(0,1,.001)
        y <- dbeta(x, input$alpha, input$beta)
        plot <- ggplot(data.frame(x=x,y=y), aes(x,y)) + 
            geom_line(aes(col="prior")) +
            #geom_line(data=data.frame(x = dens()$dens_theta0$x, y = dens()$dens_theta0$y), aes(x,y, col="posterior"),size=1) + 
            theme_bw() + 
            geom_hline(yintercept=0) + 
            geom_vline(xintercept=0) + 
            ggtitle("Placebo incidence (theta0)") + 
            ylab("Density") + 
            xlab("theta0") + 
            xlim(input$theta0_x)
            
        ggplotly(plot)
    
    })

# Plot of VE --------------------------------------------------------------

    output$ve_prior <- renderPlotly({
        lambda <- map_dbl(lambda_names(), ~ default_val(input[[.x]], 100))
        mu     <- map_dbl(mu_names(), ~ default_val(input[[.x]], 100))
        sigma  <- map_dbl(sigma_names(), ~ default_val(input[[.x]], 100))
        
        # Validation in case mixture probabilities don't sum to 1
        req(sum(lambda) == 1 & all(lambda>0), cancelOutput = TRUE)
        
    
        x <- seq(-1,1,.001)
        y <- dmix(x, p=lambda, mu=mu, sigma=sigma)
        
        plot <- ggplot(data.frame(x=x,y=y), aes(x,y)) +
            geom_line(aes(col="prior")) + 
            #geom_line(data=data.frame(x = dens()$dens_ve$x, y = dens()$dens_ve$y), aes(x,y, col="posterior"), size=1) +
            theme_bw() +
            geom_vline(xintercept=0.5, linetype= "dashed") +
            geom_hline(yintercept=0) +
            ggtitle("Vaccine Efficacy (1-RR)") +
            ylab("Density") +
            xlab("VE") + 
            xlim(input$ve_x)
        
        
        if(input$do_sim==0){
            ggplotly(plot)
        } else{
            ggplotly(plot +
                geom_line(data=data.frame(x = dens()$dens_ve$x, 
                                          y = dens()$dens_ve$y), 
                          aes(x,y, col="posterior"), size=1)) 
                
        }
        
    })
})
