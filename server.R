library(shiny)
library(LaplacesDemon)
library(ggplot2)
library(purrr)

# helper functions --------------------------------------------------------

source("sim-shiny.R")

stat_dist <- function(dist, ...) {
    ggplot2::stat_function(ggplot2::aes_(color = dist), ...)
}

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

shinyServer(function(input, output) {

# Handling arbitrary number of K-dim Gaussian mixture ---------------------

    lambda_names <- reactive(paste0("p", seq_len(input$n)))
    mu_names     <- reactive(paste0("mu", seq_len(input$n)))
    sigma_names  <- reactive(paste0("sigma", seq_len(input$n)))
    
    output$lambda <- renderUI({
        map(lambda_names(), ~ numericInput(.x, .x, value = isolate(input[[.x]]), min=0, max=1 ) %||% "")
    })
    
    output$mu <- renderUI({
        map(mu_names(), ~ numericInput(.x, .x, value = isolate(input[[.x]])) %||% "")
    })
    
    output$sigma <- renderUI({
        map(sigma_names(), ~ numericInput(.x, .x, value = isolate(input[[.x]]), min = 0) %||% "")
    })
    

# Actual stan model run ---------------------------------------------------

    dens <- eventReactive(input$do_sim, {
        lambda <- map_dbl(lambda_names(), ~ default_val(input[[.x]], NA))
        mu     <- map_dbl(mu_names(), ~ default_val(input[[.x]], NA))
        sigma  <- map_dbl(sigma_names(), ~ default_val(input[[.x]], NA))
        sim    <- vaccine_sim(rratio=input$rratio, 
                               cases=input$cases, 
                               vax_cases=input$vax_cases, 
                               N=input$N, 
                               lambda=lambda, 
                               mu=mu, 
                               sigma=sigma, 
                               alpha=input$alpha, beta=input$beta, iter=1000)
        
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

# Reset button ------------------------------------------------------------

    observeEvent(input$reset, {
        reset("N")
        reset("rratio")
        reset("cases")
        reset("vax_cases")
        reset("alpha")
        reset("beta")
        reset("n")
    })

# Plot of theta0 ----------------------------------------------------------

    output$theta_prior <- renderPlot({
        ggplot(data.frame(x = c(0,1)), ggplot2::aes(x)) +
            stat_dist("prior", size = 1, fun = dbeta,
                      args = list(shape1 = input$alpha, shape2 = input$beta)) + 
            geom_line(data=data.frame(x = dens()$dens_theta0$x, y = dens()$dens_theta0$y), aes(x,y, col="posterior")) + 
            theme_bw() + 
            geom_hline(yintercept=0) + 
            geom_vline(xintercept=0) + 
            ggtitle("Placebo incidence") + 
            ylab("Density") + 
            xlab("theta0")
    
    }, res = 96)

# Plot of VE --------------------------------------------------------------

    output$ve_prior <- renderPlot({
        lambda <- map_dbl(lambda_names(), ~ default_val(input[[.x]], NA))
        mu <- map_dbl(mu_names(), ~ default_val(input[[.x]], NA))
        sigma <- map_dbl(sigma_names(), ~ default_val(input[[.x]], NA))
        ggplot(data.frame(x = c(-.5,.5)), ggplot2::aes(x)) + 
            stat_dist("prior", size = 1, fun = dmix,
                      args = list(p=lambda, mu = mu, sigma = sigma)) + 
            geom_line(data=data.frame(x = dens()$dens_ve$x, y = dens()$dens_ve$y), aes(x,y, col="posterior")) + 
            theme_bw() + 
            geom_vline(xintercept=0.5, linetype= "dashed") + 
            geom_hline(yintercept=0) + 
            ggtitle("Vaccine Efficacy") + 
            ylab("Density") + 
            xlab("VE")
    }, res = 96)
})
