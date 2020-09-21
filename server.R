library(shiny)
library(LaplacesDemon)
library(ggplot2)
library(purrr)

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

shinyServer(function(input, output) {
    
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
    
    output$theta_prior <- renderPlot({
        ggplot(data.frame(x = c(0,1)), ggplot2::aes(x)) +
            stat_dist("prior", size = 1, fun = dbeta,
                      args = list(shape1 = input$alpha, shape2 = input$beta)) + 
            #geom_line(data=data.frame(x = dens_theta0$x, y = dens_theta0$y), aes(x,y, col="posterior")) + 
            theme_bw() + 
            geom_hline(yintercept=0) + 
            geom_vline(xintercept=0) + 
            ggtitle("Placebo incidence") + 
            ylab("Density") + 
            xlab("theta0")
    
    }, res = 96)
    
    output$ve_prior <- renderPlot({
        lambda <- map_dbl(lambda_names(), ~ default_val(input[[.x]], NA))
        mu <- map_dbl(mu_names(), ~ default_val(input[[.x]], NA))
        sigma <- map_dbl(sigma_names(), ~ default_val(input[[.x]], NA))
        ggplot(data.frame(x = c(-.5,.5)), ggplot2::aes(x)) + 
            stat_dist("prior", size = 1, fun = dmix,
                      args = list(p=lambda, mu = mu, sigma = sigma)) + 
            geom_line(data=data.frame(x = dens_ve$x, y = dens_ve$y), aes(x,y, col="posterior")) + 
            theme_bw() + 
            geom_vline(xintercept=0.5, linetype= "dashed") + 
            geom_hline(yintercept=0) + 
            ggtitle("Vaccine Efficacy") + 
            ylab("Density") + 
            xlab("VE")
    }, res = 96)
})
