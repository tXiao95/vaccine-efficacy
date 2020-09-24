library(ggplot2)
library(LaplacesDemon)
library(rstan)
library(rstanarm)

rstan_options(auto_write = TRUE)

# Vaccine simulation ------------------------------------------------------

vaccine_sim <- function(rratio, cases, vax_cases, N, 
                        lambda, mu, sigma, 
                        alpha, beta, ...){
  #' @param rratio randomization ratio: # vax / # total
  #' @param cases total cases at time of interim analysis
  #' @param vax_cases number of cases of total cases in vax arm
  #' @param N total number of subjects (vax and placebo) at enrollment
  
  #' @param lambda vector of mixing proportions
  #' @param mu vector mean of logRR ~ N(mu_i, sigma_i)
  #' @param sigma vector sd of logRR ~ N(mu_i, sigma_i)

  #' @param alpha param of theta0 ~ Beta(alpha, beta)
  #' @param beta param of theta0 ~ Beta(alpha, beta)
  #' @param ... Stan fitting MCMC arguments
  
  N1 <- round(N * rratio)
  N0 <- N - N1
  plc_cases <- cases - vax_cases
  
  stopifnot(vax_cases <= cases)
  stopifnot(vax_cases <= N1)
  stopifnot(plc_cases <= N0)
  stopifnot(length(lambda) == length(mu))
  stopifnot(length(mu) == length(sigma))
  
  y1 <- rep(0, N1)
  y1[1:vax_cases] <- 1
  y0 <- rep(0, N0)
  y0[1:plc_cases] <- 1
  
  K <- length(mu)
  
  # Iin case of scalar input
  lambda <- as.array(lambda)
  mu <- as.array(mu)
  sigma <- as.array(sigma)
  
  data <- list(y1 = y1, y0 = y0, N0 = N0, N1 = N1, 
               K = K, lambda = lambda, mu = mu, sigma = sigma, 
               alpha = alpha, beta = beta)
  
  stanfit <- readRDS('simulation.rds')
  fit = stan(file="simulation.stan", fit=stanfit, data = data, ...) 
  
  return( list(fit = fit, data=data) )
}

sim <- vaccine_sim(rratio=2/3, 
                   cases=30, 
                   vax_cases=10, 
                   N=3000, 
                   lambda=c(1), 
                   mu=c(-.38), 
                   sigma=c(.01), 
                   alpha=.15, beta=10, iter=1000)


# Rhats should be close to 1
fit <- sim$fit
data <- sim$data

samples <- extract(fit)
ve <- samples$ve
theta0 <- samples$theta0



# theta0 prior vs posterior -----------------------------------------------------------

stat_dist <- function(dist, ...) {
  ggplot2::stat_function(ggplot2::aes_(color = dist), ...)
}

dens_theta0 <- density(theta0)

ggplot2::ggplot(data.frame(x = c(0,.3)), ggplot2::aes(x)) +
  stat_dist("prior", size = .5, fun = dbeta,
            args = list(shape1 = data$alpha, shape2 = data$beta)) + 
  geom_line(data=data.frame(x = dens_theta0$x, y = dens_theta0$y), aes(x,y, col="posterior")) + 
  theme_bw() + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  ggtitle("Theta0") + 
  ylab("Density")

# VE prior vs posterior ----------------------------------------------------
dmix <- function(x, p, mu, sigma){
  x <- log(1-x)
  exp(x) * dnormm(x, p, mu, sigma)
}

dens_ve <- density(ve)
png(filename = "ve_posterior.png", width=700, height=480)
fig <- ggplot(data.frame(x = c(-.5,.5)), ggplot2::aes(x)) + 
  stat_dist("prior", size = .5, fun = dmix,
            args = list(p=data$lambda, mu = data$mu, sigma = data$sigma)) + 
  geom_line(data=data.frame(x = dens_ve$x, y = dens_ve$y), aes(x,y, col="posterior")) + 
  theme_bw() + 
  geom_vline(xintercept=0.5, linetype= "dashed") + 
  geom_hline(yintercept=0) + 
  ggtitle("Vaccine Efficacy") + 
  ylab("Density")
print(fig)
dev.off()


# Pr(VE > 0.5 | data)
mean(ve > 0.5)


# Uncomment below for diagnostics of fit on Shiny

# rstanarm::launch_shinystan(fit)

