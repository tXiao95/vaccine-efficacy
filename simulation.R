library(ggplot2)
library(rstan)
library(rstanarm)

rstan_options(auto_write = TRUE)

# Vaccine simulation ------------------------------------------------------

vaccine_sim <- function(rratio, cases, vax_cases, N, 
                        pzero, a, b, mu, sigma, alpha, beta, ...){
  #' @param rratio randomization ratio: # vax / # total
  #' @param cases total cases at time of interim analysis
  #' @param vax_cases number of cases of total cases in vax arm
  #' @param N total number of subjects (vax and placebo) at enrollment
  
  #' @param pzero Pr(VE = 0)
  #' @param a lower bound of logRR ~ Unif(a, b)
  #' @param b upper bound of logRR ~ Unif(a, b)
  #' @param mu mean of logRR ~ N(mu, sigma)
  #' @param sigma sd of logRR ~ N(mu, sigma)
  #' @param alpha param of theta0 ~ Beta(alpha, beta)
  #' @param beta param of theta0 ~ Beta(alpha, beta)
  #' @param ... Stan fitting MCMC arguments
  
  N1 <- round(N * rratio)
  N0 <- N - N1
  plc_cases <- cases - vax_cases
  
  stopifnot(vax_cases <= cases)
  stopifnot(vax_cases <= N1)
  stopifnot(plc_cases <= N0)
  
  y1 <- rep(0, N1)
  y1[1:vax_cases] <- 1
  y0 <- rep(0, N0)
  y0[1:plc_cases] <- 1
  
  data <- list(y1 = y1, y0 = y0, N0 = N0, N1 = N1, 
               pzero = pzero, a = a, b = b, 
               mu = mu, sigma = sigma, 
               alpha = alpha, beta = beta)
  
  fit = stan(file="simulation.stan", data = data, ...) 
  
  return( list(fit = fit, data=data) )
}

sim <- vaccine_sim(rratio=2/3, 
                   cases=30, 
                   vax_cases=10, 
                   N=3000, 
                   pzero=.25, 
                   a=-.01, b=.01, 
                   mu=-.38, sigma=.325, 
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

ggplot2::ggplot(data.frame(x = c(0,1)), ggplot2::aes(x)) +
  stat_dist("prior", size = .5, fun = dbeta,
            args = list(shape1 = data$alpha, shape2 = data$beta)) + 
  geom_line(data=data.frame(x = dens_theta0$x, y = dens_theta0$y), aes(x,y, col="posterior")) + 
  theme_bw() + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  ggtitle("Theta0") + 
  ylab("Density")

# VE prior vs posterior ----------------------------------------------------

dmix2 <- function(x, pzero, a, b, mu, sigma){
  # Density of VE (transformation of logRR)
  x <- log(1-x)
  exp(x) * (pzero * dunif(x, a, b) + (1 - pzero) * dnorm(x, mu, sigma))
}

dens_ve <- density(ve)
png(filename = "ve_posterior.png", width=700, height=480)
fig <- ggplot2::ggplot(data.frame(x = c(-.5,.5)), ggplot2::aes(x)) +
  stat_dist("prior", size = .5, fun = dmix2,
            args = list(pzero=data$pzero, a=data$a, b=data$b, 
                        mu=data$mu, sigma=data$sigma)) + 
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

