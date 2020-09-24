library(rstan)

# Check if stanmodel already exists otherwise write it to avoid compilation
if(!file.exists("simulation.rds")){
  model <- stan_model(file = "simulation.stan", auto_write = TRUE)
} else{
  model <- readRDS("simulation.rds")
}

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

vaccine_sim <- function(rratio, cases, vax_cases, N, 
                        lambda, mu, sigma, 
                        alpha, beta, ...){
  
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
  
  # In case of scalar input
  lambda <- as.array(lambda)
  mu     <- as.array(mu)
  sigma  <- as.array(sigma)
  
  data <- list(y1 = y1, y0 = y0, N0 = N0, N1 = N1, 
               K = K, lambda = lambda, mu = mu, sigma = sigma, 
               alpha = alpha, beta = beta)
  
  fit <- sampling(model, data = data, ...)
  
  return( list(fit = fit, data=data) )
}
