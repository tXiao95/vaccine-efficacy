library(rstan)
library(dplyr)
library(shinystan)

launch_shinystan(fit)

# compiled_model <- stan_model(file = "simulation-subgroups.stan", auto_write = TRUE)

compiled_model <- readRDS(file = "simulation-subgroups.rds")


N1 <- 1000
N0 <- 1000
y1 <- rep(0, N1); y1[1:6] <- 1
y0 <- rep(0, N0); y0[1:26] <- 1
M <- 2
group1 <- sample(1:M, N1, replace = TRUE, prob = c(0.8, 0.2))
group0 <- sample(1:M, N0, replace = TRUE, prob = c(0.8, 0.2))
run_estimation <- 0
K_j <- rep(1, M)
lambda <- rep(1, M)
mu <- c(0, -.38)
sigma <- c(0.1, 0.325)

alpha <- c(4, 1)
beta <- c(160, 1)

data <- list(N1 = N1, 
             N0 = N0, 
             y1 = y1, 
             y0 = y0, 
             M = M,
             group1 = group1,
             group0 = group0, 
             run_estimation = run_estimation, 
             K_j = K_j, 
             lambda = lambda, 
             mu = mu, 
             sigma = sigma, 
             alpha = alpha, 
             beta = beta)

fit <- sampling(compiled_model, data = data)


fake_data_matrix <- fit %>% 
  as.data.frame() %>%
  select(contains("y1_sim"))

y1 <- fake_data_matrix[2000,]
y0 <- fit %>% 
  as.data.frame() %>%
  select(contains("y1_sim"))
y0 <- y0[2000,]

data$y1 <- as.numeric(y1)
data$y0 <- as.numeric(y0)
data$run_estimation <- 0

fit2 <- sampling(compiled_model, data)

samples <- extract(fit2)

logrr <- samples$logrr
ve <- samples$ve
mean(samples$theta0[,1])

summary_tbl <- apply(fake_data_matrix[1:5,], 1, summary)
