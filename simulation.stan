functions {
  // Mixture model
  // Zero point mass (Unif(a,b)) and nonzero mass N(mu, sigma)
  real mix2_lpdf(real x, real pzero, real a, real b, real mu, real sigma) {
        return log_sum_exp(log(pzero) + uniform_lpdf(x | a, b), 
                           log1m(pzero) + normal_lpdf(x | mu, sigma));
  }
}
data {
  // Actual data
  int N1;       // total num vax subjects
  int N0;       // total num placebo subjects
  int y1[N1];   // cases in vax
  int y0[N0];   // cases in placebo 
  
  // Hyperparameters
  real<lower=0,upper=1> pzero;
  real a;
  real b;
  real mu;
  real<lower=0>  sigma;
  real<lower=0> alpha;
  real<lower=0> beta;
}
parameters {
  real<lower=0,upper=1> theta0; // prob of case in placebo arm
  real logrr;                   // log relative risk vax to placebo
}
transformed parameters { 
  real ve = 1 - exp(logrr);       // vaccine efficacy
  real theta1 = (1 - ve)*theta0;  // prob of case in vax arm
}
model {
  logrr ~ mix2(pzero, a, b, mu, sigma); // prior for log relative risk
  theta0 ~ beta(alpha, beta);          // prior for placebo incidence
  y1 ~ bernoulli(theta1);           // vax likelihood
  y0 ~ bernoulli(theta0);           // placebo likelihood
}
