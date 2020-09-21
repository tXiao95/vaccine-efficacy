functions {
  real mixK_lpdf(real x, vector lambda, vector mu, vector sigma) {
    
    int K=dims(mu)[1];
    vector[K] lps = log(lambda);
    
    for (k in 1:K){
      lps[k] += normal_lpdf(x | mu[k], sigma[k]);
    }
    return log_sum_exp(lps);
  }
}
data {
  // Actual data
  int N1;       // total num vax subjects
  int N0;       // total num placebo subjects
  int y1[N1];   // cases in vax
  int y0[N0];   // cases in placebo 
  
  // Mixture hyperparameters
  int K;                // Number of mixture components
  simplex[K] lambda;    // Mixing proportions
  vector[K] mu;
  vector<lower=0>[K]  sigma;
  
  // Theta0 Beta hyperparameters
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
  logrr ~ mixK(lambda, mu, sigma); // prior for log relative risk
  theta0 ~ beta(alpha, beta);          // prior for placebo incidence
  y1 ~ bernoulli(theta1);           // vax likelihood
  y0 ~ bernoulli(theta0);           // placebo likelihood
}
