// Modified to allow for subgroups. Independent and non-identical Bernoullis

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
  
  int<lower=0, upper=1> y1[N1];   // cases in vax
  int<lower=0, upper=1> y0[N0];   // cases in placebo 
  
  int group1[N1]; // group identifier in vax
  int group0[N0]; // group identifier in placebo
  int M; // number of unique groups
  
  int<lower=0, upper=1> run_estimation; // Whether to evaluate likelihood
  
  // Mixture hyperparameters
  int K_j[M];                // Number of mixture components for each group j
  
  // Long vector of all mixture Gaussian parameters
  vector[sum(K_j)] lambda;
  vector[sum(K_j)] mu;
  vector<lower=0>[sum(K_j)]  sigma;
  
  // Theta0 Beta hyperparameters. One set for each group
  real<lower=0> alpha[M];
  real<lower=0> beta[M];
}
parameters {
  vector<lower=0,upper=1>[M] theta0; // risk in each group
  vector[M] logrr;                   // logRR of each group
}
transformed parameters { 
  vector[M] ve = 1 - exp(logrr);       // vaccine efficacy
  vector<lower=0, upper=1>[M] theta1 = (1 - ve) .* theta0;  // prob of case in vax arm
}
model {
  int start;
  int end;
  start = 1;
  for(i in 1:M){
    end = start + K_j[i] - 1;
    logrr[i] ~ mixK(lambda[start:end], mu[start:end], sigma[start:end]);
    start = end + 1;
  }
  
  // prior for placebo incidence
  for(i in 1:M){
    theta0[i] ~ beta(alpha[i], beta[i]);          
  }
  
  if(run_estimation == 1){
      // vax likelihood
    for(i in 1:N1){
      y1[i] ~ bernoulli(theta1[group1[i]]);      
    }
    
    // placebo likelihood
    for(i in 1:N0){
      y0[i] ~ bernoulli(theta0[group0[i]]);
    }
  }
  
}
generated quantities{
  int y1_sim[N1];
  int y0_sim[N0];
  
  for(i in 1:N1){
    y1_sim[i] = bernoulli_rng(theta1[group1[i]]);
  }
  for(i in 1:N0){
    y0_sim[i] = bernoulli_rng(theta0[group0[i]]);
  }
}
