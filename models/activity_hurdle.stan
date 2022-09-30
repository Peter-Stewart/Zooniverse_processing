data{
  int n_obs; // Number of observations
  array[n_obs] real y_obs; // Observed data
  array[n_obs] real x; //Covariate x
}
parameters{
  real<lower=0> sigma;
  real betax;
  real alpha;
  real omega;
  real gamma;
}
model{
  vector[n_obs] mu;
  
  sigma ~ exponential(1);
  betax ~ normal(0,1);
  alpha ~ normal(0,1);
  omega ~ normal(0,1);
  gamma ~ normal(0,1);
  
  for(i in 1:n_obs){
    mu[i] = alpha + betax*x[i];
    if (y_obs[i] == 0) {
      1 ~ bernoulli(inv_logit(omega + gamma*x[i]));
    } else {
      0 ~ bernoulli(inv_logit(omega + gamma*x[i]));
      y_obs[i] ~ normal(mu[i], sigma) T[0.00000001, ];
    }
  }
}
