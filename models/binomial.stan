data{
  int<lower=0> N; // Number of observations
  array[N] int<lower=0> site; // Site ID for each stand
  int<lower=0> n_sites; // Number of sites
  array[N] int<lower=0> fruit; // 1 = fruit present, 0 = fruit absent
  array[N] real height; // Max. height of stand (standardised)
  array[N] real cochineal; // Percentage of pads covered in cochineal (standardised)
}
parameters{
  real alpha_bar; // Global intercept
  vector[n_sites] z; // z-scores for non-centred parameterisation
  real<lower=0> sigma; // Standard deviation for varying intercepts
  real beta_height; // Effect of height on fruit
  real beta_cochineal; // Effect of cochineal on fruit
}
model{
  vector[N] theta; // Probability of success 
  
  // Priors
  alpha_bar ~ normal(0,1);
  z ~ normal(0,1);
  sigma ~ exponential(1);
  beta_height ~ normal(0,1);
  beta_cochineal ~ normal(0,1);

  // Likelihood
  for(i in 1:N){
    theta[i] = inv_logit(alpha_bar + z[site[i]]*sigma + beta_height*height[i] + beta_cochineal*cochineal[i]);
  }
  fruit ~ bernoulli(theta);
}
