data{
  int<lower=0> N; // Number of observations
  array[N] int<lower=0> site; // Site ID for each stand
  int<lower=0> n_sites; // Number of sites
  array[N] int<lower=0> fruit; // Number of fruits
  array[N] real height; // Max. height of stand (standardised)
  array[N] real cochineal; // Percentage of pads covered in cochineal (standardised)
}
parameters{
  real alpha_bar; // Global intercept
  vector[n_sites] z; // z-scores for non-centred parameterisation
  real<lower=0> sigma; // Standard deviation for varying intercepts
  real beta_height; // Effect of height on fruit
  real beta_cochineal; // Effect of cochineal on fruit
  real<lower=0> phi; // Variance of negative binomial distribution
}
model{
  vector[N] lambda; // Rate parameter
  
  // Priors
  alpha_bar ~ normal(0,5);
  z ~ normal(0,1);
  sigma ~ exponential(1);
  beta_height ~ normal(0,1);
  beta_cochineal ~ normal(0,1);
  phi ~ exponential(1);
  
  // Likelihood
  for(i in 1:N){
    lambda[i] = exp(alpha_bar + z[site[i]]*sigma+ beta_height*height[i] + beta_cochineal*cochineal[i]);
  }
  fruit ~ neg_binomial_2(lambda, phi);
}
