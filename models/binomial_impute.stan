data{
  int<lower=0> N; // Number of observations
  array[N] int<lower=0> site; // Site ID for each stand
  int<lower=0> n_sites; // Number of sites
  array[N] int<lower=0> fruit; // 1 = fruit present, 0 = fruit absent
  array[N] real height; // Max. height of stand (standardised)
  array[N] real cochineal; // Percentage of pads covered in cochineal (standardised)
}
parameters{
  // Height model
  real alpha_height; // Intercept for height model
  real beta_G_height; // Effect of G on height
  real beta_cochineal_height; // Effect of cochineal on height
  vector[n_sites] G; // Latent group-level confound
  real<lower=0> sigma_height; 
  
  // Fruit model
  real alpha_fruit_bar; // Global intercept
  vector[n_sites] z; // z-scores for non-centred parameterisation
  real<lower=0> sigma_fruit; // Standard deviation for varying intercepts
  real beta_height_fruit; // Effect of height on fruit
  real beta_cochineal_fruit; // Effect of cochineal on fruit
  real beta_G_fruit; // Effect of G on fruit

}
model{
  vector[N] mu; // Mean for height model
  vector[N] theta; // Probability of success for fruit model
  
  // Priors
  alpha_height ~ normal(0,1);
  beta_G_height ~ normal(0,1);
  beta_cochineal_height ~ normal(0,1);
  G ~ normal(0,1); // Group-level confound to be modelled
  sigma_height ~ exponential(1);
  
  alpha_fruit_bar ~ normal(0,1);
  z ~ normal(0,1);
  sigma_fruit ~ exponential(1);
  beta_height_fruit ~ normal(0,1);
  beta_cochineal_fruit ~ normal(0,1);
  beta_G_fruit ~ normal(0,1);

  // Height model
  for(i in 1:N){
    mu[i] = alpha_height + beta_G_height*G[site[i]] + beta_cochineal_height*cochineal[i];
  }
  height ~ normal(mu, sigma_height);

  // Fruit model
  for(i in 1:N){
    theta[i] = inv_logit(alpha_fruit_bar + z[site[i]]*sigma_fruit + 
    beta_height_fruit*height[i] + 
    beta_cochineal_fruit*cochineal[i] +
    beta_G_fruit*G[site[i]]);
  }
  fruit ~ bernoulli(theta);
}
