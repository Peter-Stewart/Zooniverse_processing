data{
  int<lower=0> N; // Number of observations
  array[N] int<lower=0> site; // Site ID for each stand
  int<lower=0> n_sites; // Number of sites
  array[N] int<lower=0> fruit; // Number of fruits
  array[N] real height; // Max. height of stand (standardised)
  array[N] real cochineal; // Percentage of pads covered in cochineal (standardised)
  array[N] int tree; // 1 = not under tree, 2 = under tree
  array[N] int grass; // 1 = absent, 2 = present
  array[N] int forb; // 1 = absent, 2 = present
  array[N] int shrub; // 1 = absent, 2 = present
}
parameters{
  // Height model
  real alpha_height; // Intercept for height model
  real beta_G_height; // Effect of G on height
  real beta_cochineal_height; // Effect of cochineal on height
  vector[2] alpha_tree_height; // Parameters for tree absence/presence in height model
  vector[2] alpha_grass_height; // Parameters for grass absence/presence in height model
  vector[2] alpha_forb_height; // Parameters for forb absence/presence in height model
  vector[2] alpha_shrub_height; // Parameters for shrub absence/presence in height model
  vector[n_sites] G; // Latent group-level confound
  real<lower=0> sigma; 
  
  // Non-centred parameterisation for fruit model
  vector[n_sites] z;
  real<lower=0> tau;

  // Fruit model
  real alpha_fruit_bar;
  real beta_height_fruit; // Effect of height on fruit
  real beta_cochineal_fruit; // Effect of cochineal on fruit
  vector[2] alpha_tree_fruit; // Parameters for tree absence/presence in fruit model
  vector[2] alpha_grass_fruit; // Parameters for grass absence/presence in fruit model
  vector[2] alpha_forb_fruit; // Parameters for forb absence/presence in fruit model
  vector[2] alpha_shrub_fruit; // Parameters for shrub absence/presence in fruit model
  real beta_G_fruit; // Effect of G on fruit
  real<lower=0> phi; // Variance of negative binomial distribution
}
transformed parameters{
  vector[n_sites] alpha_fruit;
  alpha_fruit = alpha_fruit_bar + z * tau;
}
model{
  vector[N] mu; // Mean for height model
  vector[N] lambda; // Rate parameter for fruit model
  
  // Priors
  alpha_height ~ normal(0,1);
  beta_G_height ~ normal(0,1);
  beta_cochineal_height ~ normal(0,1);
  alpha_tree_height ~ normal(0,1);
  alpha_grass_height ~ normal(0,1);
  alpha_forb_height ~ normal(0,1);
  alpha_shrub_height ~ normal(0,1);
  G ~ normal(0,1); // Group-level confound to be modelled
  sigma ~ exponential(1);
  
  alpha_fruit_bar ~ normal(0,5);
  z ~ normal(0,1);
  tau ~ exponential(1);
  
  beta_height_fruit ~ normal(0,1);
  beta_cochineal_fruit ~ normal(0,1);
  alpha_tree_fruit ~ normal(0,1);
  alpha_grass_fruit ~ normal(0,1);
  alpha_forb_fruit ~ normal(0,1);
  alpha_shrub_fruit ~ normal(0,1);
  beta_G_fruit ~ normal(0,1);
  phi ~ exponential(1);
  
  // Height model
  for(i in 1:N){
    mu[i] = alpha_height + 
    alpha_tree_height[tree[i]] + 
    alpha_grass_height[grass[i]] +
    alpha_forb_height[forb[i]] +
    alpha_shrub_height[shrub[i]] +
    beta_G_height*G[site[i]] + 
    beta_cochineal_height*cochineal[i];
  }
  height ~ normal(mu, sigma);
  
  // Fruit model
  for(i in 1:N){
    lambda[i] = exp(alpha_fruit[site[i]] + 
    beta_height_fruit*height[i] + 
    beta_cochineal_fruit*cochineal[i] +
    alpha_tree_fruit[tree[i]] +
    alpha_grass_fruit[grass[i]] +
    alpha_forb_fruit[forb[i]] +
    alpha_shrub_fruit[shrub[i]] +
    beta_G_fruit*G[site[i]]);
  }
  fruit ~ neg_binomial_2(lambda, phi);
}
