data{
  int<lower=0> N; // Number of observations
  array[N] int<lower=0> site; // Site ID for each stand
  int<lower=0> n_sites; // Number of sites
  array[N] int<lower=0> fruit; // 1 = fruit present, 0 = fruit absent
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
  real beta_cochineal_height; // Effect of cochineal on height
  vector[2] alpha_tree_height; // Parameters for tree absence/presence in height model
  vector[2] alpha_grass_height; // Parameters for grass absence/presence in height model
  vector[2] alpha_forb_height; // Parameters for forb absence/presence in height model
  vector[2] alpha_shrub_height; // Parameters for shrub absence/presence in height model
  real<lower=0> sigma_height; 
  
  // Fruit model
  real alpha_fruit_bar; // Global intercept
  vector[n_sites] z; // z-scores for non-centred parameterisation
  real<lower=0> sigma_fruit; // Standard deviation for varying intercepts
  real beta_height_fruit; // Effect of height on fruit
  real beta_cochineal_fruit; // Effect of cochineal on fruit
  vector[2] alpha_tree_fruit; // Parameters for tree absence/presence in fruit model
  vector[2] alpha_grass_fruit; // Parameters for grass absence/presence in fruit model
  vector[2] alpha_forb_fruit; // Parameters for forb absence/presence in fruit model
  vector[2] alpha_shrub_fruit; // Parameters for shrub absence/presence in fruit model

}
model{
  vector[N] mu; // Mean for height model
  vector[N] theta; // Probability of success for fruit model
  
  // Priors
  alpha_height ~ normal(0,1);
  beta_cochineal_height ~ normal(0,1);
  alpha_tree_height ~ normal(0,1);
  alpha_grass_height ~ normal(0,1);
  alpha_forb_height ~ normal(0,1);
  alpha_shrub_height ~ normal(0,1);
  sigma_height ~ exponential(1);
  
  alpha_fruit_bar ~ normal(0,1);
  z ~ normal(0,1);
  sigma_fruit ~ exponential(1);
  beta_height_fruit ~ normal(0,1);
  beta_cochineal_fruit ~ normal(0,1);
  alpha_tree_fruit ~ normal(0,1);
  alpha_grass_fruit ~ normal(0,1);
  alpha_forb_fruit ~ normal(0,1);
  alpha_shrub_fruit ~ normal(0,1);

  // Height model
  for(i in 1:N){
    mu[i] = alpha_height + 
    alpha_tree_height[tree[i]] + 
    alpha_grass_height[grass[i]] +
    alpha_forb_height[forb[i]] +
    alpha_shrub_height[shrub[i]] +
    beta_cochineal_height*cochineal[i];
  }
  height ~ normal(mu, sigma_height);

  // Fruit model
  for(i in 1:N){
    theta[i] = inv_logit(alpha_fruit_bar + z[site[i]]*sigma_fruit + 
    beta_height_fruit*height[i] + 
    beta_cochineal_fruit*cochineal[i] +
    alpha_tree_fruit[tree[i]] +
    alpha_grass_fruit[grass[i]] +
    alpha_forb_fruit[forb[i]] +
    alpha_shrub_fruit[shrub[i]]);
  }
  fruit ~ bernoulli(theta);
}
