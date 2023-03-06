data{
  int<lower=0> N; // Number of observations
  array[N] int<lower=0> site; // Site ID for each stand
  int<lower=0> n_sites; // Number of sites
  array[N] int<lower=0> fruit; // Number of fruits
  array[N] real height; // Max. height of stand (cm)
  array[N] real cochineal; // Percentage of pads covered in cochineal
}
parameters{
  // Height model
  real alpha_height; // Intercept for height model
  real beta_G_height; // Effect of G on height
  real beta_cochineal_height; // Effect of cochineal on height
  vector[n_sites] G; // Latent group-level confound
  real<lower=0> sigma; 
  
  // Non-centred parameterisation for fruit model
  vector[n_sites] z;
  real<lower=0> tau;

  // Fruit model
  real alpha_fruit_bar;
  real beta_height_fruit; // Effect of height on fruit
  real beta_cochineal_fruit; // Effect of cochineal on fruit
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
  G ~ normal(0,1); // Group-level confound to be modelled
  sigma ~ exponential(1);
  
  alpha_fruit_bar ~ normal(0,5);
  z ~ normal(0,1);
  tau ~ exponential(1);
  
  beta_height_fruit ~ normal(0,1);
  beta_cochineal_fruit ~ normal(0,1);
  beta_G_fruit ~ normal(0,1);
  phi ~ exponential(1);
  
  // Height model
  for(i in 1:N){
    mu[i] = alpha_height + beta_G_height*G[site[i]] + beta_cochineal_height*cochineal[i];
  }
  height ~ normal(mu, sigma);
  
  // Fruit model
  for(i in 1:N){
    lambda[i] = exp(alpha_fruit[site[i]] + 
    beta_height_fruit*height[i] + 
    beta_cochineal_fruit*cochineal[i] +
    beta_G_fruit*G[site[i]]);
  }
  fruit ~ neg_binomial_2(lambda, phi);
}
