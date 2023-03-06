data{
  int<lower=0> N; // Number of observations
  array[N] int<lower=0> fruit; // Number of fruits
  array[N] real height; // Max. height of stand (cm)
  array[N] real cochineal; // Percentage of pads covered in cochineal
}
parameters{
  real alpha; // Intercept
  real beta_height; // Effect of height on fruit
  real beta_cochineal;
  real<lower=0> phi; // Variance of negative binomial distribution
}
model{
  vector[N] lambda; // Rate parameter
  
  // Priors
  alpha ~ normal(0,10);
  beta_height ~ normal(0,0.05);
  beta_cochineal ~ normal(0,0.05);
  phi ~ exponential(1);
  
  // Likelihood
  for(i in 1:N){
    lambda[i] = exp(alpha + beta_height*height[i] + beta_cochineal*cochineal[i]);
  }
  fruit ~ neg_binomial_2(lambda, phi);
}
