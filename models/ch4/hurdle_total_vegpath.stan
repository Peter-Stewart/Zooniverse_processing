functions{
    matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + delta;
        return K;
    }
}
data{
  int n_obs; // Number of observations
  array[n_obs] real y_obs; // Observed data
  array[n_obs] real opuntia; // Opuntia % cover (standardised)
  array[n_obs] real surveys; // Survey effort (number of camera trap days, standardised)
  array[n_obs] real d_water; // Distance to river (standardised)
  array[n_obs] real d_road; // Distance to road (standardised)
  array[n_obs] real livestock; // Proportion of days where livestock are present (standardised)
  matrix[n_obs, n_obs] dmat; // Distance matrix
}
parameters{
  real<lower=0> sigma;
  
  real k_bar;
  real<lower=0> beta_surveys;
  real beta_opuntia;
  real beta_d_river;
  real beta_d_road;
  real beta_livestock;
  
  real omega_bar;
  real<upper=0> gamma_surveys;
  real gamma_opuntia;
  real gamma_d_river;
  real gamma_d_road;
  real gamma_livestock;
  
  // Gaussian process parameters
  vector[n_obs] z; // z-scores for intercept term (for non-centred parameterisation)
  real<lower=0> etasq; // Maximum covariance between sites
  real<lower=0> rhosq; // Rate of decline in covariance with distance
  
  vector[n_obs] z2; // z-scores for intercept term (for non-centred parameterisation)
  real<lower=0> etasq2; // Maximum covariance between sites
  real<lower=0> rhosq2; // Rate of decline in covariance with distance
}
model{
// Priors
  sigma ~ exponential(5);
  
  k_bar ~ normal(0,0.4);
  beta_surveys ~ normal(0,0.4);
  beta_opuntia ~ normal(0,0.4);
  beta_d_river ~ normal(0,0.4);
  beta_d_road ~ normal(0,0.4);
  beta_livestock ~ normal(0,0.4);
  
  omega_bar ~ normal(0,1);
  gamma_surveys ~ normal(0,1);
  gamma_opuntia ~ normal(0,1);
  gamma_d_river ~ normal(0,1);
  gamma_d_road ~ normal(0,1);
  gamma_livestock ~ normal(0,1);
  
  etasq ~ exponential(2);
  rhosq ~ lognormal(0,1);
  z ~ normal(0,1);
  
  etasq2 ~ exponential(2);
  rhosq2 ~ lognormal(0,1);
  z2 ~ normal(0,1);
  
// Gaussian process
  matrix[n_obs, n_obs] L_SIGMA; // Cholesky-decomposed covariance matrix
  matrix[n_obs, n_obs] SIGMA; // Covariance matrix
  vector[n_obs] k; // Intercept term for each site (perturbation from k_bar)

  matrix[n_obs, n_obs] L_SIGMA2; // Cholesky-decomposed covariance matrix
  matrix[n_obs, n_obs] SIGMA2; // Covariance matrix
  vector[n_obs] k2; // Intercept term for each site (perturbation from k_bar)

  SIGMA = cov_GPL2(dmat, etasq, rhosq, 0.01);
  L_SIGMA = cholesky_decompose(SIGMA);
  k = L_SIGMA * z;
 
  SIGMA2 = cov_GPL2(dmat, etasq2, rhosq2, 0.01);
  L_SIGMA2 = cholesky_decompose(SIGMA2);
  k2 = L_SIGMA2 * z2;
  
// Likelihood
  for(i in 1:n_obs){
    if (y_obs[i] == 0) {
      1 ~ bernoulli(inv_logit(omega_bar + k2[i] + 
      gamma_surveys*surveys[i] + 
      gamma_opuntia*opuntia[i] +
      gamma_d_river*d_water[i] + 
      gamma_d_road*d_road[i] +
      gamma_livestock*livestock[i]));
    } else {
      0 ~ bernoulli(inv_logit(omega_bar + k2[i] + 
      gamma_surveys*surveys[i] + 
      gamma_opuntia*opuntia[i] +
      gamma_d_river*d_water[i] + 
      gamma_d_road*d_road[i] +
      gamma_livestock*livestock[i]));
      y_obs[i] ~ lognormal(k_bar + k[i] + 
      beta_surveys*surveys[i] + 
      beta_opuntia*opuntia[i] +
      beta_d_river*d_water[i] + 
      beta_d_road*d_road[i] +
      beta_livestock*livestock[i], 
      sigma) T[0.00000001, ];
    }
  }
}
