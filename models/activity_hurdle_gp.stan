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
  array[n_obs] real x; //Covariate x
  matrix[n_obs, n_obs] dmat; // Distance matrix
}
parameters{
  real<lower=0> sigma;
  real beta_fruit;
  real k_bar;
  real omega_bar;
  real gamma;
  
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
  sigma ~ exponential(1);
  beta_fruit ~ normal(0,1);
  k_bar ~ normal(0,1);
  omega_bar ~ normal(0,1);
  gamma ~ normal(0,1);
  
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
      1 ~ bernoulli(inv_logit(omega_bar + k2[i] + gamma*x[i]));
    } else {
      0 ~ bernoulli(inv_logit(omega_bar + k2[i] + gamma*x[i]));
      y_obs[i] ~ normal(k_bar + k[i] + beta_fruit*x[i], sigma) T[0.00000001, ];
    }
  }
}
