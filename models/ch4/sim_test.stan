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
  // Numbers of sites and visits
  int<lower=1> nsites; // Number of sites
  int<lower=1> N_maxvisits; // Maximum number of survey visits received by a site
  array[nsites] int<lower=1> V; // Number of visits per site
  
  // Observed presence/absence data (NA's replaced with -9999)
  array[nsites, N_maxvisits] int<upper=1> y; 
  
  // Occupancy covariates
  array[nsites] real x; // covariate x (variable of interest)
  array[nsites] real m; // covariate m (confound)

  // Detection covariates
  array[nsites, N_maxvisits] real w; // Detection covariate, varies with time

  // Distance matrix
  matrix[nsites, nsites] dmat;
  
}

parameters{
  // Occupancy submodel
  real k_bar; // Average occupancy in entire population of sites
  real beta_x; // Effect of Opuntia
  real beta_m; // Effect of fruit
  
  // Detection submodel
  real alphadet; // Detection intercept
  real betadet; // Effect of detection covariate

  // Gaussian process parameters
  vector[nsites] z; // z-scores for intercept term (for non-centred parameterisation)
  real<lower=0> etasq; // Maximum covariance between sites
  real<lower=0> rhosq; // Rate of decline in covariance with distance
}

transformed parameters{
  vector[nsites] psi; // Probability of occurrence at each site i
  array[nsites, N_maxvisits] real pij; // Probability of detection at each site i at each visit j
  
  matrix[nsites, nsites] L_SIGMA; // Cholesky-decomposed covariance matrix
  matrix[nsites, nsites] SIGMA; // Covariance matrix
  vector[nsites] k; // Intercept term for each site (offset from k_bar)
 
 // Gaussian process - non-centred
  SIGMA = cov_GPL2(dmat, etasq, rhosq, 0.01);
  L_SIGMA = cholesky_decompose(SIGMA);
  k = L_SIGMA * z;
  
  // Calculate psi_i and pij
  for(isite in 1:nsites){
    // Occupancy submodel
    psi[isite] = inv_logit(k_bar + k[isite] + x[isite]*beta_x + m[isite]*beta_m);

    // Detection submodel
    for(ivisit in 1:V[isite]){
      pij[isite, ivisit] = inv_logit(alphadet + betadet*w[isite,ivisit]);
    }
  }
  
}

model{
  
  vector[nsites] log_psi; // Log of psi
  vector[nsites] log1m_psi; // Log of 1-psi
  
  // Priors
  k_bar ~ normal(0, 0.5);
  beta_x ~ normal(0,1);
  beta_m ~ normal(0,1);
  
  alphadet ~ normal(0,0.5);
  betadet ~ normal(0,1);

  etasq ~ exponential(2);
  rhosq ~ lognormal(0, 1);
  z ~ normal(0, 1);
  
  // Log psi and log(1-psi)
  for(isite in 1:nsites){
    log_psi[isite] = log(psi[isite]);
    log1m_psi[isite] = log1m(psi[isite]);
  }
  
  // Likelihood
  for(isite in 1:nsites){
    
    if(sum(y[isite, 1:V[isite]]) > 0){
      target += log_psi[isite] + bernoulli_lpmf(y[isite, 1:V[isite]] | pij[isite, 1:V[isite]]);
    } else {
      target += log_sum_exp(log_psi[isite] + bernoulli_lpmf(y[isite, 1:V[isite]] | pij[isite, 1:V[isite]]), log1m_psi[isite]);
    }
  }
}
