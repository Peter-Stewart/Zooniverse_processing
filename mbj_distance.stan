data {
  int<lower=1> n_site;
  int<lower=1> n_distance_bins;
  vector[n_distance_bins + 1] bin_breakpoints;
  array[n_site, n_distance_bins] int y;
  array[n_site] int n_obs;
}

transformed data {
  real max_distance = max(bin_breakpoints);
  real log_two = log(2);
  real log_max_distance_sq = 2 * log(max(bin_breakpoints));
  vector[n_distance_bins + 1] bin_breakpoints_sq = bin_breakpoints^2;
}

parameters {
  real log_lambda;
  real log_sigma;
}

transformed parameters {
  real log_p;
  vector[n_distance_bins] log_p_raw;
  
  {
    real two_sig_sq = 2 * exp(2 * log_sigma);
    for (i in 1:n_distance_bins) {
      // p_raw[i] = pr(animal occurs and is detected in bin i)
      // assuming a half-normal detection fn
      log_p_raw[i] = log_two + 2 * log_sigma - log_max_distance_sq +
        log_diff_exp(
          - bin_breakpoints_sq[i] / two_sig_sq,
          - bin_breakpoints_sq[i + 1] / two_sig_sq
        );
    }
    log_p = log_sum_exp(log_p_raw);
  }
}

model {
  log_lambda ~ normal(3, 1);
  log_sigma ~ normal(4, 1);
  n_obs ~ poisson_log(log_lambda + log_p);
  for (i in 1:n_site) {
    y[i, ] ~ multinomial_logit(log_p_raw);
  }
}

generated quantities {
  array[n_site] int n_unobserved;
  array[n_site] int n;
  
  for (i in 1:n_site) {
    n_unobserved[i] = poisson_rng(exp(log_lambda) * (1 - exp(log_p)));
    n[i] = n_obs[i] + n_unobserved[i];
  }
}