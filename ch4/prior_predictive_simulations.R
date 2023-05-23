library(rethinking)
library(MASS)

# Gaussian process parameters ####
# Need to consider actual distances between sites in order to choose sensible priors
setwd("C:/Zooniverse_processing/PhD Data")
site_data <- read.csv("Cameras_site_data_main.csv", header = TRUE)
site_data <- site_data %>% filter(Site_ID %in% sitedays$Site)
source("C:/Zooniverse_processing/helper_functions.R", echo = FALSE)
dmat <- generate_distance_matrix(site_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)
dists <- dmat[upper.tri(dmat, diag = TRUE)] # Only distances i -> j or i - > i, not j -> i

dmat_raw <- generate_distance_matrix(site_data, rescale = FALSE, log = FALSE, jitter = FALSE)
dists_raw <- dmat_raw[upper.tri(dmat_raw, diag = TRUE)] # Only distances i -> j or i - > i, not j -> i

N <- 1e4 # Number of samples to draw
x_seq <- seq(0,6.1,0.01)

# Option 1 - exponential for rho2
par(mfrow=c(2,2))

eta2_prior <- rexp(N, 1)
rho2_prior <- rexp(N, 1)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,4), main=expression(eta^2 * "~ Exponential(1), " * rho^2 * "~ Exponential(1)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

#
eta2_prior <- rexp(N, 2)
rho2_prior <- rexp(N, 1)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,4), main=expression(eta^2 * "~ Exponential(2), " * rho^2 * "~ Exponential(1)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

#
eta2_prior <- rexp(N, 1)
rho2_prior <- rexp(N, 2)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,4), main=expression(eta^2 * "~ Exponential(1), " * rho^2 * "~ Exponential(2)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

#
eta2_prior <- rexp(N, 2)
rho2_prior <- rexp(N, 2)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,4), main=expression(eta^2 * "~ Exponential(2), " * rho^2 * "~ Exponential(2)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

# Option 2 - lognormal for rho2
par(mfrow=c(1,3))

eta2_prior <- rexp(N, 1)
rho2_prior <- rlnorm(N, 0, 0.5)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,4), main=expression(eta^2 * "~ Exponential(1), " * rho^2 * "~ Log-normal(0, 0.5)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

#
eta2_prior <- rexp(N, 1)
rho2_prior <- rlnorm(N, 0, 1)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,4), main=expression(eta^2 * "~ Exponential(1), " * rho^2 * "~ Log-normal(0, 1)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

#
eta2_prior <- rexp(N, 1)
rho2_prior <- rlnorm(N, 0, 2)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,4), main=expression(eta^2 * "~ Exponential(1), " * rho^2 * "~ Log-normal(0, 2)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)


# Actual distances
par(mfrow=c(1,2))
hist(dists_raw, breaks=100, xlab = "Distance (m)", main="")
hist(dists, breaks=100, xlab = "Distance (rescaled)", main="")


# Final prior
eta2_prior <- rexp(N, 2)
rho2_prior <- rlnorm(N, 0, 1)

priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, mean)
priorcov_nu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

par(mfrow=c(1,1))
plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,1.5), main=expression(eta^2 * "~ Exponential(2), " * rho^2 * "~ Log-normal(0, 1)"))
lines(x_seq, priorcov_mu, lwd=2, lty=2)
lines(x_seq, priorcov_nu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

# Parameters for occupancy and detection submodels ####
N <- 500 # Number of lines to draw from priors

# Alpha and beta priors
alpha1 <- rnorm( N , 0 , 100 ) 
beta1 <- rnorm( N , 0 , 100 )

alpha2 <- rnorm( N , 0 , 10 ) 
beta2 <- rnorm( N , 0 , 10 )

alpha3 <- rnorm( N , 0 , 1 ) 
beta3 <- rnorm( N , 0 , 1 )

# Plots
par(mfrow=c(1,3))

## Make an empty plot
plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(alpha *" ~ Normal(0, 100),  " * beta * " ~ Normal(0, 100)"))
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for ( i in 1:N ) curve(inv_logit( alpha1[i] + beta1[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )

## Make an empty plot
plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(alpha *" ~ Normal(0, 10),  " * beta * " ~ Normal(0, 10)"))
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for ( i in 1:N ) curve(inv_logit( alpha2[i] + beta2[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )

## Make an empty plot
plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(alpha *" ~ Normal(0, 1),  " * beta * " ~ Normal(0, 1)"))
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for ( i in 1:N ) curve(inv_logit( alpha3[i] + beta3[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )


# Changing alpha while holding beta constant
# Alpha and beta priors
alpha1 <- rnorm( N , 0 , 2 ) 
beta1 <- rnorm( N , 0 , 1 )

alpha2 <- rnorm( N , 0 , 1 ) 
beta2 <- rnorm( N , 0 , 1 )

alpha3 <- rnorm( N , 0 , 0.5 ) 
beta3 <- rnorm( N , 0 , 1 )

# Plots
par(mfrow=c(1,3))

## Make an empty plot
plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(alpha *" ~ Normal(0, 2),  " * beta * " ~ Normal(0, 1)"))
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for ( i in 1:N ) curve(inv_logit( alpha1[i] + beta1[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )

## Make an empty plot
plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(alpha *" ~ Normal(0, 1),  " * beta * " ~ Normal(0, 1)"))
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for ( i in 1:N ) curve(inv_logit( alpha2[i] + beta2[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )

## Make an empty plot
plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(alpha *" ~ Normal(0, 0.5),  " * beta * " ~ Normal(0, 1)"))
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for ( i in 1:N ) curve(inv_logit( alpha3[i] + beta3[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )







# Selecting a suitable prior for k_bar
N <- 500

# Use priors for eta2 and rho2 which were selected above.
eta2_prior <- rexp(N, 2)
rho2_prior <- rlnorm(N, 0, 1)

dmat2 <- dmat^2

covmat_prior <- array(data = NA, dim=c(nrow(dmat), ncol(dmat), N))
for(k in 1:N){
  covmat_prior[,,k] <- eta2_prior[k]*exp(-rho2_prior[k]*dmat2)
}

k_prior <- matrix(data = NA, nrow = nrow(dmat), ncol = N)
for(k in 1:N){
  k_prior[,k] <- mvrnorm(n = 1,
                         mu = rep(0, nrow(dmat)),
                         Sigma = covmat_prior[,,k])
}

k_bar_prior1 <- rnorm(N, 0, 1)
k_bar_prior2 <- rnorm(N, 0, 0.5)
k_bar_prior3 <- rnorm(N, 0, 0.2)

# Use prior for beta which was selected above.
beta_prior <- rnorm(N, 0, 1)

par(mfrow=c(1,3))
plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(bar(k) *" ~ Normal(0, 1)" ), cex.main = 2)
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for(j in 1:4){
for ( i in 1:N ) curve(inv_logit( k_bar_prior1[i] + k_prior[j,i] + beta_prior[i]*(x)) ,
                       from=-2 , to=2 , add=TRUE ,
                       col=col.alpha("black",0.2) )
}

plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(bar(k) *" ~ Normal(0, 0.5)" ), cex.main = 2)
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for(j in 1:4){
  for ( i in 1:N ) curve(inv_logit( k_bar_prior2[i] + k_prior[j,i] + beta_prior[i]*(x)) ,
                         from=-2 , to=2 , add=TRUE ,
                         col=col.alpha("black",0.2) )
}

plot( NULL , xlim=c(-2,2) , ylim=c(0,1) , xlab="x" , ylab="probability", main = expression(bar(k) *" ~ Normal(0, 0.2)" ), cex.main = 2)
abline( h=0.5 , lty=2 ) # Add horizontal line at 0.5 detection probablity
# Draw N lines using our priors
for(j in 1:4){
  for ( i in 1:N ) curve(inv_logit( k_bar_prior3[i] + k_prior[j,i] + beta_prior[i]*(x)) ,
                         from=-2 , to=2 , add=TRUE ,
                         col=col.alpha("black",0.2) )
}


# Hurdle model ####
N <- 1e4

alpha <- rnorm(N, 0, 0.4)
beta <- rnorm(N, 0, 0.4)
sigma <- rexp(N, 5)

x_seq <- seq(-2, 2, by = 0.01)

y_sim <- matrix(NA, nrow = length(x_seq), ncol=N)
for(i in 1:length(x_seq)){
  y_sim[i,] <- rlnorm(n = N, 
                      meanlog = alpha + beta*x_seq[i], 
                      sdlog = sigma)
}

y_sim_mu <- apply(y_sim, 1, median)
y_sim_mu2 <- apply(y_sim, 1, mean)
y_sim_ci <- apply(y_sim, 1, HPDI, prob = 0.95)
y_sim_ci2 <- apply(y_sim, 1, HPDI, prob = 0.89)
y_sim_ci3 <- apply(y_sim, 1, HPDI, prob = 0.90)
y_sim_ci4 <- apply(y_sim, 1, HPDI, prob = 0.80)
y_sim_ci5 <- apply(y_sim, 1, HPDI, prob = 0.70)
y_sim_ci6 <- apply(y_sim, 1, HPDI, prob = 0.60)
y_sim_ci7 <- apply(y_sim, 1, HPDI, prob = 0.50)

plot( NULL , xlim=c(-2,2) , ylim=c(0,5) , xlab="x" , ylab="y", main = expression(alpha *" ~ Normal(0, 0.4),  " * beta * " ~ Normal(0, 0.4), " * sigma * " ~ Exponential(5)"))
abline(v=0, lty=2)
lines(x_seq, y_sim_mu, lwd=2)
lines(x_seq, y_sim_mu2, lwd=2, lty=2)
shade(y_sim_ci, x_seq)
shade(y_sim_ci2, x_seq)
shade(y_sim_ci3, x_seq)
shade(y_sim_ci4, x_seq)
shade(y_sim_ci5, x_seq)
shade(y_sim_ci6, x_seq)
shade(y_sim_ci7, x_seq)

