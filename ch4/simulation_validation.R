# Load packages ####
library(rethinking)
library(MASS)
library(dplyr)
library(tidyr)
library(bayesplot)

# Source helper functions ####
source("C:/Users/PeteS/OneDrive/R Scripts Library/Projects/Zooniverse/helper_functions_v2.R", echo = FALSE)

# Load real data so we can use the real distances between sites ####
setwd("C:/temp/Zooniverse/Oct22/processed")
sitedays <- get(load("sitedays.Rdata"))
sitedays_aggregated <- sitedays %>% group_by(Site) %>% summarise(Days_total = sum(Days))

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
site_data <- read.csv("Cameras_site_data_main.csv", header = TRUE)
site_data <- site_data %>% filter(Site_ID %in% sitedays$Site)

# Generate synthetic data ####
# Set seed
set.seed(6322)

# Basic parameters for simulation
sites <- nrow(site_data) # Use real number of sites
days <- as.integer(sitedays_aggregated$Days_total) # Use real number of deployment days for each camera

# Create distance matrix and squared distance matrix
dmat <- generate_distance_matrix(site_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)
dmat2 <- dmat^2 # Squared distances

# Pick values for eta2 and rho2 then generate covariance matrix
eta2 <- 1.1
rho2 <- 0.9

# Examine how covariance changes with distance based on the chosen values of eta2 and rho2 (not accounting for delta)
par(mfrow=c(1,2))
curve(eta2*exp(-rho2*x^2),from=0, to=10, lty=1, xlab="Distance", ylab="Covariance") # Visualise covariance (y) vs distance (x)
hist(as.vector(dmat), breaks=100, xlab="Distance (rescaled)") # Visualise the distances between sites as well
par(mfrow=c(1,1))

# Generate covariance matrix
covmat <- eta2*exp(-rho2*dmat2)

# Generate varying intercept 
k_bar <- 0 # Select value for k_bar

varint <- mvrnorm(n = 1, 
                  mu = rep(k_bar, nrow(dmat)), 
                  Sigma = covmat)
varint <- as.numeric(varint)   

# True effect sizes:
betax <- 1.0 # Effect of x on psi
betam <- -0.8 # Effect of m on psi
betamx <- 0.5 # Effect of m on x

# Explanatory variables
m <- rnorm(nrow(dmat), 0, 1)
x <- rnorm(nrow(dmat), betamx*m, 1)

# True occupancy probability at each site
psi <- inv_logit(k_bar + varint + betax*x + betam*m)

# Covariate for detection probability
w <- matrix(NA, nrow=nrow(dmat), ncol=max(sitedays_aggregated$Days_total))
for(i in 1:nrow(w)){
  for(k in 1:ncol(w)){
    w[i,k] <- rnorm(1,0,1)
  }
}

# True detection probability
alphadet <- -1 # 
betadet <- 0.4 # 

pdet <- matrix(NA, nrow=nrow(w), ncol=ncol(w))
for(i in 1:nrow(pdet)){
  for(k in 1:ncol(pdet)){
    pdet[i,k] <- inv_logit(alphadet + betadet*w[i,k])
  }
}

# True occupancy state
z <- rep(NA, length=nrow(dmat))
for(i in 1:nrow(dmat)){
  z[i] <- rbinom(1,1,psi[i])
}

# Observed detection history
y <- matrix(NA, nrow=nrow(dmat), ncol=max(sitedays_aggregated$Days_total))
for(i in 1:nrow(y)){
  for(k in 1:ncol(y)){
    y[i,k] <- rbinom(1,1,pdet[i,k]*z[i])
  }
}

# Replace observed data with NA for visits which did not take place
for(i in 1:nrow(y)){
  for(k in 1:max(sitedays_aggregated$Days_total)){
    if(k <= sitedays_aggregated$Days_total[i])
      y[i, k] <- y[i, k]
    else
      y[i, k] <- NA
  }
}

# Replace NA values with ridiculous number, so it is obvious if the values are ever accessed by the model
y[is.na(y)] <- -9999
mode(y) <- "integer"

w[is.na(w)] <- -9999


# Data list for Stan ####
dlist <- list(
  # Number of sites and visits
  nsites = as.integer(nrow(dmat)),
  N_maxvisits = as.integer(max(sitedays_aggregated$Days_total)),
  V = as.integer(sitedays_aggregated$Days_total),
  # Observed data
  y = y,
  # Occupancy covariates
  x = x,
  m = m,
  # Detection covariates
  w = w,
  # Distance matrix
  dmat = dmat
)

# Run model ####
mtest <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/occupancy_models/ch3/sim_test.stan",
               data = dlist,
               chains = 4,
               cores = 4,
               warmup = 3000,
               iter = 4000,
               seed = 998)

# Model diagnostics ####
# Diagnostic plots
dashboard(mtest)

# Summary table
precis(mtest)
  
# Trankplots
trankplot(mtest, pars = c("beta_x", "beta_m", "alphadet", "betadet", "k_bar", "etasq", "rhosq"))

# Bayesplot diagnostics
color_scheme_set("darkgray") # Set colour scheme for Bayesplot diagnostics
np <- nuts_params(mtest) # Extract NUTS parameters
mcmc_nuts_energy(np) # Centred marginal energy distribution and first-differenced distribution overlaid

# Extract posterior samples
post <- extract.samples(mtest)

# Compare posterior distributions for key parameters with true values ####
# Get names of key parameters to plot
key_pars <- names(post)[grep("beta", names(post))] # Beta parameters
key_pars2 <- c("alphadet", "k_bar") # Other parameters
key_pars <- c(key_pars, key_pars2)
par_symbols <- c(expression(beta[x]),
                 expression(beta[m]),
                 expression(beta[DET]),
                 expression(alpha[DET]),
                 expression(bar(k)))

true_vals <- c(betax, betam, betadet, alphadet, k_bar) # True values

par(mfrow=c(ceiling(length(key_pars)/3),3))

for(p in 1:length(key_pars)){
  pr <- post[names(post) %in% key_pars[p]]
  pr <- pr[[1]]
  den <- density(pr)
  PI95 <- HPDI(pr, prob = 0.95)
  PI89 <- HPDI(pr, prob = 0.89)
  PI80 <- HPDI(pr, prob = 0.80)
  PI70 <- HPDI(pr, prob = 0.70)
  PI60 <- HPDI(pr, prob = 0.60)
  PI50 <- HPDI(pr, prob = 0.50)
  PI_all <- rbind(PI95, PI89, PI80, PI70, PI60, PI50)
  plot(den, main = par_symbols[p])
  for(i in 1:nrow(PI_all)){
    l <- min(which(den$x >= PI_all[i,1]))
    h <- max(which(den$x < PI_all[i,2]))
    polygon(c(den$x[c(l, l:h, h)]),
            c(0, den$y[l:h], 0),
            col = col.alpha("black", 0.15), border=NA)
  }
  abline(v = true_vals[p], lty=2)
}

# Compare posterior distributions for psi with true values ####
# Calculate posterior median and compatability intervals
psi_mu <- apply(post$psi,2,median)
#psi_mu <- apply(post$psi,2,chainmode) # Or use this code instead to show the posterior mode

psi_PI95 <- apply(post$psi, 2, HPDI, prob=0.95)
psi_PI89 <- apply(post$psi, 2, HPDI, prob=0.89)
psi_PI80 <- apply(post$psi, 2, HPDI, prob=0.80)
psi_PI70 <- apply(post$psi, 2, HPDI, prob=0.70)
psi_PI60 <- apply(post$psi, 2, HPDI, prob=0.60)
psi_PI50 <- apply(post$psi, 2, HPDI, prob=0.50)

# Plot estimate against true value
par(mfrow=c(1,1))
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab=expression("True "*psi*" value"), ylab=expression(hat(psi)))
abline(a=0,b=1, lty=2) # true = predicted

# Compatability intervals
wd <- 2.5 # Width for compatability interval lines

for(i in 1:length(psi)){
  lines(x = rep(psi[i],2), y = c(psi_PI95[1,i], psi_PI95[2,i]), col=col.alpha("black", 0.2), lwd = wd)
}
for(i in 1:length(psi)){
  lines(x = rep(psi[i],2), y = c(psi_PI89[1,i], psi_PI89[2,i]), col=col.alpha("black", 0.2), lwd = wd)
}
for(i in 1:length(psi)){
  lines(x = rep(psi[i],2), y = c(psi_PI80[1,i], psi_PI80[2,i]), col=col.alpha("black", 0.2), lwd = wd)
}
for(i in 1:length(psi)){
  lines(x = rep(psi[i],2), y = c(psi_PI70[1,i], psi_PI70[2,i]), col=col.alpha("black", 0.2), lwd = wd)
}
for(i in 1:length(psi)){
  lines(x = rep(psi[i],2), y = c(psi_PI60[1,i], psi_PI60[2,i]), col=col.alpha("black", 0.2), lwd = wd)
}
for(i in 1:length(psi)){
  lines(x = rep(psi[i],2), y = c(psi_PI50[1,i], psi_PI50[2,i]), col=col.alpha("black", 0.2), lwd = wd)
}

# Median - sites with more visits have darker points
for(i in 1:length(psi)){
  points(x = psi[i], y = psi_mu[i], pch = 16, col=col.alpha("black",  sitedays_aggregated$Days_total[i]/max(sitedays_aggregated$Days_total)))
}
#points(x = psi, y = psi_mu, pch=16, col="black") # Or use this code for points instead to make them all same shade

# Compare actual vs. estimated spatial autocorrelation ####

# Compute true correlation matrix
Rho_t <- round(cov2cor(covmat),2)
rownames(Rho_t) <- seq(1,nrow(covmat),1)
colnames(Rho_t) <- rownames(Rho)

par(mfrow=c(1,2))

# Plot correlations on a map
plot(GPS_lat~GPS_long, data=site_data, col=rangi2, pch=16, main="True correlation", xlab="longitude", ylab="latitude")

for(i in 1:nrow(dmat)){
  for(j in 1:ncol(dmat)){
    if(i < j){
      lines(x = c(site_data$GPS_long[i], site_data$GPS_long[j]), 
            y = c(site_data$GPS_lat[i], site_data$GPS_lat[j]),
            lwd=2, col=col.alpha("black", Rho_t[i,j]^2))
    }
  }
}

# Model's median correlation matrix
K <- matrix(0, nrow=nrow(dmat), ncol=ncol(dmat))
for(i in 1:nrow(dmat)){
  for(j in 1:ncol(dmat)){
    K[i,j] <- median(post$etasq) * exp(-median(post$rhosq)*dmat2[i,j])
  }
}
diag(K) <- median(post$etasq) + 0.01

Rho <- round(cov2cor(K),2)
rownames(Rho) <- seq(1,nrow(dmat),1)
colnames(Rho) <- rownames(Rho)

# Plot correlations on a map
plot(GPS_lat~GPS_long, data=site_data, col=rangi2, pch=16, main="Estimated correlation (median)", xlab="longitude", ylab="latitude")

for(i in 1:nrow(dmat)){
  for(j in 1:ncol(dmat)){
    if(i < j){
      lines(x = c(site_data$GPS_long[i], site_data$GPS_long[j]), 
            y = c(site_data$GPS_lat[i], site_data$GPS_lat[j]),
            lwd=2, col=col.alpha("black", Rho[i,j]^2))
    }
  }
}

