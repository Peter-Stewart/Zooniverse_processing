# Load packages ####
library(MASS)
library(dplyr)
library(tidyr)
library(lubridate)
library(bipartite)
library(rethinking)
library(stringr)
library(forcats)
library(viridis)

source("C:/Users/PeteS/OneDrive/R Scripts Library/Projects/Zooniverse/helper_functions_v2.R", echo = FALSE)

# Load data ####
# Opuntia data from camera trap deployments
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
opuntia_data1 <- read.csv("Cameras_opuntia_data_main.csv", header = TRUE)

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data/Sign_survey_data")
opuntia_data2 <- read.csv("Sign_survey_opuntia_data_main.csv", header = TRUE)

# Interaction data
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
interactions1 <- read.csv("Fieldseason1_interactions_results.csv", header = TRUE)
interactions2 <- read.csv("Fieldseason2_interactions_results.csv", header = TRUE)

# Site data
site_data <- read.csv("Cameras_site_data_main.csv", header = TRUE)

# Consensus classifications
setwd("C:/temp/Zooniverse/Feb23/processed")
consensus_classifications <- get(load("consensus_classifications.Rdata"))

# Prepare dataframes ####
# Get both datasets into same format
# Make survey date variable for both datasets
opuntia_data1$Month <- ifelse(opuntia_data1$Deploy_month < 10, paste("0",opuntia_data1$Deploy_month,sep=""), opuntia_data1$Deploy_month)
opuntia_data1 <- opuntia_data1 %>% unite(DateNum, c("Deploy_year", "Month", "Deploy_day"), sep = "-", remove = FALSE)
opuntia_data1$DateLub <- as_date(opuntia_data1$DateNum)

opuntia_data2$Month <- ifelse(opuntia_data2$Month < 10, paste("0",opuntia_data2$Month,sep=""), opuntia_data2$Month)
opuntia_data2 <- opuntia_data2 %>% unite(DateNum, c("Year", "Month", "Day"), sep = "-", remove = FALSE)
opuntia_data2$DateLub <- as_date(opuntia_data2$DateNum)

# Remove columns we don't need for now
opuntia_data1 <- opuntia_data1 %>% 
                  select(-"Cam_ID",
                        -"DateNum",
                        -"Deploy_day",
                        -"Deploy_month",
                        -"Deploy_year",
                        -"Deploy_date",
                        -"Deploy_day_of_year",
                        -"Month")

opuntia_data2 <- opuntia_data2 %>% 
                  select(-"Replicate",
                        -"Quarter",
                        -"X",
                        -"Y",
                        -"DateNum",
                        -"Day",
                        -"Month",
                        -"Year",
                        -"Notes")

# Prefix sign survey sites so the stands are not incorrectly assigned to a camera trap site
opuntia_data2$Site_ID <- paste0("S", opuntia_data2$Site_ID)

# Bind into one dataset
opuntia_data <- rbind(opuntia_data1,opuntia_data2)
opuntia_data <- opuntia_data %>% filter(!is.na(Species))

# Change variable classes to factor where relevant, correct any inconsistent factor coding
opuntia_data$Site_ID <- as.factor(opuntia_data$Site_ID)
opuntia_data$Stand_ID <- as.factor(opuntia_data$Stand_ID)
opuntia_data$Under_tree <- ifelse(opuntia_data$Under_tree == "n" | opuntia_data$Under_tree == "N" | opuntia_data$Under_tree == "no", 0, opuntia_data$Under_tree)
opuntia_data$Under_tree <- ifelse(opuntia_data$Under_tree == "Y" | opuntia_data$Under_tree == "yes", 1, opuntia_data$Under_tree)
opuntia_data$Under_tree <- as.factor(opuntia_data$Under_tree)

# Remove NA's (from sites where no Opuntia present)
opuntia_data <- opuntia_data %>% filter(!is.na(opuntia_data$Under_tree))
opuntia_data <- opuntia_data %>% filter(!is.na(opuntia_data$Ripe_fruits))

# Split into stricta and engelmannii
stricta <- opuntia_data %>% filter(Species == "stricta")
engelmannii <- opuntia_data %>% filter(Species == "engelmannii")

# Prior predictive simulations ####
N <- 1000 # Number of iterations

# Proposed priors
alpha_prior <- rnorm(N, 0, 5)
beta_prior <- rnorm(N, 0, 1)
phi_prior <- rexp(N, 0.1)

# Calculate lambda over xseq
#xseq <- seq(from = min(opuntia_data$Max_height), to = max(opuntia_data$Max_height), by = 1)
xseq <- seq(from = min(standardize(opuntia_data$Max_height)), to = max(standardize(opuntia_data$Max_height)), by = 0.05)


# Visualise 
#plot(NULL, xlim=range(xseq), ylim=c(0,730), xlab="Max. height (cm)", ylab="Number of fruits", 
#     main = expression(alpha *" ~ Normal(0, 10),  " * beta * " ~ Normal(0, 0.01)"))

plot(NULL, xlim=range(xseq), ylim=c(0,730), xlab="Max. height (standardised)", ylab="Number of fruits", 
     main = expression(alpha *" ~ Normal(0, 5),  " * beta * " ~ Normal(0, 10)"))

#for ( i in 1:N ) curve(exp( alpha_prior[i] + beta_prior[i]*(x)) ,
#                       from=min(xseq) , to=max(xseq) , add=TRUE ,
#                       col=col.alpha("black",0.2) )

for ( i in 1:N ) curve(exp( alpha_prior[i] + beta_prior[i]*(x)) ,
                       from=min(xseq) , to=max(xseq) , add=TRUE ,
                       col=col.alpha("black",0.2) )

# Distribution of observations for different values of psi
sim_obs <- matrix(nrow=length(alpha_prior), ncol = length(xseq))
for(i in 1:length(xseq)){
  for(j in 1:length(alpha_prior)){
    lambda <- exp(alpha_prior[j] + beta_prior[j]*xseq[i])
    sim_obs[j,i] <- rnegbin(1, mu = lambda, theta = phi_prior[j]) 
  }
}
sim_obs_mu <- apply(sim_obs, 2, median)
sim_obs_pi95 <- apply(sim_obs, 2, HPDI, prob = 0.95)
sim_obs_pi89 <- apply(sim_obs, 2, HPDI, prob = 0.89)
sim_obs_pi80 <- apply(sim_obs, 2, HPDI, prob = 0.80)
sim_obs_pi70 <- apply(sim_obs, 2, HPDI, prob = 0.70)
sim_obs_pi60 <- apply(sim_obs, 2, HPDI, prob = 0.60)
sim_obs_pi50 <- apply(sim_obs, 2, HPDI, prob = 0.50)

plot(NULL, xlim=range(xseq), ylim=c(0,730), xlab="Max. height (standardised)", ylab="Number of fruits", 
     main = expression(alpha *" ~ Normal(0, 5),  " * beta * " ~ Normal(0, 1)  " * phi * " ~ Exponential(1)"))

points(x = xseq, y = sim_obs_mu, type = "l")
shade(sim_obs_pi95, xseq)
shade(sim_obs_pi89, xseq)
shade(sim_obs_pi80, xseq)
shade(sim_obs_pi70, xseq)
shade(sim_obs_pi60, xseq)
shade(sim_obs_pi50, xseq)


# Modelling relationship between size and fruiting ####
df <- stricta

dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = ifelse(df$Ripe_fruits > 0, 1, 0),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)

m1 <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/fruit_analysis/binomial_v3.stan",
            data = dlist,
            chains = 4,
            cores = 4,
            warmup = 28000,
            iter = 30000)

dashboard(m1); par(mfrow=c(1,1))
post <- extract.samples(m1)

dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = as.integer(df$Ripe_fruits),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)

m1 <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/fruit_analysis/neg_bin_varyingint_v3.stan",
            data = dlist,
            chains = 4,
            cores = 4,
            warmup = 28000,
            iter = 30000)

dashboard(m1); par(mfrow=c(1,1))
post <- extract.samples(m1)

# Modelling unobserved site-level confound ####
df <- engelmannii

dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = ifelse(df$Ripe_fruits > 0, 1, 0),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)

m1 <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/fruit_analysis/binomial_impute_v1.stan",
            data = dlist,
            chains = 4,
            cores = 4,
            warmup = 28000,
            iter = 30000)
dashboard(m1); par(mfrow=c(1,1))
post <- extract.samples(m1)

dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = as.integer(df$Ripe_fruits),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)

m1 <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/fruit_analysis/neg_bin_v7.stan",
            data = dlist,
            chains = 4,
            cores = 4,
            warmup = 28000,
            iter = 30000)
dashboard(m1); par(mfrow=c(1,1))
post <- extract.samples(m1)


# Load posterior distributions ####
setwd("E:/ch4_post")
post1 <- get(load("engelmannii_bin.Rdata"))
post2 <- get(load("engelmannii_neg_bin.Rdata"))
post3 <- get(load("engelmannii_bin_impute.Rdata"))
post4 <- get(load("engelmannii_neg_bin_impute.Rdata"))
post5 <- get(load("stricta_bin.Rdata"))
post6 <- get(load("stricta_neg_bin.Rdata"))
post7 <- get(load("stricta_bin_impute.Rdata"))
post8 <- get(load("stricta_neg_bin_impute.Rdata"))

effects_list_main <- list(post1, post2, post5, post6)

names(effects_list_main) <- c("engelmannii_bin", 
                              "engelmannii_neg_bin",
                              "stricta_bin",
                              "stricta_neg_bin")

# Size vs fruit figures for main text ####
#tiff("height_fruit_main.tiff", width = 15.82, height = 7.5, units = 'cm', res = 300)
tiff("height_fruit_main.tiff", width = 23, height = 7.5, units = 'cm', res = 300)
col_pal <- viridis(9)
col_alpha <- 0.3
ln <- 2 # For adjusting axis positions
par(mfrow=c(1,2),
    mar = c(2.6, 3, 2.1, 3) + 0.75,
    cex = 0.83)

df <- engelmannii
post <- effects_list_main$engelmannii_bin
dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = as.integer(df$Ripe_fruits),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)
xseq <- seq(min(dlist$height), max(dlist$height), by = 0.05)

theta <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  theta[,i] <- inv_logit(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}
theta_mu <- apply(theta, 2, median)
theta_pi95 <- apply(theta, 2, HPDI, prob = 0.95)
theta_pi89 <- apply(theta, 2, HPDI, prob = 0.89)
theta_pi80 <- apply(theta, 2, HPDI, prob = 0.80)
theta_pi70 <- apply(theta, 2, HPDI, prob = 0.70)
theta_pi60 <- apply(theta, 2, HPDI, prob = 0.60)
theta_pi50 <- apply(theta, 2, HPDI, prob = 0.50)

post <- effects_list_main$engelmannii_neg_bin
lambda <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  lambda[,i] <- exp(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}

lambda_mu <- apply(lambda, 2, median)
lambda_pi95 <- apply(lambda, 2, HPDI, prob = 0.95)
lambda_pi89 <- apply(lambda, 2, HPDI, prob = 0.89)
lambda_pi80 <- apply(lambda, 2, HPDI, prob = 0.80)
lambda_pi70 <- apply(lambda, 2, HPDI, prob = 0.70)
lambda_pi60 <- apply(lambda, 2, HPDI, prob = 0.60)
lambda_pi50 <- apply(lambda, 2, HPDI, prob = 0.50)

plot(NULL, xlim = range(xseq), 
     ylim=c(0,round(max(df$Ripe_fruits), digits = -1)), xlab="", 
     ylab="", 
     #main = substitute(paste(italic("O. engelmannii"))),
     xaxt = "n", yaxt = "n")
title("A)", adj=0, line = 0.7)

mtext("Max. height (cm)", side = 1, line = ln)
axis(side = 1, at = c(-1,0,1,2), labels = round(c(mean(df$Max_height)-sd(df$Max_height),
                                                  mean(df$Max_height),
                                                  mean(df$Max_height)+sd(df$Max_height),
                                                  mean(df$Max_height)+2*sd(df$Max_height)), 
                                                digits = 0))
axis(side = 2, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,0.5,1), padj = 0)
mtext("Probability of producing fruit", side = 2, line = ln)    

axis(side = 4, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), padj = 0)
mtext("Number of fruits", side = 4, line = ln) 

shade(lambda_pi95, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi89, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi80, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi70, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi60, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi50, xseq, col = col.alpha(col_pal[2], col_alpha))
points(x = xseq, y = lambda_mu, type = "l", lwd = 2)

shade(theta_pi95*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi89*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi80*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi70*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi60*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi50*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
points(x = xseq, y = theta_mu*round(max(df$Ripe_fruits), digits = -1), type = "l", lty = 2, lwd = 2)

points(x = dlist$height, y = dlist$fruit, pch=16)

df <- stricta
post <- effects_list_main$stricta_bin
dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = as.integer(df$Ripe_fruits),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)
xseq <- seq(min(dlist$height), max(dlist$height), by = 0.05)

theta <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  theta[,i] <- inv_logit(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}
theta_mu <- apply(theta, 2, median)
theta_pi95 <- apply(theta, 2, HPDI, prob = 0.95)
theta_pi89 <- apply(theta, 2, HPDI, prob = 0.89)
theta_pi80 <- apply(theta, 2, HPDI, prob = 0.80)
theta_pi70 <- apply(theta, 2, HPDI, prob = 0.70)
theta_pi60 <- apply(theta, 2, HPDI, prob = 0.60)
theta_pi50 <- apply(theta, 2, HPDI, prob = 0.50)

post <- effects_list_main$stricta_neg_bin
lambda <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  lambda[,i] <- exp(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}

lambda_mu <- apply(lambda, 2, median)
lambda_pi95 <- apply(lambda, 2, HPDI, prob = 0.95)
lambda_pi89 <- apply(lambda, 2, HPDI, prob = 0.89)
lambda_pi80 <- apply(lambda, 2, HPDI, prob = 0.80)
lambda_pi70 <- apply(lambda, 2, HPDI, prob = 0.70)
lambda_pi60 <- apply(lambda, 2, HPDI, prob = 0.60)
lambda_pi50 <- apply(lambda, 2, HPDI, prob = 0.50)


plot(NULL, xlim = range(xseq), 
     ylim=c(0,round(max(df$Ripe_fruits), digits = -1)), xlab="", 
     ylab="", 
     #main = substitute(paste(italic("O. stricta"))),
     xaxt = "n", yaxt = "n")
title("B)", adj=0, line = 0.7)

mtext("Max. height (cm)", side = 1, line = ln)
axis(side = 1, at = c(-1,0,1,2), labels = round(c(mean(df$Max_height)-sd(df$Max_height),
                                                  mean(df$Max_height),
                                                  mean(df$Max_height)+sd(df$Max_height),
                                                  mean(df$Max_height)+2*sd(df$Max_height)), 
                                                digits = 0))
axis(side = 2, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,0.5,1))
mtext("Probability of producing fruit", side = 2, line = ln)    

axis(side = 4, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)))
mtext("Number of fruits", side = 4, line = ln) 

shade(lambda_pi95, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi89, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi80, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi70, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi60, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi50, xseq, col = col.alpha(col_pal[2], col_alpha))
points(x = xseq, y = lambda_mu, type = "l", lwd = 2)

shade(theta_pi95*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi89*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi80*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi70*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi60*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi50*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
points(x = xseq, y = theta_mu*round(max(df$Ripe_fruits), digits = -1), type = "l", lty = 2, lwd = 2)

points(x = dlist$height, y = dlist$fruit, pch=16)
dev.off()

# Size vs fruit figures for supplementary material ####
tiff("height_fruit_sup.tiff", width = 23, height = 7.5, units = 'cm', res = 300)
col_pal <- viridis(9)
col_alpha <- 0.3
ln <- 2 # For adjusting axis positions
par(mfrow=c(1,2),
    mar = c(2.6, 3, 2.1, 3) + 0.75,
    cex = 0.83)

df <- engelmannii
post <- effects_list_sup$engelmannii_bin
dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = as.integer(df$Ripe_fruits),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)
xseq <- seq(min(dlist$height), max(dlist$height), by = 0.05)

theta <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  theta[,i] <- inv_logit(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}
theta_mu <- apply(theta, 2, median)
theta_pi95 <- apply(theta, 2, HPDI, prob = 0.95)
theta_pi89 <- apply(theta, 2, HPDI, prob = 0.89)
theta_pi80 <- apply(theta, 2, HPDI, prob = 0.80)
theta_pi70 <- apply(theta, 2, HPDI, prob = 0.70)
theta_pi60 <- apply(theta, 2, HPDI, prob = 0.60)
theta_pi50 <- apply(theta, 2, HPDI, prob = 0.50)

post <- effects_list_sup$engelmannii_neg_bin
lambda <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  lambda[,i] <- exp(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}

lambda_mu <- apply(lambda, 2, median)
lambda_pi95 <- apply(lambda, 2, HPDI, prob = 0.95)
lambda_pi89 <- apply(lambda, 2, HPDI, prob = 0.89)
lambda_pi80 <- apply(lambda, 2, HPDI, prob = 0.80)
lambda_pi70 <- apply(lambda, 2, HPDI, prob = 0.70)
lambda_pi60 <- apply(lambda, 2, HPDI, prob = 0.60)
lambda_pi50 <- apply(lambda, 2, HPDI, prob = 0.50)

plot(NULL, xlim = range(xseq), 
     ylim=c(0,round(max(df$Ripe_fruits), digits = -1)), xlab="", 
     ylab="", 
     #main = substitute(paste(italic("O. engelmannii"))),
     xaxt = "n", yaxt = "n")
title("A)", adj=0, line = 0.7)

mtext("Max. height (cm)", side = 1, line = ln)
axis(side = 1, at = c(-1,0,1,2), labels = round(c(mean(df$Max_height)-sd(df$Max_height),
                                                  mean(df$Max_height),
                                                  mean(df$Max_height)+sd(df$Max_height),
                                                  mean(df$Max_height)+2*sd(df$Max_height)), 
                                                digits = 0))
axis(side = 2, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,0.5,1), padj = 0)
mtext("Probability of producing fruit", side = 2, line = ln)    

axis(side = 4, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), padj = 0)
mtext("Number of fruits", side = 4, line = ln) 

shade(lambda_pi95, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi89, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi80, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi70, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi60, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi50, xseq, col = col.alpha(col_pal[2], col_alpha))
points(x = xseq, y = lambda_mu, type = "l", lwd = 2)

shade(theta_pi95*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi89*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi80*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi70*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi60*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi50*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
points(x = xseq, y = theta_mu*round(max(df$Ripe_fruits), digits = -1), type = "l", lty = 2, lwd = 2)

points(x = dlist$height, y = dlist$fruit, pch=16)

df <- stricta
post <- effects_list_sup$stricta_bin
dlist <- list(N = as.integer(nrow(df)),
              site = as.integer(as.factor(as.character(df$Site_ID))),
              n_sites = as.integer(length(unique(df$Site_ID))),
              fruit = as.integer(df$Ripe_fruits),
              height = standardize(as.numeric(df$Max_height)),
              cochineal = standardize(as.numeric(df$Cochineal_.)),
              tree = as.integer(df$Under_tree),
              grass = df$Grass + 1L,
              forb = df$Forb + 1L,
              shrub = df$Shrub + 1L)
xseq <- seq(min(dlist$height), max(dlist$height), by = 0.05)

theta <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  theta[,i] <- inv_logit(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}
theta_mu <- apply(theta, 2, median)
theta_pi95 <- apply(theta, 2, HPDI, prob = 0.95)
theta_pi89 <- apply(theta, 2, HPDI, prob = 0.89)
theta_pi80 <- apply(theta, 2, HPDI, prob = 0.80)
theta_pi70 <- apply(theta, 2, HPDI, prob = 0.70)
theta_pi60 <- apply(theta, 2, HPDI, prob = 0.60)
theta_pi50 <- apply(theta, 2, HPDI, prob = 0.50)

post <- effects_list_sup$stricta_neg_bin
lambda <- matrix(nrow=length(post$alpha_fruit_bar), ncol = length(xseq))
for(i in 1:length(xseq)){
  lambda[,i] <- exp(post$alpha_fruit_bar + post$beta_height_fruit*xseq[i])
}

lambda_mu <- apply(lambda, 2, median)
lambda_pi95 <- apply(lambda, 2, HPDI, prob = 0.95)
lambda_pi89 <- apply(lambda, 2, HPDI, prob = 0.89)
lambda_pi80 <- apply(lambda, 2, HPDI, prob = 0.80)
lambda_pi70 <- apply(lambda, 2, HPDI, prob = 0.70)
lambda_pi60 <- apply(lambda, 2, HPDI, prob = 0.60)
lambda_pi50 <- apply(lambda, 2, HPDI, prob = 0.50)


plot(NULL, xlim = range(xseq), 
     ylim=c(0,round(max(df$Ripe_fruits), digits = -1)), xlab="", 
     ylab="", 
     #main = substitute(paste(italic("O. stricta"))),
     xaxt = "n", yaxt = "n")
title("B)", adj=0, line = 0.7)

mtext("Max. height (cm)", side = 1, line = ln)
axis(side = 1, at = c(-1,0,1,2), labels = round(c(mean(df$Max_height)-sd(df$Max_height),
                                                  mean(df$Max_height),
                                                  mean(df$Max_height)+sd(df$Max_height),
                                                  mean(df$Max_height)+2*sd(df$Max_height)), 
                                                digits = 0))
axis(side = 2, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,0.5,1))
mtext("Probability of producing fruit", side = 2, line = ln)    

axis(side = 4, at = c(0, round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)), labels = c(0,round(max(df$Ripe_fruits), digits = -1)/2, round(max(df$Ripe_fruits), digits = -1)))
mtext("Number of fruits", side = 4, line = ln) 

shade(lambda_pi95, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi89, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi80, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi70, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi60, xseq, col = col.alpha(col_pal[2], col_alpha))
shade(lambda_pi50, xseq, col = col.alpha(col_pal[2], col_alpha))
points(x = xseq, y = lambda_mu, type = "l", lwd = 2)

shade(theta_pi95*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi89*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi80*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi70*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi60*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
shade(theta_pi50*round(max(df$Ripe_fruits), digits = -1), xseq, col = col.alpha(col_pal[6], col_alpha))
points(x = xseq, y = theta_mu*round(max(df$Ripe_fruits), digits = -1), type = "l", lty = 2, lwd = 2)

points(x = dlist$height, y = dlist$fruit, pch=16)
dev.off()

# Effect sizes figure for main text ####
n_effects <- 11
n_mod <- 4
PI_prob <- 0.95

effect_labs <- c("H %->% F",
                 "C %->% F",
                 "T %->% F",
                 "Gr %->% F",
                 "Fb %->% F",
                 "S %->% F",
                 "C %->% H",
                 "T %->% H",
                 "Gr %->% H",
                 "Fb %->% H",
                 "S %->% H")

effects_df_main <- as.data.frame(matrix(nrow = n_effects*n_mod, ncol = 6))
colnames(effects_df_main) <- c("species",
                               "model",
                               "label",
                               "median",
                               "L95CI",
                               "U95CI")

effects_df_main$species <- rep(c("engelmannii", "stricta"), each = n_effects*(n_mod/2))
effects_df_main$model <- rep(c("bin","neg_bin"), each = n_effects)
effects_df_main$label <- rep(effect_labs, times = n_mod)

for(i in 1:nrow(effects_df_main)){
  vr <- paste0(effects_df_main$species[i], "_", effects_df_main$model[i])
  post <- effects_list_main[[vr]]
  
  if(effects_df_main$label[i] == effect_labs[1]){
    effects_df_main$median[i] <- median(post$beta_height_fruit)
    effects_df_main$L95CI[i] <- HPDI(post$beta_height_fruit, prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$beta_height_fruit, prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[2]){
    effects_df_main$median[i] <- median(post$beta_cochineal_fruit)
    effects_df_main$L95CI[i] <- HPDI(post$beta_cochineal_fruit, prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$beta_cochineal_fruit, prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[3]){
    effects_df_main$median[i] <- median(post$alpha_tree_fruit[,2]-post$alpha_tree_fruit[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_tree_fruit[,2]-post$alpha_tree_fruit[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_tree_fruit[,2]-post$alpha_tree_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[4]){
    effects_df_main$median[i] <- median(post$alpha_grass_fruit[,2]-post$alpha_grass_fruit[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_grass_fruit[,2]-post$alpha_grass_fruit[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_grass_fruit[,2]-post$alpha_grass_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[5]){
    effects_df_main$median[i] <- median(post$alpha_forb_fruit[,2]-post$alpha_forb_fruit[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_forb_fruit[,2]-post$alpha_forb_fruit[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_forb_fruit[,2]-post$alpha_forb_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[6]){
    effects_df_main$median[i] <- median(post$alpha_shrub_fruit[,2]-post$alpha_shrub_fruit[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_shrub_fruit[,2]-post$alpha_shrub_fruit[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_shrub_fruit[,2]-post$alpha_shrub_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[7]){
    effects_df_main$median[i] <- median(post$beta_cochineal_height)
    effects_df_main$L95CI[i] <- HPDI(post$beta_cochineal_height, prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$beta_cochineal_height, prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[8]){
    effects_df_main$median[i] <- median(post$alpha_tree_height[,2]-post$alpha_tree_height[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_tree_height[,2]-post$alpha_tree_height[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_tree_height[,2]-post$alpha_tree_height[,1], prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[9]){
    effects_df_main$median[i] <- median(post$alpha_grass_height[,2]-post$alpha_grass_height[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_grass_height[,2]-post$alpha_grass_height[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_grass_height[,2]-post$alpha_grass_height[,1], prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[10]){
    effects_df_main$median[i] <- median(post$alpha_forb_height[,2]-post$alpha_forb_height[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_forb_height[,2]-post$alpha_forb_height[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_forb_height[,2]-post$alpha_forb_height[,1], prob = PI_prob)[2]
  }else if(effects_df_main$label[i] == effect_labs[11]){
    effects_df_main$median[i] <- median(post$alpha_shrub_height[,2]-post$alpha_shrub_height[,1])
    effects_df_main$L95CI[i] <- HPDI(post$alpha_shrub_height[,2]-post$alpha_shrub_height[,1], prob = PI_prob)[1]
    effects_df_main$U95CI[i] <- HPDI(post$alpha_shrub_height[,2]-post$alpha_shrub_height[,1], prob = PI_prob)[2]
  }
}

pr <- par()
tiff("morphology_effects_main.tiff", width = 15.82, height = 15, units = 'cm', res = 300)
par(mfrow = c(1,2),
    oma = c(0,5,0,0),
    mar = c(5,1,4,0.5))
off <- 0.15 # y offset for points to avoid overplotting
col_pal <- viridis(9)
xcoord <- -6.0

plot(NULL, 
     ylim = c(0, 12), 
     xlim = c(-4.2, 4.2),
     ylab = "",
     xlab = "Effect (median w/ 95% C.I.)",
     yaxt = "n",
     #xaxt = "n",
     main = substitute(paste(italic("O. engelmannii"))))
axis(2, at = 11:1, labels = FALSE)
text(x = xcoord, y = 11, labels = expression(H %->% F*" "), xpd = NA)
text(x = xcoord, y = 10, labels = expression(C %->% F*" "), xpd = NA)
text(x = xcoord, y = 9, labels = expression(T %->% F*" "), xpd = NA)
text(x = xcoord, y = 8, labels = expression(Gr %->% F*"   "), xpd = NA)
text(x = xcoord, y = 7, labels = expression(Fb %->% F*"   "), xpd = NA)
text(x = xcoord, y = 6, labels = expression(S %->% F*" "), xpd = NA)
text(x = xcoord, y = 5, labels = expression(C %->% H*" "), xpd = NA)
text(x = xcoord, y = 4, labels = expression(T %->% H*" "), xpd = NA)
text(x = xcoord, y = 3, labels = expression(Gr %->% H*"   "), xpd = NA)
text(x = xcoord, y = 2, labels = expression(Fb %->% H*"   "), xpd = NA)
text(x = xcoord, y = 1, labels = expression(S %->% H*" "), xpd = NA)
abline(v = 0, lty = 2)

df1 <- effects_df_main %>% filter(species %in% "engelmannii" & model %in% "bin")
points(y = 11:1 + off, x = df1$median, pch = 16, col = col_pal[6])
for(i in 1:11){
  lines(y = rep(12-i+off,2), x = c(df1$U95CI[i], df1$L95CI[i]), col = col_pal[6], lwd = 2)
}
df2 <- effects_df_main %>% filter(species %in% "engelmannii" & model %in% "neg_bin")
points(y = 11:1 - off, x = df2$median, pch = 15, col = col_pal[2])
for(i in 1:11){
  lines(y = rep(12-i-off,2), x = c(df2$U95CI[i], df2$L95CI[i]), col = col_pal[2], lwd = 2)
}

plot(NULL, 
     ylim = c(0, 12), 
     xlim = c(-4.2, 4.2),
     ylab = "",
     xlab = "Effect (median w/ 95% C.I.)",
     yaxt = "n",
     #xaxt = "n",
     main = substitute(paste(italic("O. stricta"))))
axis(2, at = 11:1, labels = FALSE)
abline(v = 0, lty = 2)

df3 <- effects_df_main %>% filter(species %in% "stricta" & model %in% "bin")
points(y = 11:1 + off, x = df3$median, pch = 16, col = col_pal[6])
for(i in 1:11){
  lines(y = rep(12-i+off,2), x = c(df3$U95CI[i], df3$L95CI[i]), col = col_pal[6], lwd = 2)
}
df4 <- effects_df_main %>% filter(species %in% "stricta" & model %in% "neg_bin")
points(y = 11:1 - off, x = df4$median, pch = 15, col = col_pal[2])
for(i in 1:11){
  lines(y = rep(12-i-off,2), x = c(df4$U95CI[i], df4$L95CI[i]), col = col_pal[2], lwd = 2)
}
dev.off()
par(pr)

# Effect sizes figure for supplementary material ####
effects_list_sup <- list(post3, post4, post7, post8)
names(effects_list_sup) <- c("engelmannii_bin", 
                             "engelmannii_neg_bin",
                             "stricta_bin",
                             "stricta_neg_bin")
effects_df_sup <- as.data.frame(matrix(nrow = n_effects*n_mod, ncol = 6))
colnames(effects_df_sup) <- c("species",
                              "model",
                              "label",
                              "median",
                              "L95CI",
                              "U95CI")

effects_df_sup$species <- rep(c("engelmannii", "stricta"), each = n_effects*(n_mod/2))
effects_df_sup$model <- rep(c("bin","neg_bin"), each = n_effects)
effects_df_sup$label <- rep(effect_labs, times = n_mod)

for(i in 1:nrow(effects_df_sup)){
  vr <- paste0(effects_df_sup$species[i], "_", effects_df_sup$model[i])
  post <- effects_list_sup[[vr]]
  
  if(effects_df_sup$label[i] == effect_labs[1]){
    effects_df_sup$median[i] <- median(post$beta_height_fruit)
    effects_df_sup$L95CI[i] <- HPDI(post$beta_height_fruit, prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$beta_height_fruit, prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[2]){
    effects_df_sup$median[i] <- median(post$beta_cochineal_fruit)
    effects_df_sup$L95CI[i] <- HPDI(post$beta_cochineal_fruit, prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$beta_cochineal_fruit, prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[3]){
    effects_df_sup$median[i] <- median(post$alpha_tree_fruit[,2]-post$alpha_tree_fruit[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_tree_fruit[,2]-post$alpha_tree_fruit[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_tree_fruit[,2]-post$alpha_tree_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[4]){
    effects_df_sup$median[i] <- median(post$alpha_grass_fruit[,2]-post$alpha_grass_fruit[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_grass_fruit[,2]-post$alpha_grass_fruit[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_grass_fruit[,2]-post$alpha_grass_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[5]){
    effects_df_sup$median[i] <- median(post$alpha_forb_fruit[,2]-post$alpha_forb_fruit[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_forb_fruit[,2]-post$alpha_forb_fruit[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_forb_fruit[,2]-post$alpha_forb_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[6]){
    effects_df_sup$median[i] <- median(post$alpha_shrub_fruit[,2]-post$alpha_shrub_fruit[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_shrub_fruit[,2]-post$alpha_shrub_fruit[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_shrub_fruit[,2]-post$alpha_shrub_fruit[,1], prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[7]){
    effects_df_sup$median[i] <- median(post$beta_cochineal_height)
    effects_df_sup$L95CI[i] <- HPDI(post$beta_cochineal_height, prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$beta_cochineal_height, prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[8]){
    effects_df_sup$median[i] <- median(post$alpha_tree_height[,2]-post$alpha_tree_height[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_tree_height[,2]-post$alpha_tree_height[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_tree_height[,2]-post$alpha_tree_height[,1], prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[9]){
    effects_df_sup$median[i] <- median(post$alpha_grass_height[,2]-post$alpha_grass_height[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_grass_height[,2]-post$alpha_grass_height[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_grass_height[,2]-post$alpha_grass_height[,1], prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[10]){
    effects_df_sup$median[i] <- median(post$alpha_forb_height[,2]-post$alpha_forb_height[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_forb_height[,2]-post$alpha_forb_height[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_forb_height[,2]-post$alpha_forb_height[,1], prob = PI_prob)[2]
  }else if(effects_df_sup$label[i] == effect_labs[11]){
    effects_df_sup$median[i] <- median(post$alpha_shrub_height[,2]-post$alpha_shrub_height[,1])
    effects_df_sup$L95CI[i] <- HPDI(post$alpha_shrub_height[,2]-post$alpha_shrub_height[,1], prob = PI_prob)[1]
    effects_df_sup$U95CI[i] <- HPDI(post$alpha_shrub_height[,2]-post$alpha_shrub_height[,1], prob = PI_prob)[2]
  }
}

pr <- par()
tiff("morphology_effects_sup.tiff", width = 15.82, height = 15, units = 'cm', res = 300)
par(mfrow = c(1,2),
    oma = c(0,5,0,0),
    mar = c(5,1,4,0.5))
off <- 0.15 # y offset for points to avoid overplotting
col_pal <- viridis(9)
xcoord <- -6.0

plot(NULL, 
     ylim = c(0, 12), 
     xlim = c(-4.2, 4.2),
     ylab = "",
     xlab = "Effect (median w/ 95% C.I.)",
     yaxt = "n",
     #xaxt = "n",
     main = substitute(paste(italic("O. engelmannii"))))
axis(2, at = 11:1, labels = FALSE)
text(x = xcoord, y = 11, labels = expression(H %->% F*" "), xpd = NA)
text(x = xcoord, y = 10, labels = expression(C %->% F*" "), xpd = NA)
text(x = xcoord, y = 9, labels = expression(T %->% F*" "), xpd = NA)
text(x = xcoord, y = 8, labels = expression(Gr %->% F*"   "), xpd = NA)
text(x = xcoord, y = 7, labels = expression(Fb %->% F*"   "), xpd = NA)
text(x = xcoord, y = 6, labels = expression(S %->% F*" "), xpd = NA)
text(x = xcoord, y = 5, labels = expression(C %->% H*" "), xpd = NA)
text(x = xcoord, y = 4, labels = expression(T %->% H*" "), xpd = NA)
text(x = xcoord, y = 3, labels = expression(Gr %->% H*"   "), xpd = NA)
text(x = xcoord, y = 2, labels = expression(Fb %->% H*"   "), xpd = NA)
text(x = xcoord, y = 1, labels = expression(S %->% H*" "), xpd = NA)
abline(v = 0, lty = 2)

df1 <- effects_df_sup %>% filter(species %in% "engelmannii" & model %in% "bin")
points(y = 11:1 + off, x = df1$median, pch = 16, col = col_pal[6])
for(i in 1:11){
  lines(y = rep(12-i+off,2), x = c(df1$U95CI[i], df1$L95CI[i]), col = col_pal[6], lwd = 2)
}
df2 <- effects_df_sup %>% filter(species %in% "engelmannii" & model %in% "neg_bin")
points(y = 11:1 - off, x = df2$median, pch = 15, col = col_pal[2])
for(i in 1:11){
  lines(y = rep(12-i-off,2), x = c(df2$U95CI[i], df2$L95CI[i]), col = col_pal[2], lwd = 2)
}

plot(NULL, 
     ylim = c(0, 12), 
     xlim = c(-4.2, 4.2),
     ylab = "",
     xlab = "Effect (median w/ 95% C.I.)",
     yaxt = "n",
     #xaxt = "n",
     main = substitute(paste(italic("O. stricta"))))
axis(2, at = 11:1, labels = FALSE)
abline(v = 0, lty = 2)

df3 <- effects_df_sup %>% filter(species %in% "stricta" & model %in% "bin")
points(y = 11:1 + off, x = df3$median, pch = 16, col = col_pal[6])
for(i in 1:11){
  lines(y = rep(12-i+off,2), x = c(df3$U95CI[i], df3$L95CI[i]), col = col_pal[6], lwd = 2)
}
df4 <- effects_df_sup %>% filter(species %in% "stricta" & model %in% "neg_bin")
points(y = 11:1 - off, x = df4$median, pch = 15, col = col_pal[2])
for(i in 1:11){
  lines(y = rep(12-i-off,2), x = c(df4$U95CI[i], df4$L95CI[i]), col = col_pal[2], lwd = 2)
}
dev.off()
par(pr)

# Analysing behavioural interactions ####
interactions <- rbind(interactions1, interactions2)

interactions <- interactions %>% select(-ImageQuality, 
                                        -DeleteFlag,
                                        -RelativePath,
                                        -Folder)

interactions$site <- interactions$File
interactions$site <- gsub('_part.*','',interactions$site)
interactions$site <- gsub('Site_','',interactions$site)
interactions$site <- as.integer(interactions$site)

interactions$Type <- as.factor(interactions$Type)
interactions$Confidence <- as.factor(interactions$Confidence)

# Create DateTime column
interactions <- interactions %>% separate(Date, into = c("Day", "Month", "Year"), sep = "-", remove = FALSE, convert = FALSE)
interactions$Month <- match(interactions$Month, month.abb)
interactions$Month2 <- as.character(interactions$Month)
interactions$Month2 <- ifelse(interactions$Month < 10, paste("0",interactions$Month2,sep=""), interactions$Month2)
interactions <- interactions %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)
interactions <- interactions %>% unite(DateTimeNum, c("DateNum", "Time"), sep = " ", remove = FALSE)
interactions$DateTimeLub <- as_datetime(interactions$DateTimeNum)

# Dates/times are wrong for three sites where camera 15 was deployed, due to a fault with the camera
# Correct these using offset calculated from known deployment time

# Site 35 parts 1 and 2 (part 3 used cam 6 which is ok)
errors <- interactions %>% filter(grepl("Site_35_part1|Site_35_part2",File))
correct <- interactions %>% filter(!grepl("Site_35_part1|Site_35_part2",File))

errors$DateTimeLub <- errors$DateTimeLub + 34443510 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Dates are wrong for Site 69
# timestamp = 2020-01-21 15:09:37
# true time = 2021-02-23 15:11:00
# offset = 34,473,683 seconds
rm(errors); rm(correct)

errors <- interactions %>% filter(grepl("Site_69",File))
correct <- interactions %>% filter(!grepl("Site_69",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473683 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Dates are wrong for site 86 part 1
# timestamp = 2020-02-10 13:08:00
# true time = 2021-03-15 13:09:00
# offset = 34,473,660
rm(errors); rm(correct)

errors <- interactions %>% filter(grepl("Site_86_part1",File))
correct <- interactions %>% filter(!grepl("Site_86_part1",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473660 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Dates are wrong for site 86 parts 2 to 3
# timestamp = 2020-01-01 12:01:29
# true time = 2021-03-19 11:55:00
# offset = 38,274,811

rm(errors); rm(correct)

errors <- interactions %>% filter(grepl("Site_86_part2|Site_86_part3",File))
correct <- interactions %>% filter(!grepl("Site_86_part2|Site_86_part3",File))

errors$DateTimeLub <- errors$DateTimeLub + 38274811 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

interactions <- rbind(errors, correct)

# Make sure to only use dry season data for now
dry_season <- interval("2021-01-01", "2021-05-01")
interactions <- interactions %>% filter(DateTimeLub %within% dry_season)

# Group interactions by interaction ID. For each ID get highest confidence rating for each type of interaction.
Max_conf <- interactions %>% group_by(Interaction_ID, Type) %>% summarise(Max_conf_int = max(as.integer(Confidence)))
Max_conf$Max_conf <- as.factor(Max_conf$Max_conf_int)
levels(Max_conf$Max_conf) <- levels(interactions$Confidence)

interactions <- merge(interactions, Max_conf, by = c("Interaction_ID","Type"))

# Remove interactions where max confidence is "unsure" or type is "none"
interactions <- interactions %>% filter(Max_conf != "Unsure") %>%
  filter(Type != "None")

# Split 'notes' variable to deal with cases with multiple species and/or interactions
interactions$Notes <- replace(interactions$Notes, interactions$Notes=="",NA)

interactionsA <- interactions %>% filter(!is.na(Notes))
interactionsB <- interactions %>% filter(is.na(Notes))

interactionsA <- interactionsA %>% separate_rows(Notes, sep = ",") %>% 
  select(-Type) %>%
  separate(Notes, into = c("species", "Type"), sep = "-", remove = FALSE, convert = FALSE)

# Merge with consensus classifications to match interaction with species
consensus_classifications <- consensus_classifications %>%
  mutate(File = str_extract(metadata, regex("(Site)(.*)(jpg)", ignore_case = TRUE))) %>%
  filter(!is.na(species))

consensus_sp <- consensus_classifications %>% select(File, species)

interactionsB <- merge(interactionsB, consensus_sp, by = "File", all.x = TRUE)

#check2 <- interactionsB %>% group_by(File) %>% filter(n_distinct(species) > 1) %>% filter(Type != "None")

# Deal with cases where no consensus classification (species = NA)
# Create list of these files - open and classify in Timelapse 
interactions_X <- rbind(interactionsA, interactionsB)

missing_sp <- interactions_X %>% filter(species=="noanimalspresent"|is.na(species))
missing_sp$subfolder <- gsub("_IMG.*","",missing_sp$File)
missing_sp$subfolder <- gsub("_RCNX.*","",missing_sp$subfolder)
missing_sp$path <- paste0("Site_",missing_sp$site,"/",
                          missing_sp$subfolder,"/",
                          missing_sp$File)
missing_sp_filelist <- missing_sp$path

#write.table(missing_sp_filelist, file = "interactions_missing_sp_filelist.txt",
#           sep = "\t", col.names = FALSE, row.names = FALSE)

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
missing_sp_ids <- read.csv("interactions_missing_sp.csv", header = TRUE)

missing_sp_ids <- missing_sp_ids %>% separate(Notes, into = c("species", "Type"), sep = "-", remove = FALSE, convert = FALSE) %>%
  select(File, species)
colnames(missing_sp_ids) <- c("File", "species_id")

interactions_X <- interactions_X %>% filter(species != "noanimalspresent")
interactions_Y <- merge(interactions_X, missing_sp_ids, by = "File", all.x = TRUE)

interactions_Y <- interactions_Y %>% mutate(species = coalesce(species, species_id))

# Deal with cases where other animals are in background
# Currently there are none of these which have not been dealt with using the "notes" field

# Create variable for the species of Opuntia that is present at each site
opuntia_sp <- opuntia_data1 %>% select(Site_ID, Species) %>% distinct()

# For cases where multiple sp. or no sp. (because cactus is >10m away) check camera images
opuntia_sp <- opuntia_sp %>% filter(!is.na(Species)) %>% 
  filter(Species != "ficus_indica") %>% 
  filter(Site_ID %notin% c(66,96,98))

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
opuntia_sp_manual <- read.csv("opuntia_site_sp.csv", header = TRUE)
opuntia_sp <- opuntia_sp %>% filter(Site_ID %notin% opuntia_sp_manual$Site_ID)
opuntia_sp <- rbind(opuntia_sp, opuntia_sp_manual)
colnames(opuntia_sp) <- c("Site_ID", "opuntia_sp")

interactions_Z <- merge(interactions_Y, opuntia_sp, by.x = "site", by.y = "Site_ID", all.x = TRUE)
interactions <- interactions_Z

# Clean up interaction type variable
interactions$Type <- fct_recode(interactions$Type, Eating_fruit = "Eating fruit")
interactions$Type <- fct_recode(interactions$Type, Eating_fruit = "eatingfrui")
interactions$Type <- fct_recode(interactions$Type, Eating_fruit = "eatingfruit")

interactions$Type <- fct_recode(interactions$Type, Eating_pads_or_roots = "Eating pads/roots")
interactions$Type <- fct_recode(interactions$Type, Eating_pads_or_roots = "eatingpadsroots")

interactions$Type <- fct_recode(interactions$Type, Eating_other_vegetation = "Eating other vegetation")
interactions$Type <- fct_recode(interactions$Type, Eating_other_vegetation = "eatingothervegetation")

interactions$Type <- fct_recode(interactions$Type, Hiding_under_cactus = "Hiding under cactus")
interactions$Type <- fct_recode(interactions$Type, Hiding_under_cactus = "hidingundercactus")

interactions$Type <- fct_recode(interactions$Type, Eating_flowers = "eatingflower")

interactions$Type <- fct_recode(interactions$Type, Perching_on_cactus = "perching")

# Create variable which separates interaction type by Opuntia species
interactions$Type_opuntia_sp <- paste0(interactions$opuntia_sp,"_",interactions$Type)

# Keep only unique interaction classifications for each interaction ID
interactions_unique <- interactions %>% select(Interaction_ID, 
                                               species, 
                                               Type, 
                                               opuntia_sp, 
                                               Type_opuntia_sp)

interactions_unique <- unique(interactions_unique)

engelmannii_unique <- interactions_unique %>% filter(opuntia_sp == "engelmannii")
stricta_unique <- interactions_unique %>% filter(opuntia_sp == "stricta")

# Create interaction matrices
interaction_matrix <- as.matrix(table(interactions$Type_opuntia_sp, interactions$species))

interaction_matrix_unique <- as.matrix(table(interactions_unique$Type_opuntia_sp, interactions_unique$species))

engelmannii_matrix_unique <- as.matrix(table(engelmannii_unique$Type, engelmannii_unique$species))
stricta_matrix_unique <- as.matrix(table(stricta_unique$Type, stricta_unique$species))

sp_labs <- c("Olive baboon",
             "Bird (other)",
             "Buffalo",
             "Camel",
             "Dikdik",
             "Elephant",
             "Vulturine guineafowl",
             "Hippo",
             "Impala",
             "Livestock",
             "Oryx",
             "Squirrel",
             "Vervet monkey",
             "Warthog",
             "Grevy's zebra",
             "Plains zebra")

int_labs <- c("Eating flowers e",
              "Eating fruit e",
              "Eating other veg. e",
              "Hiding under cactus e",
              "Perching e",
              "Eating fruit s",
              "Eating other veg. s",
              "Eating pads/roots s",
              "Hiding under cactus s",
              "Perching s")

sp_labs_eng <- c("Olive baboon",
             "Bird (other)",
             "Buffalo",
             "Camel",
             "Dikdik",
             "Elephant",
             "Vulturine guineafowl",
             "Impala",
             "Livestock",
             "Oryx",
             "Squirrel",
             "Vervet monkey",
             "Warthog",
             "Grevy's zebra",
             "Plains zebra")

sp_labs_str <- c("Olive baboon",
                 "Bird (other)",
                 "Buffalo",
                 "Dikdik",
                 "Elephant",
                 "Hippo",
                 "Impala",
                 "Livestock",
                 "Vervet monkey")

int_labs_eng <- c("Eating fruit",
                  "Eating other veg.",
                  "Eating pads/roots",
                  "Eating flowers",
                  "Hiding under cactus",
                  "Perching")

int_labs_str <- int_labs_eng

colnames(interaction_matrix) <- sp_labs
rownames(interaction_matrix) <- int_labs

colnames(interaction_matrix_unique) <- sp_labs
rownames(interaction_matrix_unique) <- int_labs

colnames(engelmannii_matrix_unique) <- sp_labs_eng
rownames(engelmannii_matrix_unique) <- int_labs_eng

colnames(stricta_matrix_unique) <- sp_labs_str
rownames(stricta_matrix_unique) <- int_labs_str

# Plot bipartite network
pal1 <- viridis(length(unique(interactions$species)))
x1 <- ifelse(sp_labs %in% sp_labs_eng,1,0)
x2 <- ifelse(sp_labs %in% sp_labs_str,1,0)

pal1a <- pal1[which(x1==1)]
pal1b <- pal1[which(x2==1)]

pal2 <- c(rep("#05520d", 5),
          rep("#03a81f", 5))
pal3 <- rep("#05520d", 5)
pal4 <- rep("#03a81f", 5)

llength <- str_length(int_labs) - 2L

plotweb(interaction_matrix,
              method = "normal",
              text.rot = 0,
              col.high = pal1,
              col.interaction = pal1,
              col.low = pal2,
              low.lablength = llength)

plotweb(interaction_matrix_unique,
        method = "normal",
        text.rot = 0,
        col.high = pal1,
        col.interaction = pal1,
        col.low = pal2,
        low.lablength = llength)

par(mfrow=c(1,2))
plotweb(engelmannii_matrix_unique,
        method = "normal",
        text.rot = 0,
        col.high = pal1a,
        col.interaction = pal1a,
        col.low = pal3)

plotweb(stricta_matrix_unique,
        method = "normal",
        text.rot = 0,
        col.high = pal1b,
        col.interaction = pal1b,
        col.low = pal4)
par(mfrow=c(1,1))

# Calculate interaction metrics 
# Species level
specieslevel(interaction_matrix_unique)

# Network level
metrics <- c("links per species",
             "number of compartments",
             "cluster coefficient",
             "weighted NODF",
             "linkage density",
             "Shannon diversity",
             "H2")

#networklevel(interaction_matrix_unique, index = metrics)
n1 <- networklevel(engelmannii_matrix_unique, index = metrics)
n2 <- networklevel(stricta_matrix_unique, index = metrics)



#
visweb(interaction_matrix_unique)
visweb(engelmannii_matrix_unique)
visweb(stricta_matrix_unique)
