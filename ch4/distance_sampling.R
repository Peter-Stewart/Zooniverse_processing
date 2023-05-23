# See here: https://discourse.mc-stan.org/t/deriving-abundance-from-a-distance-sampling-model/24565/7

# Packages
library(rethinking)
library(dplyr)
library(tidyr)

# Load data
setwd("C:/temp/opuntia_distance")

df <- read.csv("opuntia_density_transects_updated.csv")
df$grid_square <- as.factor(df$grid_square)
df$species <- as.factor(df$species)
df$size <- as.factor(df$size)
df$fruit <- as.factor(df$fruit)
df$indicator <- 1

t_lengths <- read.csv("transect_lengths.csv")
t_lengths$grid_squar <- as.factor(t_lengths$grid_squar)
colnames(t_lengths) <- c("grid_square","length")

df <- merge(df, t_lengths, by="grid_square")

# Make sure sites are same order in both df and t_lengths
df <- df[order(df$grid_square),]
t_lengths <- t_lengths[order(t_lengths$grid_square),]


# Get rid of everything over 80m
df <- df %>% filter(distance <= 80)

# Get data into required format ####
# Make each site one row, each column one distance bin, each cell a count of individuals
# Also separate out size, fruit classes into separate dataframes
# Large, fruiting
df_f_l <- df %>% filter(size=="large", fruit=="yes") %>% 
  select(grid_square, distance, indicator) %>% group_by(grid_square, distance) %>% summarise(n = sum(indicator)) %>%
  arrange(distance) %>% pivot_wider(names_from = distance, values_from = n)
df_f_l2 <- data.matrix(df_f_l) 
df_f_l2 <- df_f_l2[,-1]
row.names(df_f_l2) <- df_f_l$grid_square
y_f_l <- matrix(NA, nrow = length(unique(df$grid_square)), ncol=80, dimnames = list(unique(df$grid_square),1:80))
i <- match(rownames(df_f_l2), rownames(y_f_l))
j <- match(colnames(df_f_l2), colnames(y_f_l))
y_f_l[i,j] <- df_f_l2
y_f_l[is.na(y_f_l)] <- 0
n_obs_f_l <- rowSums(y_f_l)

# Large, non-fruiting
df_n_l <- df %>% filter(size=="large", fruit=="no") %>% 
  select(grid_square, distance, indicator) %>% group_by(grid_square, distance) %>% summarise(n = sum(indicator)) %>%
  arrange(distance) %>% pivot_wider(names_from = distance, values_from = n)
df_n_l2 <- data.matrix(df_n_l) 
df_n_l2 <- df_n_l2[,-1]
row.names(df_n_l2) <- df_n_l$grid_square
y_n_l <- matrix(NA, nrow = length(unique(df$grid_square)), ncol=80, dimnames = list(unique(df$grid_square),1:80))
i <- match(rownames(df_n_l2), rownames(y_n_l))
j <- match(colnames(df_n_l2), colnames(y_n_l))
y_n_l[i,j] <- df_n_l2
y_n_l[is.na(y_n_l)] <- 0
n_obs_n_l <- rowSums(y_n_l)

# Medium, fruiting
df_f_m <- df %>% filter(size=="medium", fruit=="yes") %>% 
  select(grid_square, distance, indicator) %>% group_by(grid_square, distance) %>% summarise(n = sum(indicator)) %>%
  arrange(distance) %>% pivot_wider(names_from = distance, values_from = n)
df_f_m2 <- data.matrix(df_f_m) 
df_f_m2 <- df_f_m2[,-1]
row.names(df_f_m2) <- df_f_m$grid_square
y_f_m <- matrix(NA, nrow = length(unique(df$grid_square)), ncol=80, dimnames = list(unique(df$grid_square),1:80))
i <- match(rownames(df_f_m2), rownames(y_f_m))
j <- match(colnames(df_f_m2), colnames(y_f_m))
y_f_m[i,j] <- df_f_m2
y_f_m[is.na(y_f_m)] <- 0
n_obs_f_m <- rowSums(y_f_m)

# Medium, non-fruiting
df_n_m <- df %>% filter(size=="medium", fruit=="no") %>% 
  select(grid_square, distance, indicator) %>% group_by(grid_square, distance) %>% summarise(n = sum(indicator)) %>%
  arrange(distance) %>% pivot_wider(names_from = distance, values_from = n)
df_n_m2 <- data.matrix(df_n_m) 
df_n_m2 <- df_n_m2[,-1]
row.names(df_n_m2) <- df_n_m$grid_square
y_n_m <- matrix(NA, nrow = length(unique(df$grid_square)), ncol=80, dimnames = list(unique(df$grid_square),1:80))
i <- match(rownames(df_n_m2), rownames(y_n_m))
j <- match(colnames(df_n_m2), colnames(y_n_m))
y_n_m[i,j] <- df_n_m2
y_n_m[is.na(y_n_m)] <- 0
n_obs_n_m <- rowSums(y_n_m)

# Small, fruiting
df_f_s <- df %>% filter(size=="small", fruit=="yes") %>% 
  select(grid_square, distance, indicator) %>% group_by(grid_square, distance) %>% summarise(n = sum(indicator)) %>%
  arrange(distance) %>% pivot_wider(names_from = distance, values_from = n)
df_f_s2 <- data.matrix(df_f_s) 
df_f_s2 <- df_f_s2[,-1]
row.names(df_f_s2) <- df_f_s$grid_square
y_f_s <- matrix(NA, nrow = length(unique(df$grid_square)), ncol=80, dimnames = list(unique(df$grid_square),1:80))
i <- match(rownames(df_f_s2), rownames(y_f_s))
j <- match(colnames(df_f_s2), colnames(y_f_s))
y_f_s[i,j] <- df_f_s2
y_f_s[is.na(y_f_s)] <- 0
n_obs_f_s <- rowSums(y_f_s)

# Small, non-fruiting
df_n_s <- df %>% filter(size=="small", fruit=="no") %>% 
  select(grid_square, distance, indicator) %>% group_by(grid_square, distance) %>% summarise(n = sum(indicator)) %>%
  arrange(distance) %>% pivot_wider(names_from = distance, values_from = n)
df_n_s2 <- data.matrix(df_n_s) 
df_n_s2 <- df_n_s2[,-1]
row.names(df_n_s2) <- df_n_s$grid_square
y_n_s <- matrix(NA, nrow = length(unique(df$grid_square)), ncol=80, dimnames = list(unique(df$grid_square),1:80))
i <- match(rownames(df_n_s2), rownames(y_n_s))
j <- match(colnames(df_n_s2), colnames(y_n_s))
y_n_s[i,j] <- df_n_s2
y_n_s[is.na(y_n_s)] <- 0
n_obs_n_s <- rowSums(y_n_s)

# Run models ####
# Large, fruiting
dlist_f_l <- list(
  n_site = length(unique(df$grid_square)),
  n_distance_bins = 80L,
  bin_breakpoints = seq(0, 80, length.out = 80+1),
  y = y_f_l,
  n_obs = n_obs_f_l
)
mod_f_l <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/distance_sampling/mbj_distance.stan",
                 data = dlist_f_l,
                 chains = 4, 
                 cores = 4,
                 warmup = 1500, 
                 iter = 3500)
post_f_l <- extract.samples(mod_f_l)

# Large, non-fruiting
dlist_n_l <- list(
  n_site = length(unique(df$grid_square)),
  n_distance_bins = 80L,
  bin_breakpoints = seq(0, 80, length.out = 80+1),
  y = y_n_l,
  n_obs = n_obs_n_l
)
mod_n_l <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/distance_sampling/mbj_distance.stan",
                 data = dlist_n_l,
                 chains = 4, 
                 cores = 4,
                 warmup = 1500, 
                 iter = 3500)
post_n_l <- extract.samples(mod_n_l)

# Medium, fruiting
dlist_f_m <- list(
  n_site = length(unique(df$grid_square)),
  n_distance_bins = 80L,
  bin_breakpoints = seq(0, 80, length.out = 80+1),
  y = y_f_m,
  n_obs = n_obs_f_m
)
mod_f_m <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/distance_sampling/mbj_distance.stan",
                 data = dlist_f_m,
                 chains = 4, 
                 cores = 4,
                 warmup = 1500, 
                 iter = 3500)
post_f_m <- extract.samples(mod_f_m)

# Medium, non-fruiting
dlist_n_m <- list(
  n_site = length(unique(df$grid_square)),
  n_distance_bins = 80L,
  bin_breakpoints = seq(0, 80, length.out = 80+1),
  y = y_n_m,
  n_obs = n_obs_n_m
)
mod_n_m <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/distance_sampling/mbj_distance.stan",
                 data = dlist_n_m,
                 chains = 4, 
                 cores = 4,
                 warmup = 1500, 
                 iter = 3500)
post_n_m <- extract.samples(mod_n_m)

# Small, fruiting
dlist_f_s <- list(
  n_site = length(unique(df$grid_square)),
  n_distance_bins = 80L,
  bin_breakpoints = seq(0, 80, length.out = 80+1),
  y = y_f_s,
  n_obs = n_obs_f_s
)
mod_f_s <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/distance_sampling/mbj_distance.stan",
                 data = dlist_f_s,
                 chains = 4, 
                 cores = 4,
                 warmup = 1500, 
                 iter = 3500)
post_f_s <- extract.samples(mod_f_s)

# Small, non-fruiting
dlist_n_s <- list(
  n_site = length(unique(df$grid_square)),
  n_distance_bins = 80L,
  bin_breakpoints = seq(0, 80, length.out = 80+1),
  y = y_n_s,
  n_obs = n_obs_n_s
)
mod_n_s <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/distance_sampling/mbj_distance.stan",
                 data = dlist_n_s,
                 chains = 4, 
                 cores = 4,
                 warmup = 1500, 
                 iter = 3500)
post_n_s <- extract.samples(mod_n_s)

# Plot inferred number of stands for each site +/- 89% compatability interval ####
par(mfrow=c(3,2))
mu_f_l <- apply(post_f_l$n, 2, median) / t_lengths$length
pi89_f_l <- apply(post_f_l$n, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_f_l)){
  pi89_f_l[,i] <- pi89_f_l[,i] / t_lengths$length[i]
}
plot(NULL, xlim=c(1,41), ylim=c(0,1), xlab="Grid square", 
     xaxt = "n",
     ylab="Number of stands per transect metre (median +/- 89% C.I.", main="Large, fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_f_l), las = 2)
points(x=1:41, y=mu_f_l, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_f_l[2,i],pi89_f_l[1,i]))
}

mu_n_l <- apply(post_n_l$n, 2, median) / t_lengths$length
pi89_n_l <- apply(post_n_l$n, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_f_l)){
  pi89_n_l[,i] <- pi89_n_l[,i] / t_lengths$length[i]
}
plot(NULL, xlim=c(1,41), ylim=c(0,1), xlab="Grid square", 
     xaxt = "n",
     ylab="Number of stands per transect metre (median +/- 89% C.I.", main="Large, non-fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_n_l), las = 2)
points(x=1:41, y=mu_n_l, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_n_l[2,i],pi89_n_l[1,i]))
}

mu_f_m <- apply(post_f_m$n, 2, median) / t_lengths$length
pi89_f_m <- apply(post_f_m$n, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_f_l)){
  pi89_f_m[,i] <- pi89_f_m[,i] / t_lengths$length[i]
}
plot(NULL, xlim=c(1,41), ylim=c(0,2.5), xlab="Grid square", 
     xaxt = "n",
     ylab="Number of stands per transect metre (median +/- 89% C.I.", main="Medium, fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_f_m), las = 2)
points(x=1:41, y=mu_f_m, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_f_m[2,i],pi89_f_m[1,i]))
}

mu_n_m <- apply(post_n_m$n, 2, median) / t_lengths$length
pi89_n_m <- apply(post_n_m$n, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_f_l)){
  pi89_n_m[,i] <- pi89_n_m[,i] / t_lengths$length[i]
}
plot(NULL, xlim=c(1,41), ylim=c(0,2.5), xlab="Grid square", 
     xaxt = "n",
     ylab="Number of stands per transect metre (median +/- 89% C.I.", main="Medium, non-fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_n_m), las = 2)
points(x=1:41, y=mu_n_m, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_n_m[2,i],pi89_n_m[1,i]))
}

mu_f_s <- apply(post_f_s$n, 2, median) / t_lengths$length
pi89_f_s <- apply(post_f_s$n, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_f_l)){
  pi89_f_s[,i] <- pi89_f_s[,i] / t_lengths$length[i]
}
plot(NULL, xlim=c(1,41), ylim=c(0,3.5), xlab="Grid square", 
     xaxt = "n",
     ylab="Number of stands per transect metre (median +/- 89% C.I.", main="Small, fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_f_s), las = 2)
points(x=1:41, y=mu_f_s, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_f_s[2,i],pi89_f_s[1,i]))
}

mu_n_s <- apply(post_n_s$n, 2, median) / t_lengths$length
pi89_n_s <- apply(post_n_s$n, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_f_l)){
  pi89_n_s[,i] <- pi89_n_s[,i] / t_lengths$length[i]
}
plot(NULL, xlim=c(1,41), ylim=c(0,3.5), xlab="Grid square", 
     xaxt = "n",
     ylab="Number of stands per transect metre (median +/- 89% C.I.", main="Small, non-fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_n_s), las = 2)
points(x=1:41, y=mu_n_s, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_n_s[2,i],pi89_n_s[1,i]))
}


# Combine estimates for each size/fruit class to get an overall Opuntia (relative) volume for each grid square
# Model each size class as hemisphere with r = 0.5m, 1.5m, 2.5m (small, medium and large respectively) to get volume
fruiting_volume <- post_f_l$n * 32.725 +
                   post_f_m$n * 7.07 +
                   post_f_s$n * 0.26

non_fruiting_volume <- post_n_l$n * 32.725 +
                       post_n_m$n * 7.07 +
                       post_n_s$n * 0.26

total_volume <- fruiting_volume + non_fruiting_volume

mu_f <- apply(fruiting_volume, 2, median) / t_lengths$length
pi89_f <- apply(fruiting_volume, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_f)){
  pi89_f[,i] <- pi89_f[,i] / t_lengths$length[i]
}
mu_n <- apply(non_fruiting_volume, 2, median) / t_lengths$length
pi89_n <- apply(non_fruiting_volume, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_n)){
  pi89_n[,i] <- pi89_n[,i] / t_lengths$length[i]
}
mu_t <- apply(total_volume, 2, median) / t_lengths$length
pi89_t <- apply(total_volume, 2, HPDI, prob=0.89)
for(i in 1:ncol(pi89_t)){
  pi89_t[,i] <- pi89_t[,i] / t_lengths$length[i]
}

# Plot
par(mfrow=c(3,1))

plot(NULL, xlim=c(1,41), ylim=c(0,35), xlab="Grid square", 
     xaxt = "n",
     ylab="Relative volume (median +/- 89% C.I.", main="Fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_n_s), las = 2)
points(x=1:41, y=mu_f, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_f[2,i],pi89_f[1,i]))
}

plot(NULL, xlim=c(1,41), ylim=c(0,35), xlab="Grid square", 
     xaxt = "n",
     ylab="Relative volume (median +/- 89% C.I.", main="Non-fruiting")
axis(side = 1, at = 1:41, labels = rownames(y_n_s), las = 2)
points(x=1:41, y=mu_n, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_n[2,i],pi89_n[1,i]))
}

plot(NULL, xlim=c(1,41), ylim=c(0,35), xlab="Grid square", 
     xaxt = "n",
     ylab="Relative volume (median +/- 89% C.I.", main="Total")
axis(side = 1, at = 1:41, labels = rownames(y_n_s), las = 2)
points(x=1:41, y=mu_t, pch=16)
for(i in 1:41){
  lines(x = rep(i,2), y = c(pi89_t[2,i],pi89_t[1,i]))
}

# Get median densities as dataframes
df_mu_f <- as.data.frame(cbind(rownames(y_n_s), mu_f))
colnames(df_mu_f) <- c("grid_square", "grid_fruiting_volume")
df_mu_f$grid_square <- as.factor(df_mu_f$grid_square)

df_mu_n <- as.data.frame(cbind(rownames(y_n_s), mu_n))
colnames(df_mu_n) <- c("grid_square", "grid_non_fruiting_volume")
df_mu_n$grid_square <- as.factor(df_mu_n$grid_square)

df_mu_t <- as.data.frame(cbind(rownames(y_n_s), mu_t))
colnames(df_mu_t) <- c("grid_square", "grid_total_volume")
df_mu_t$grid_square <- as.factor(df_mu_t$grid_square)

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
save(df_mu_f, file = "grid_square_fruiting.Rdata")
save(df_mu_n, file = "grid_square_non_fruiting.Rdata")
save(df_mu_t, file = "grid_square_total.Rdata")
