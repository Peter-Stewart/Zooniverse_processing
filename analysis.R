# Load packages
library(rethinking)
library(dplyr)
library(tidyr)

# Load data
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
site_data <- read.csv("Cameras_site_data_main.csv", header = TRUE)
site_data <- site_data %>% filter(Site_ID %in% sitedays$Site)

opuntia_data <- read.csv("Cameras_opuntia_data_main.csv", header = TRUE)

dist_river <- read.csv("distance_to_river.csv", header = TRUE)
dist_road <- read.csv("distance_to_road.csv", header = TRUE)

# Merge distance to river/road into the main sites dataframe
dist_river <- dist_river %>% select(Site_ID, HubDist) %>% rename(dist_river = HubDist)
dist_road <- dist_road %>% select(Site_ID, HubDist) %>% rename(dist_road = HubDist)

site_data <- merge(site_data, dist_river, by="Site_ID")
site_data <- merge(site_data, dist_road, by="Site_ID")

# Calculate amount of ripe fruit at each site 
fruit_sites <- opuntia_data %>% select(Site_ID, Ripe_fruits) %>% 
  group_by(Site_ID) %>% 
  summarise(total_ripe = sum(Ripe_fruits))
fruit_sites[is.na(fruit_sites)] <- 0
fruit_sites <- fruit_sites %>% filter(Site_ID %in% sitedays$Site)

site_data <- merge(site_data, fruit_sites, by="Site_ID")

# Calculate Opuntia volume
opuntia_max_heights <- opuntia_data %>% select(Site_ID, Max_height) %>%
  group_by(Site_ID) %>%
  summarise(Max_height_site = max(Max_height))
opuntia_max_heights[is.na(opuntia_max_heights)] <- 0
opuntia_max_heights <- opuntia_max_heights %>% filter(Site_ID %in% sitedays$Site)

site_data <- merge(site_data, opuntia_max_heights, by="Site_ID")

site_data$opuntia_total_cover <- (site_data$Opuntia_stricta_FOV + site_data$Opuntia_other_FOV +
                                    (3*site_data$Opuntia_stricta_area) + (3*site_data$Opuntia_other_area))/8
site_data$opuntia_volume <- site_data$opuntia_total_cover * site_data$Max_height_site

# Average % covers across FOV and area
site_data$grass_total <- (site_data$Grass_FOV + (3*site_data$Grass_area)) / 4
site_data$forb_total <- (site_data$Forb_FOV + (3*site_data$Forb_area)) / 4
site_data$shrub_total <- (site_data$Shrub_FOV + (3*site_data$Shrub_area)) / 4
site_data$succulent_total <- (site_data$Succulent_FOV + (3*site_data$Succulent_area)) / 4
site_data$tree_total <- (site_data$Tree_FOV + (3*site_data$Tree_area)) / 4

site_data$grass_total[is.na(site_data$grass_total)] <- 0
site_data$forb_total[is.na(site_data$forb_total)] <- 0
site_data$shrub_total[is.na(site_data$shrub_total)] <- 0
site_data$succulent_total[is.na(site_data$succulent_total)] <- 0
site_data$tree_total[is.na(site_data$tree_total)] <- 0


# Calculate how often site is used by livestock (inc. camels)
livestock <- detmats$livestock[,-1] + detmats$camel[,-1]
livestock_proportion <- matrix(NA, ncol = 1, nrow = nrow(livestock))
for(i in 1:nrow(livestock)){
  livestock_proportion[i] <- sum(livestock[i,], na.rm = TRUE) / (ncol(livestock) - sum(is.na(livestock[i,])))
}
livestock_proportion <- as.data.frame(livestock_proportion)
livestock_proportion$Site_ID <- detmats$livestock[,1]
livestock_proportion$Site_ID <- gsub("Site_","", livestock_proportion$Site_ID)
livestock_proportion$Site_ID <- as.integer(livestock_proportion$Site_ID)
colnames(livestock_proportion) <- c("livestock_proportion", "Site_ID")

site_data <- merge(site_data, livestock_proportion, by="Site_ID")

# Generate distance matrix
generate_distance_matrix <- function(df, center = FALSE, rescale = FALSE, sites_as_days = FALSE, jitter = TRUE, log = FALSE, logbase = 15, squared = FALSE){
  
  # Select columns which contain "long" or "lat" in their name
  coords <- df %>% select(contains("long") | contains("lat"))
  
  # Return error if more than two columns
  if(length(coords) > 2){
    stop("More than two coordinate columns - make sure dataframe contains lat and long coordinates only")
  }
  
  if(sites_as_days == TRUE){
    if(!exists("sitedays")){
      stop("Please load the sitedays dataframe")
    }
    
    sites <- df %>% select(contains("site") & !contains("description"))
    colnames(sites) <- c("Site")
    coords <- cbind(sites, coords)
    coords <- merge(coords, sitedays, by = "Site")
    ff <- coords %>% uncount(Days)
    ff$Site <- ifelse(ff$Site < 10, paste0("0", ff$Site), ff$Site)
    ff$Site <- paste0("Site_",ff$Site)
    gg <- rep(NA, nrow(ff))
    gg[1] <- 1
    ff <- cbind(ff,gg)
    for(i in 2:nrow(ff)){
      if(ff$Site[i]==ff$Site[i-1])
        ff$gg[i] <-  ff$gg[i-1] + 1L 
      else
        ff$gg[i] <- 1L
    }
    ff$site_day <- paste0(ff$Site,"_",ff$gg)
    coords <- ff %>% select(-Site, -gg)
    rownames(coords) <- coords$site_day
    coords <- coords %>% select(-site_day)
  }
  
  # If center is true, center around zero
  if(center == TRUE & rescale == FALSE){
    coords$GPS_long <- scale(coords$GPS_long, scale = FALSE)
    coords$GPS_lat <- scale(coords$GPS_lat, scale = FALSE)
  }
  
  # If rescale is true, rescale using max lat for both lat and long to avoid warping distances
  if(center == FALSE & rescale == TRUE){
    coords$GPS_long <- scale(coords$GPS_long, center = FALSE, scale = FALSE) / max(coords$GPS_lat) *10
    coords$GPS_lat <- scale(coords$GPS_lat, center = FALSE, scale = FALSE) / max(coords$GPS_lat) *10
  }
  
  # If center and rescale are  true, center to zero and rescale using max lat for both lat and long to avoid warping distances
  if(center == TRUE & rescale == TRUE){
    coords$GPS_long <- scale(coords$GPS_long, scale = FALSE) / max(coords$GPS_lat) *10
    coords$GPS_lat <- scale(coords$GPS_lat, scale = FALSE) / max(coords$GPS_lat) *10
  }
  
  coords <- coords %>% select(GPS_long, GPS_lat)
  colnames(coords) <- c("x", "y")
  
  attr(coords$x, "scaled:center") <- NULL
  attr(coords$y, "scaled:center") <- NULL
  
  # Calculate distance matrix
  dmat <- dist(coords, diag=T, upper=T)
  dmat <- as.matrix(dmat)
  
  # Optionally add jitter to off-diagonal zeros to prevent numerical underflow
  if(jitter == TRUE){
    warning("Adding jitter to off-diagonal zeros to prevent numerical underflow in Gaussian Process. Set jitter = FALSE to disable.")
    dmat <- ifelse(dmat==0, dmat+1.001, dmat)
    diag(dmat) <- 0
  }
  
  # Optionally log-transform
  if(log == TRUE){
    dmat <- ifelse(dmat > 0, log(dmat, base = logbase), dmat)
  }
  
  # Optionally return squared distances
  if(squared == TRUE){
    dmat2 <- dmat^2
    return(dmat2)
  }
  else{
    return(dmat)
  } 
}

dmat <- generate_distance_matrix(site_data, log = TRUE)

# simulate detection covariate for now
w <- matrix(NA, nrow=nrow(dmat), ncol=max(sitedays$Days))
for(i in 1:nrow(dmat)){
  for(k in 1:sitedays$Days[i]){
    w[i,k] <- rnorm(1,0,1)
  }
}
w[is.na(w)] <- -9999

dd <- as.matrix(detmats$elephant[,-1])
dd[is.na(dd)] <- -9999
mode(dd) <- "integer"

dlist <- list(
  # Number of sites and visits
  nsites = as.integer(nrow(dmat)),
  N_maxvisits = as.integer(max(sitedays$Days)),
  V = as.integer(sitedays$Days),
  # Observed data
  y = dd,
  # Occupancy covariates
  opuntia_vol = standardize(site_data$opuntia_volume),
  fruit = standardize(site_data$total_ripe),
  d_water = standardize(site_data$dist_river),
  d_road = standardize(site_data$dist_road),
  livestock = standardize(site_data$livestock_proportion),
  grass = standardize(site_data$grass_total),
  forb = standardize(site_data$forb_total),
  shrub = standardize(site_data$shrub_total),
  succulent = standardize(site_data$succulent_total),
  tree = standardize(site_data$tree_total),
  # Detection covariates
  w = w,
  # Distance matrix
  dmat = dmat
)

m1_nc <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/occupancy_models/ch3/occ_gp_nc_v1.stan",
               data = dlist,
               chains = 4,
               cores = 4,
               warmup = 2500,
               iter = 3500,
               seed = 33)

dashboard(m1_nc)
precis(m1_nc)
post <- extract.samples(m1_nc)

dmat2 <- dmat^2

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

par(mfrow=c(1,1))
plot(GPS_lat~GPS_long, data=site_data, col=rangi2, pch=16, main="Correlation inferred from model")

for(i in 1:nrow(dmat)){
  for(j in 1:ncol(dmat)){
    if(i < j){
      lines(x = c(site_data$GPS_long[i], site_data$GPS_long[j]), 
            y = c(site_data$GPS_lat[i], site_data$GPS_lat[j]),
            lwd=2, col=col.alpha("black", Rho[i,j]^2))
    }
  }
}


psi_mu <- apply(post$psi,2,mean)
psi_PI89 <- apply(post$psi, 2, HPDI, prob=0.89)

par(mfrow=c(1,1))
plot(NULL, xlim=c(0,84), ylim=c(0,1), xlab="Site", ylab="Psi (mean +/- 89%CI", main=expression(psi))
abline(h=0.5, lty=2)
xseq <- 1:84
points(x = xseq, y = psi_mu, pch=16, col=rangi2)
for(i in 1:length(xseq)){
  lines(x = rep(xseq[i],2), y = c(psi_PI89[1,i], psi_PI89[2,i]), col=rangi2)
}



##### 
samp <- 1e4
rho2_prior <- rexp(samp, 2)
eta2_prior <- rexp(samp, 0.7)

x_seq <- seq(0,4,0.01)
priorcov <- sapply(x_seq, function(x) eta2_prior * exp(-rho2_prior * x^2))
priorcov_mu <- apply(priorcov, 2, median)
priorcov_95CI <- apply(priorcov, 2, HPDI, prob=0.95)
priorcov_89CI <- apply(priorcov, 2, HPDI, prob=0.89)
priorcov_80CI <- apply(priorcov, 2, HPDI, prob=0.80)
priorcov_70CI <- apply(priorcov, 2, HPDI, prob=0.70)
priorcov_60CI <- apply(priorcov, 2, HPDI, prob=0.60)
priorcov_50CI <- apply(priorcov, 2, HPDI, prob=0.50)

par(mfrow=c(1,1))
plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,4), ylim=c(0,10), main="Prior w/ Compatability Intervals")
lines(x_seq, priorcov_mu, lwd=2)
shade(priorcov_95CI, x_seq)
shade(priorcov_89CI, x_seq)
shade(priorcov_80CI, x_seq)
shade(priorcov_70CI, x_seq)
shade(priorcov_60CI, x_seq)
shade(priorcov_50CI, x_seq)

x_seq <- seq(0,4,0.01)
pmcov <- sapply(x_seq, function(x) post$etasq * exp(-post$rhosq * x^2))
pmcov_mu <- apply(pmcov, 2, median)
pmcov_95CI <- apply(pmcov, 2, HPDI, prob=0.95)
pmcov_89CI <- apply(pmcov, 2, HPDI, prob=0.89)
pmcov_80CI <- apply(pmcov, 2, HPDI, prob=0.80)
pmcov_70CI <- apply(pmcov, 2, HPDI, prob=0.70)
pmcov_60CI <- apply(pmcov, 2, HPDI, prob=0.60)
pmcov_50CI <- apply(pmcov, 2, HPDI, prob=0.50)

par(mfrow=c(1,1))
plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,4), ylim=c(0,10), main="Posterior w/ Compatability Intervals")
lines(x_seq, pmcov_mu, lwd=2)
shade(pmcov_95CI, x_seq)
shade(pmcov_89CI, x_seq)
shade(pmcov_80CI, x_seq)
shade(pmcov_70CI, x_seq)
shade(pmcov_60CI, x_seq)
shade(pmcov_50CI, x_seq)
curve(eta2 * exp(-rho2 * x^2), add=TRUE, lwd=3, lty=2)
