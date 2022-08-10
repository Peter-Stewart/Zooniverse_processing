# Load packages
library(rethinking)
library(dplyr)
library(tidyr)
library(bayesplot)
library(ggdist)
library(viridis)


# Load data
setwd("C:/temp/Zooniverse/June22")
sitedays <- get(load("sitedays.Rdata"))
detmats <- get(load("detmats.Rdata"))

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
site_data <- read.csv("Cameras_site_data_main.csv", header = TRUE)
site_data <- site_data %>% filter(Site_ID %in% sitedays$Site)

opuntia_data <- read.csv("Cameras_opuntia_data_main.csv", header = TRUE)

tree_data <- read.csv("Cameras_tree_data_main.csv", header = TRUE)

dist_river <- read.csv("distance_to_river.csv", header = TRUE)
dist_road <- read.csv("distance_to_road.csv", header = TRUE)

df_mu_f <- get(load("grid_square_fruiting.Rdata"))
df_mu_n <- get(load("grid_square_non_fruiting.Rdata"))
df_mu_t <- get(load("grid_square_total.Rdata"))

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
site_data$total_ripe_bin <- ifelse(site_data$total_ripe > 0, 1, 0)

# Calculate Opuntia volume
opuntia_max_heights <- opuntia_data %>% select(Site_ID, Max_height) %>%
  group_by(Site_ID) %>%
  summarise(Max_height_site = max(Max_height))
opuntia_max_heights[is.na(opuntia_max_heights)] <- 0
opuntia_max_heights <- opuntia_max_heights %>% filter(Site_ID %in% sitedays$Site)

opuntia_median_heights <- opuntia_data %>% select(Site_ID, Max_height) %>%
  group_by(Site_ID) %>%
  summarise(Median_height_site = median(Max_height))
opuntia_median_heights[is.na(opuntia_median_heights)] <- 0
opuntia_median_heights <- opuntia_median_heights %>% filter(Site_ID %in% sitedays$Site)

site_data <- merge(site_data, opuntia_max_heights, by="Site_ID")
site_data <- merge(site_data, opuntia_median_heights, by="Site_ID")

site_data$opuntia_total_cover <- (site_data$Opuntia_stricta_FOV + site_data$Opuntia_other_FOV +
                                    (3*site_data$Opuntia_stricta_area) + (3*site_data$Opuntia_other_area))/8
site_data$opuntia_volume <- site_data$opuntia_total_cover * site_data$Median_height_site

# Count trees at each site
tree_data <- tree_data %>% filter(!grepl("Fallen", Species)) %>% filter(!grepl("Dead", Species)) # Not counting fallen or dead trees
tree_sites <- tree_data %>% select(Site_ID, FOV, Area) %>%
  group_by(Site_ID) %>%
  summarise(n_trees = FOV+Area) %>%
  group_by(Site_ID) %>%
  summarise(n_trees = sum(n_trees)) 
tree_sites <- tree_sites %>% filter(Site_ID %in% sitedays$Site)

site_data <- merge(site_data, tree_sites, by="Site_ID")


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

# Merge in grid square-level Opuntia densities
site_data$grid_square <- as.factor(site_data$Grid_square)
site_data <- merge(site_data, df_mu_t, by="grid_square", all.x = TRUE)

# Ensure that site_data is ordered by site_ID
site_data <- site_data[order(site_data$Site_ID),]

# Generate distance matrix
generate_distance_matrix <- function(df, rescale = FALSE, rescale_constant = 1, sites_as_days = FALSE, jitter = TRUE, jitter_amount = 1.001, log = FALSE, logbase = 15, squared = FALSE){
  
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
  

  # Calculate distance matrix
  dmat <- dist(coords, diag=T, upper=T)
  dmat <- as.matrix(dmat)
  
  # Optionally rescale by dividing each distance by some constant (default = 1, i.e. you need to define a constant or nothing will happen!)
  if(rescale == TRUE){
    if(rescale_constant == 1){
      warning("Please define a constant to rescale distances, using the rescale_constant option.")
    }
    dmat <- dmat / rescale_constant
  }
  
  # Optionally add jitter to off-diagonal zeros to prevent numerical underflow
  if(jitter == TRUE){
    warning("Adding jitter to off-diagonal zeros to prevent numerical underflow in Gaussian Process. Set jitter = FALSE to disable.")
    dmat <- ifelse(dmat==0, dmat+jitter_amount, dmat)
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

dmat <- generate_distance_matrix(site_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

# Run models #### 
setwd("C:/temp/ch3_post")

key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "giraffe",
            "hyenaspotted")

indexes <- list()
for(i in 1:length(detmats)){
    if(names(detmats[i]) %in% key_sp){
      indexes[i] <- i
    }else{
      indexes[i] <- NULL}
}
indexes <- do.call(rbind, indexes)

# List the models which will be run
#model_list <- c("direct_effects",
#                "total_effect_no_veg_path",
#                "total_effect_veg_path")

#model_list <- "direct_effects"
#model_list <- "total_effect_no_veg_path"
model_list <- "total_effect_veg_path"


# Parameters to control the models
n_chains <- 4 # Number of chains
n_cores <- 4 # Number of computer cores
n_warmup <- 3000 # Number of warmup iterations per chain
n_iter <- 4000 # Total number of iterations (warmup + sample) per chain
  
# Temporarily suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# For each model 
# loop through all key species, run model, save results and diagnostics
for(m in 1:length(model_list)){
  
  setwd(paste0("C:/temp/ch3_post/",model_list[m]))
  
  for(sp in 1:length(key_sp)){
  
  # Select data for the species
  dd <- detmats[indexes[sp,]]
  dd <- dd[[1]]
  dd <- as.matrix(dd[,-1])
  dd[is.na(dd)] <- -9999
  mode(dd) <- "integer"
  
  # Prepare data list for Stan
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
    tree = standardize(site_data$n_trees),
    # Detection covariates
    
    # Distance matrix
    dmat = dmat
  )
  
  # Run the model
  m1_nc <- cstan(file = paste0("C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/occupancy_models/ch3/",model_list[m],".stan"),
                 data = dlist,
                 chains = n_chains,
                 cores = n_cores,
                 warmup = n_warmup,
                 iter = n_iter)
  
  # Save diagnostic plots
  png(file = paste0(key_sp[sp],"_",model_list[m],"_diagnostics.png"), width = 804, height = 500, units = "px")
  dashboard(m1_nc)
  dev.off()
  
  # Save posterior samples
  post <- extract.samples(m1_nc)
  save(post, file = paste0(key_sp[sp],"_",model_list[m],"_post.Rdata"))
  
  # Parameters to save in traceplots and trankplots
  p <- names(post)[grep("beta", names(post))] # Beta parameters
  p2 <- c("alphadet", "etasq", "rhosq", "k_bar") # Other parameters
  
  # Save traceplots for key parameters
  png(file = paste0(key_sp[sp],"_",model_list[m],"_traceplots.png"), width = 804, height = 500, units = "px")
  p1 <- rstan::traceplot(m1_nc, pars=c(p, p2), inc_warmup = TRUE)
  print(p1)
  dev.off()
  
  # Save trankplots for key parameters
  png(file = paste0(key_sp[sp],"_",model_list[m],"_trankplots.png"), width = 804, height = 500, units = "px")
  trankplot(m1_nc, pars=c(p, p2))
  dev.off()
  
  # Save hist of centred marginal energy distribution and first-differenced distribution overlaid
  color_scheme_set("darkgray") # Set colour scheme for Bayesplot 
  np <- nuts_params(m1_nc) # Extract NUTS parameters
  png(file = paste0(key_sp[sp],"_",model_list[m],"_HMC_energy.png"), width = 804, height = 500, units = "px")
  p2 <- mcmc_nuts_energy(np)
  print(p2)
  dev.off()
  
  
  # Clean up between iterations
  rm(post)
  rm(np)
  rm(p1)
  rm(p2)
  rm(m1_nc)
  rm(dlist)
  rm(dd)
  gc()
  
}
}
# Re-enable warnings
options(warn = oldw)

# Load posterior samples

# Direct effects ####
setwd("C:/temp/ch3_post/direct_effects")
post_direct_baboon <- get(load("baboon_direct_effects_post.Rdata")); rm(post); gc()
post_direct_elephant <- get(load("elephant_direct_effects_post.Rdata")); rm(post); gc()
post_direct_vervetmonkey <- get(load("vervetmonkey_direct_effects_post.Rdata")); rm(post); gc()
post_direct_zebragrevys <- get(load("zebragrevys_direct_effects_post.Rdata")); rm(post); gc()
post_direct_impala <- get(load("impala_direct_effects_post.Rdata")); rm(post); gc()
post_direct_giraffe <- get(load("giraffe_direct_effects_post.Rdata")); rm(post); gc()
post_direct_hyenaspotted <- get(load("hyenaspotted_direct_effects_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_direct_baboon <- as.data.frame(cbind(post_direct_baboon$beta_opuntia, post_direct_baboon$beta_fruit, post_direct_baboon$k_bar))
df_direct_elephant <- as.data.frame(cbind(post_direct_elephant$beta_opuntia, post_direct_elephant$beta_fruit, post_direct_elephant$k_bar))
df_direct_vervetmonkey <- as.data.frame(cbind(post_direct_vervetmonkey$beta_opuntia, post_direct_vervetmonkey$beta_fruit, post_direct_vervetmonkey$k_bar))
df_direct_zebragrevys <- as.data.frame(cbind(post_direct_zebragrevys$beta_opuntia, post_direct_zebragrevys$beta_fruit, post_direct_zebragrevys$k_bar))
df_direct_impala <- as.data.frame(cbind(post_direct_impala$beta_opuntia, post_direct_impala$beta_fruit, post_direct_impala$k_bar))
df_direct_giraffe <- as.data.frame(cbind(post_direct_giraffe$beta_opuntia, post_direct_giraffe$beta_fruit, post_direct_giraffe$k_bar))
df_direct_hyenaspotted <- as.data.frame(cbind(post_direct_hyenaspotted$beta_opuntia, post_direct_hyenaspotted$beta_fruit, post_direct_hyenaspotted$k_bar))

# Remove full posterior distributions to save memory
rm(post_direct_baboon)
rm(post_direct_elephant)
rm(post_direct_vervetmonkey)
rm(post_direct_zebragrevys)
rm(post_direct_impala)
rm(post_direct_giraffe)
rm(post_direct_hyenaspotted)
gc()

# Add species column
df_direct_baboon$species <- as.factor("baboon")
df_direct_elephant$species <- as.factor("elephant")
df_direct_vervetmonkey$species <- as.factor("vervetmonkey")
df_direct_zebragrevys$species <- as.factor("zebragrevys")
df_direct_impala$species <- as.factor("impala")
df_direct_giraffe$species <- as.factor("giraffe")
df_direct_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_direct_baboon) <- c("beta_opuntia", "beta_fruit","k_bar1", "species")
colnames(df_direct_elephant) <- c("beta_opuntia", "beta_fruit","k_bar1", "species")
colnames(df_direct_vervetmonkey) <- c("beta_opuntia", "beta_fruit","k_bar1", "species")
colnames(df_direct_zebragrevys) <-c("beta_opuntia", "beta_fruit","k_bar1", "species")
colnames(df_direct_impala) <- c("beta_opuntia", "beta_fruit","k_bar1", "species")
colnames(df_direct_giraffe) <- c("beta_opuntia", "beta_fruit","k_bar1", "species")
colnames(df_direct_hyenaspotted) <- c("beta_opuntia", "beta_fruit","k_bar1", "species")

# Bind into one big dataframe
beta_direct_df <- rbind(df_direct_baboon, 
                        df_direct_elephant,
                        df_direct_vervetmonkey,
                        df_direct_zebragrevys,
                        df_direct_impala,
                        df_direct_giraffe,
                        df_direct_hyenaspotted)

# Remove the separate df's to save memory
rm(df_direct_baboon)
rm(df_direct_elephant)
rm(df_direct_giraffe)
rm(df_direct_hyenaspotted)
rm(df_direct_impala)
rm(df_direct_vervetmonkey)
rm(df_direct_zebragrevys)
gc()

# Total effects without vegetation pathway ####
setwd("C:/temp/ch3_post/total_effect_no_veg_path")
post_total_no_veg_path_baboon <- get(load("baboon_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_elephant <- get(load("elephant_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_vervetmonkey <- get(load("vervetmonkey_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_zebragrevys <- get(load("zebragrevys_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_impala <- get(load("impala_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_giraffe <- get(load("giraffe_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_hyenaspotted <- get(load("hyenaspotted_total_effect_no_veg_path_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_total_no_veg_path_baboon <- as.data.frame(cbind(post_total_no_veg_path_baboon$beta_opuntia, post_total_no_veg_path_baboon$beta_fruit, post_total_no_veg_path_baboon$k_bar))
df_total_no_veg_path_elephant <- as.data.frame(cbind(post_total_no_veg_path_elephant$beta_opuntia, post_total_no_veg_path_elephant$beta_fruit, post_total_no_veg_path_elephant$k_bar))
df_total_no_veg_path_vervetmonkey <- as.data.frame(cbind(post_total_no_veg_path_vervetmonkey$beta_opuntia, post_total_no_veg_path_vervetmonkey$beta_fruit, post_total_no_veg_path_vervetmonkey$k_bar))
df_total_no_veg_path_zebragrevys <- as.data.frame(cbind(post_total_no_veg_path_zebragrevys$beta_opuntia, post_total_no_veg_path_zebragrevys$beta_fruit, post_total_no_veg_path_zebragrevys$k_bar))
df_total_no_veg_path_impala <- as.data.frame(cbind(post_total_no_veg_path_impala$beta_opuntia, post_total_no_veg_path_impala$beta_fruit, post_total_no_veg_path_impala$k_bar))
df_total_no_veg_path_giraffe <- as.data.frame(cbind(post_total_no_veg_path_giraffe$beta_opuntia, post_total_no_veg_path_giraffe$beta_fruit, post_total_no_veg_path_giraffe$k_bar))
df_total_no_veg_path_hyenaspotted <- as.data.frame(cbind(post_total_no_veg_path_hyenaspotted$beta_opuntia, post_total_no_veg_path_hyenaspotted$beta_fruit, post_total_no_veg_path_hyenaspotted$k_bar))

# Remove full posterior distributions to save memory
rm(post_total_no_veg_path_baboon)
rm(post_total_no_veg_path_elephant)
rm(post_total_no_veg_path_vervetmonkey)
rm(post_total_no_veg_path_zebragrevys)
rm(post_total_no_veg_path_impala)
rm(post_total_no_veg_path_giraffe)
rm(post_total_no_veg_path_hyenaspotted)
gc()

# Add species column
df_total_no_veg_path_baboon$species <- as.factor("baboon")
df_total_no_veg_path_elephant$species <- as.factor("elephant")
df_total_no_veg_path_vervetmonkey$species <- as.factor("vervetmonkey")
df_total_no_veg_path_zebragrevys$species <- as.factor("zebragrevys")
df_total_no_veg_path_impala$species <- as.factor("impala")
df_total_no_veg_path_giraffe$species <- as.factor("giraffe")
df_total_no_veg_path_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_total_no_veg_path_baboon) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_elephant) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_vervetmonkey) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_zebragrevys) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_impala) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_giraffe) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_hyenaspotted) <- c("beta_total1", "k_bar2", "species")

# Bind into one big dataframe
beta_total_no_veg_path_df <- rbind(df_total_no_veg_path_baboon, 
                                   df_total_no_veg_path_elephant,
                                   df_total_no_veg_path_vervetmonkey,
                                   df_total_no_veg_path_zebragrevys,
                                   df_total_no_veg_path_impala,
                                   df_total_no_veg_path_giraffe,
                                   df_total_no_veg_path_hyenaspotted)

# Remove the separate df's to save memory
rm(df_total_no_veg_path_baboon)
rm(df_total_no_veg_path_elephant)
rm(df_total_no_veg_path_giraffe)
rm(df_total_no_veg_path_hyenaspotted)
rm(df_total_no_veg_path_impala)
rm(df_total_no_veg_path_vervetmonkey)
rm(df_total_no_veg_path_zebragrevys)
gc()

# Total effects with vegetation pathway ####
setwd("C:/temp/ch3_post/total_effect_veg_path")
post_total_veg_path_baboon <- get(load("baboon_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_elephant <- get(load("elephant_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_vervetmonkey <- get(load("vervetmonkey_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_zebragrevys <- get(load("zebragrevys_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_impala <- get(load("impala_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_giraffe <- get(load("giraffe_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_hyenaspotted <- get(load("hyenaspotted_total_effect_veg_path_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_total_veg_path_baboon <- as.data.frame(cbind(post_total_veg_path_baboon$beta_opuntia, post_total_veg_path_baboon$beta_fruit, post_total_veg_path_baboon$k_bar))
df_total_veg_path_elephant <- as.data.frame(cbind(post_total_veg_path_elephant$beta_opuntia, post_total_veg_path_elephant$beta_fruit, post_total_veg_path_elephant$k_bar))
df_total_veg_path_vervetmonkey <- as.data.frame(cbind(post_total_veg_path_vervetmonkey$beta_opuntia, post_total_veg_path_vervetmonkey$beta_fruit, post_total_veg_path_vervetmonkey$k_bar))
df_total_veg_path_zebragrevys <- as.data.frame(cbind(post_total_veg_path_zebragrevys$beta_opuntia, post_total_veg_path_zebragrevys$beta_fruit, post_total_veg_path_zebragrevys$k_bar))
df_total_veg_path_impala <- as.data.frame(cbind(post_total_veg_path_impala$beta_opuntia, post_total_veg_path_impala$beta_fruit, post_total_veg_path_impala$k_bar))
df_total_veg_path_giraffe <- as.data.frame(cbind(post_total_veg_path_giraffe$beta_opuntia, post_total_veg_path_giraffe$beta_fruit, post_total_veg_path_giraffe$k_bar))
df_total_veg_path_hyenaspotted <- as.data.frame(cbind(post_total_veg_path_hyenaspotted$beta_opuntia, post_total_veg_path_hyenaspotted$beta_fruit, post_total_veg_path_hyenaspotted$k_bar))

# Remove full posterior distributions to save memory
rm(post_total_veg_path_baboon)
rm(post_total_veg_path_elephant)
rm(post_total_veg_path_vervetmonkey)
rm(post_total_veg_path_zebragrevys)
rm(post_total_veg_path_impala)
rm(post_total_veg_path_giraffe)
rm(post_total_veg_path_hyenaspotted)
gc()

# Add species column
df_total_veg_path_baboon$species <- as.factor("baboon")
df_total_veg_path_elephant$species <- as.factor("elephant")
df_total_veg_path_vervetmonkey$species <- as.factor("vervetmonkey")
df_total_veg_path_zebragrevys$species <- as.factor("zebragrevys")
df_total_veg_path_impala$species <- as.factor("impala")
df_total_veg_path_giraffe$species <- as.factor("giraffe")
df_total_veg_path_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_total_veg_path_baboon) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_elephant) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_vervetmonkey) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_zebragrevys) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_impala) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_giraffe) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_hyenaspotted) <- c("beta_total2", "k_bar3", "species")

# Bind into one big dataframe
beta_total_veg_path_df <- rbind(df_total_veg_path_baboon, 
                                df_total_veg_path_elephant,
                                df_total_veg_path_vervetmonkey,
                                df_total_veg_path_zebragrevys,
                                df_total_veg_path_impala,
                                df_total_veg_path_giraffe,
                                df_total_veg_path_hyenaspotted)

# Remove the separate df's to save memory
rm(df_total_veg_path_baboon)
rm(df_total_veg_path_elephant)
rm(df_total_veg_path_giraffe)
rm(df_total_veg_path_hyenaspotted)
rm(df_total_veg_path_impala)
rm(df_total_veg_path_vervetmonkey)
rm(df_total_veg_path_zebragrevys)
gc()


# Combine into one dataframe
beta_all_df <- cbind(beta_direct_df, beta_total_no_veg_path_df[,-3], beta_total_veg_path_df[,-3])


# Marginal effect plots ####
key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "giraffe",
            "hyenaspotted")

species_names <- c("Olive baboon",
                   "Elephant",
                   "Vervet monkey",
                   "Grevy's zebra",
                   "Impala", 
                   "Giraffe",
                   "Spotted hyena")

# Colours for shading CI's for each species
species_colours <- viridis(7)
colouralpha <- 0.4


par(mfrow=c(length(key_sp),4), mar=c(3, 2, 2, 2))
xseq <- seq(-2, 2, by = 0.01)

# Loop over each species
for(i in 1:length(key_sp)){
  s <- beta_all_df %>% filter(species == key_sp[i])
  
  # Calculate marginal effects using k_bar and the beta parameters
  p1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p3 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p4 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p1[,x] <- inv_logit(s$k_bar1 + s$beta_opuntia*xseq[x])
    p2[,x] <- inv_logit(s$k_bar1 + s$beta_fruit*xseq[x])
    p3[,x] <- inv_logit(s$k_bar2 + s$beta_total1*xseq[x])
    p4[,x] <- inv_logit(s$k_bar3 + s$beta_total2*xseq[x])
  }
  
  # Calculate mean and CI's
  mu1 <- apply(p1, 2, mean)
  PI95_1 <- apply(p1, 2, HPDI, prob=0.96)
  PI89_1 <- apply(p1, 2, HPDI, prob=0.89)
  PI80_1 <- apply(p1, 2, HPDI, prob=0.80)
  PI70_1 <- apply(p1, 2, HPDI, prob=0.70)
  PI60_1 <- apply(p1, 2, HPDI, prob=0.60)
  PI50_1 <- apply(p1, 2, HPDI, prob=0.50)
  PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p2, 2, mean)
  PI95_2 <- apply(p2, 2, HPDI, prob=0.96)
  PI89_2 <- apply(p2, 2, HPDI, prob=0.89)
  PI80_2 <- apply(p2, 2, HPDI, prob=0.80)
  PI70_2 <- apply(p2, 2, HPDI, prob=0.70)
  PI60_2 <- apply(p2, 2, HPDI, prob=0.60)
  PI50_2 <- apply(p2, 2, HPDI, prob=0.50)
  PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  mu3 <- apply(p3, 2, mean)
  PI95_3 <- apply(p3, 2, HPDI, prob=0.96)
  PI89_3 <- apply(p3, 2, HPDI, prob=0.89)
  PI80_3 <- apply(p3, 2, HPDI, prob=0.80)
  PI70_3 <- apply(p3, 2, HPDI, prob=0.70)
  PI60_3 <- apply(p3, 2, HPDI, prob=0.60)
  PI50_3 <- apply(p3, 2, HPDI, prob=0.50)
  PI_all_3 <- rbind(PI95_3, PI89_3, PI80_3, PI70_3, PI60_3, PI50_3)
  
  mu4 <- apply(p4, 2, mean)
  PI95_4 <- apply(p4, 2, HPDI, prob=0.96)
  PI89_4 <- apply(p4, 2, HPDI, prob=0.89)
  PI80_4 <- apply(p4, 2, HPDI, prob=0.80)
  PI70_4 <- apply(p4, 2, HPDI, prob=0.70)
  PI60_4 <- apply(p4, 2, HPDI, prob=0.60)
  PI50_4 <- apply(p4, 2, HPDI, prob=0.50)
  PI_all_4 <- rbind(PI95_4, PI89_4, PI80_4, PI70_4, PI60_4, PI50_4)
  
  # Make the plots
  plot(NULL, xlim=c(-2,2), ylim=c(0,1), main=paste0(species_names[i],", Structural Effect"), ylab = expression(psi), xlab="Opuntia volume (standardised)")
  shade(PI95_1, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI80_1, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI70_1, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI60_1, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI50_1, xseq, col=col.alpha(species_colours[i], colouralpha))
  abline(v=0, lty=2)
  abline(h=0.5, lty=2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  plot(NULL, xlim=c(-2,2), ylim=c(0,1), main=paste0(species_names[i],", Fruit Effect"), ylab = expression(psi), xlab="Number of ripe fruits (standardised)")
  shade(PI95_2, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI80_2, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI70_2, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI60_2, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI50_2, xseq, col=col.alpha(species_colours[i], colouralpha))
  abline(v=0, lty=2)
  abline(h=0.5, lty=2)
  points(x = xseq, y = mu2, type="l", lwd=2)
  
  plot(NULL, xlim=c(-2,2), ylim=c(0,1), main=paste0(species_names[i],", Total Effect (no veg. path)"), ylab = expression(psi), xlab="Opuntia volume (standardised)")
  shade(PI95_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI89_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI80_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI70_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI60_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI50_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  abline(v=0, lty=2)
  abline(h=0.5, lty=2)
  points(x = xseq, y = mu3, type="l", lwd=2)
  
  plot(NULL, xlim=c(-2,2), ylim=c(0,1), main=paste0(species_names[i],", Total Effect (veg. path)"), ylab = expression(psi), xlab="Opuntia volume (standardised)")
  shade(PI95_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI89_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI80_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI70_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI60_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI50_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  abline(v=0, lty=2)
  abline(h=0.5, lty=2)
  points(x = xseq, y = mu4, type="l", lwd=2)
}


# Other Plots ####
key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "giraffe",
            "hyenaspotted")

species_names <- c("Olive baboon",
                   "Elephant",
                   "Vervet monkey",
                   "Grevy's zebra",
                   "Impala", 
                   "Giraffe",
                   "Spotted hyena")

par(mfrow=c(length(key_sp),4), mar=c(3, 4, 2, 2))
for(i in 1:length(key_sp)){
  for(j in 1:2){
    pr <- beta_direct_df %>% filter(species == key_sp[i])
    pr <- pr[,j]
    
    den <- density(pr)
    
    PI95 <- HPDI(pr, prob = 0.95)
    PI89 <- HPDI(pr, prob = 0.89)
    PI80 <- HPDI(pr, prob = 0.80)
    PI70 <- HPDI(pr, prob = 0.70)
    PI60 <- HPDI(pr, prob = 0.60)
    PI50 <- HPDI(pr, prob = 0.50)
    PI_all <- rbind(PI95, PI89, PI80, PI70, PI60, PI50)
    
    if(j == 1){
      ttl <- bquote(.(species_names[i]) ~ beta[OPUNTIA])
    }else{
      ttl <- bquote(.(species_names[i]) ~ beta[FRUIT])
    }
    
    
    plot(den, main = ttl, xlab="")
    
    for(k in 1:nrow(PI_all)){
      l <- min(which(den$x >= PI_all[k,1]))
      h <- max(which(den$x < PI_all[k,2]))
      polygon(c(den$x[c(l, l:h, h)]),
              c(0, den$y[l:h], 0),
              col = col.alpha("black", 0.15), border=NA)
    }
    abline(v = 0, lty=1)
  }
}


beta_df_long <- beta_df %>% pivot_longer(cols = c("beta_opuntia", "beta_fruit"), 
                                         names_to = "parameter",
                                         values_to = "value")
beta_df_long$parameter <- as.factor(beta_df_long$parameter)
beta_df_long <- as.data.frame(beta_df_long)

p1 <- ggplot(beta_df_long, aes(y = species, x = value, fill = parameter, colour = parameter)) +
  stat_dots(position = "dodgejust") +
  theme_classic() + 
  geom_vline(xintercept = 0)
p1

#####
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
plot(GPS_lat~GPS_long, data=site_data, col=rangi2, pch=16, main="Correlation inferred from model (elephant)")

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

x_seq <- seq(0,6.1,0.01)
pmcov <- sapply(x_seq, function(x) post$etasq * exp(-post$rhosq * x^2))
pmcov_mu <- apply(pmcov, 2, median)
pmcov_95CI <- apply(pmcov, 2, HPDI, prob=0.95)
pmcov_89CI <- apply(pmcov, 2, HPDI, prob=0.89)
pmcov_80CI <- apply(pmcov, 2, HPDI, prob=0.80)
pmcov_70CI <- apply(pmcov, 2, HPDI, prob=0.70)
pmcov_60CI <- apply(pmcov, 2, HPDI, prob=0.60)
pmcov_50CI <- apply(pmcov, 2, HPDI, prob=0.50)

par(mfrow=c(1,1))
plot(NULL, xlab="Distance", ylab="Covariance", xlim=c(0,6.1), ylim=c(0,3), main="Posterior w/ Compatability Intervals")
lines(x_seq, pmcov_mu, lwd=2)
shade(pmcov_95CI, x_seq)
shade(pmcov_89CI, x_seq)
shade(pmcov_80CI, x_seq)
shade(pmcov_70CI, x_seq)
shade(pmcov_60CI, x_seq)
shade(pmcov_50CI, x_seq)
curve(eta2 * exp(-rho2 * x^2), add=TRUE, lwd=3, lty=2)




# Simulating intervention ####

# Simulate for the actual sites surveyed
simsites <- ncol(post$psi) # Use all sites
nsamples <- length(post$beta_opuntia) # Use all posterior samples

# Matrices to store results
S1 <- matrix(0, nrow=nsamples, ncol=simsites)
S2 <- matrix(0, nrow=nsamples, ncol=simsites)

orig_opuntia_vol <- site_data$opuntia_volume
increased_opuntia_vol <- 1.5 * orig_opuntia_vol

orig_total_ripe <- site_data$total_ripe
increased_total_ripe <- 1.5 * site_data$total_ripe

# Under status quo 
for(s in 1:simsites){
  inv_psi <- post$k_bar + post$k[,s] + 
    post$beta_opuntia * standardize(orig_opuntia_vol)[s] + 
    post$beta_fruit * standardize(orig_total_ripe)[s] +
    post$beta_d_water * standardize(site_data$dist_river)[s] +
    post$beta_d_road * standardize(site_data$dist_road)[s] +
    post$beta_livestock * standardize(site_data$livestock_proportion)[s] +
    post$beta_grass * standardize(site_data$grass_total)[s] +
    post$beta_shrub * standardize(site_data$shrub_total)[s] +
    post$beta_succulent * standardize(site_data$succulent_total)[s] +
    post$beta_tree * standardize(site_data$tree_total)[s]
  psi_sim <- inv_logit(inv_psi)
  S1[,s] <- psi_sim
}

# Under distribution of x after intervention
for(s in 1:simsites){
  inv_psi <- post$k_bar + post$k[,s] + 
    post$beta_opuntia * standardize(increased_opuntia_vol)[s] + 
    post$beta_fruit * standardize(increased_total_ripe)[s] +
    post$beta_d_water * standardize(site_data$dist_river)[s] +
    post$beta_d_road * standardize(site_data$dist_road)[s] +
    post$beta_livestock * standardize(site_data$livestock_proportion)[s] +
    post$beta_grass * standardize(site_data$grass_total)[s] +
    post$beta_shrub * standardize(site_data$shrub_total)[s] +
    post$beta_succulent * standardize(site_data$succulent_total)[s] +
    post$beta_tree * standardize(site_data$tree_total)[s]
  psi_sim <- inv_logit(inv_psi)
  S2[,s] <- psi_sim
}

Sdiff <- S2-S1
hist(Sdiff, breaks=100)




# Broad-scale (grid-square) models
grid_data <- site_data %>% filter(!is.na(grid_total_volume))
dd <- detmats$elephant
dd <- dd %>% filter(dd$site %in% grid_data$Site_ID)
dd <- as.matrix(dd[,-1])
dd[is.na(dd)] <- -9999
mode(dd) <- "integer"

dmat <- generate_distance_matrix(grid_data, log = TRUE)
sitedays_grid <- sitedays %>% filter(Site %in% grid_data$Site_ID)

dlist <- list(
  # Number of sites and visits
  nsites = as.integer(nrow(dmat)),
  N_maxvisits = as.integer(max(sitedays_grid$Days)),
  V = as.integer(sitedays_grid$Days),
  # Observed data
  y = dd,
  # Occupancy covariates
  opuntia_vol = standardize(as.numeric(grid_data$grid_total_volume)),
  d_water = standardize(grid_data$dist_river),
  d_road = standardize(grid_data$dist_road),
  livestock = standardize(grid_data$livestock_proportion),
  grass = standardize(grid_data$grass_total),
  forb = standardize(grid_data$forb_total),
  shrub = standardize(grid_data$shrub_total),
  succulent = standardize(grid_data$succulent_total),
  tree = standardize(grid_data$n_trees),
  # Detection covariates
  
  # Distance matrix
  dmat = dmat
)

m_test <- cstan(file = "C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/occupancy_models/ch3/total_effect_no_veg_path.stan",
               data = dlist,
               chains = 4,
               cores = 4,
               warmup = 4500,
               iter = 5500,
               seed = 99)

dashboard(m_test)
precis(m_test)
post <- extract.samples(m_test)
