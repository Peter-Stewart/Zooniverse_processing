# Load packages ####
library(rethinking)
library(dplyr)
library(tidyr)
library(bayesplot)
library(viridis)
library(activity)
library(lubridate)

# Source helper functions ####
source("C:/Users/PeteS/OneDrive/R Scripts Library/Projects/Zooniverse/helper_functions_v2.R", echo = FALSE)

# Load data ####
setwd("C:/temp/Zooniverse/Oct22/processed")
consensus_classifications <- get(load("consensus_classifications.Rdata"))
detmats <- get(load("detmats.Rdata"))
startends <- get(load("startends.Rdata"))
sitedays <- get(load("sitedays.Rdata"))
sitedays <- sitedays %>% group_by(Site) %>% summarise(Days = sum(Days)) # Group by site to get total days
validation_set <- get(load("validation_set.Rdata"))

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
site_data <- read.csv("Cameras_site_data_main.csv", header = TRUE)
site_data <- site_data %>% filter(Site_ID %in% sitedays$Site) # Remove the two vandalised sites

opuntia_data <- read.csv("Cameras_opuntia_data_main.csv", header = TRUE)
opuntia_data <- opuntia_data %>% filter(Site_ID %in% sitedays$Site) # Remove the two vandalised sites

tree_data <- read.csv("Cameras_tree_data_main.csv", header = TRUE)
tree_data <- tree_data %>% filter(Site_ID %in% sitedays$Site) # Remove the two vandalised sites

df_mu_f <- get(load("grid_square_fruiting.Rdata"))
df_mu_n <- get(load("grid_square_non_fruiting.Rdata"))
df_mu_t <- get(load("grid_square_total.Rdata"))

setwd("C:/GIS_temp/Ch3_spatial_analysis/Fieldseason1/Outputs")
dist_river <- read.csv("distance_to_river.csv", header = TRUE)
dist_road <- read.csv("distance_to_road.csv", header = TRUE)

setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data")
weather <- read.csv("Mpala_weather.csv")
weather <- weather %>% separate(TIMESTAMP, into = c("Date", "Time"), sep = " ", remove = FALSE, convert = FALSE)

# Process data into required format ####
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

site_data <- merge(site_data, tree_sites, by="Site_ID", all.x = TRUE)
site_data$n_trees[is.na(site_data$n_trees)] <- 0 # Sites with no tree data have zero trees

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
colnames(df_mu_t) <- c("grid_square", "volume_total")
colnames(df_mu_f) <- c("grid_square", "volume_fruiting")
colnames(df_mu_n) <- c("grid_square", "volume_nonfruiting")

site_data$grid_square <- as.factor(site_data$Grid_square)
site_data <- merge(site_data, df_mu_t, by="grid_square", all.x = TRUE)
site_data <- merge(site_data, df_mu_f, by="grid_square", all.x = TRUE)
site_data <- merge(site_data, df_mu_n, by="grid_square", all.x = TRUE)

# Detection covariates 
# Camera model
site_data$Cam_model <- as.factor(site_data$Cam_model)

# Weather
temperature <- bind_daily_covs(startends = startends,
                               day_data = weather, 
                               day_cov = "AirTC_1_Avg",
                               summary_type = "mean",
                               standardise = TRUE,
                               date_format = "dmy")
temperature[is.na(temperature)] <- -9999 # Replace NA with ridiculous number so it's obvious if these values are used accidentally
temperature_mat <- as.matrix(temperature[,-1])

# Ensure that site_data is ordered by site_ID
site_data <- site_data[order(site_data$Site_ID),]

# Generate distance matrix
dmat <- generate_distance_matrix(site_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

# Occupancy models - fine-scale #### 
#setwd("C:/temp/ch3_post")
setwd("F:/ch3_post_new/fine_scale")

key_sp <- "dikdik"

indexes <- list()
for(i in 1:length(detmats)){
  if(names(detmats[i]) %in% key_sp){
    indexes[i] <- i
  }else{
    indexes[i] <- NULL}
}
indexes <- do.call(rbind, indexes)

# List the models which will be run
#model_list <- c("direct_effects","total_effect_no_veg_path","total_effect_veg_path")

#model_list <- "direct_effects"
model_list <- "total_effect_no_veg_path"
#model_list <- "total_effect_veg_path"


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
  
  setwd(paste0("F:/ch3_post_new/fine_scale/",model_list[m]))
  
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
      opuntia_vol = standardize(site_data$opuntia_total_cover),
      fruit = site_data$total_ripe_bin,
      d_water = standardize(site_data$dist_river),
      d_road = standardize(site_data$dist_road),
      livestock = standardize(site_data$livestock_proportion),
      grass = standardize(site_data$grass_total),
      forb = standardize(site_data$forb_total),
      shrub = standardize(site_data$shrub_total),
      succulent = standardize(site_data$succulent_total),
      tree = standardize(site_data$n_trees),
      # Detection covariates
      cam_model = as.integer(site_data$Cam_model),
      temp = temperature_mat,
      
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
    #trankplot(m1_nc, pars=c(p, p2))
    trankplot(m1_nc, pars=p)
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

# Occupancy models - broad-scale ####
setwd("F:/ch3_post_new/grid_square")

key_sp <- "dikdik"

indexes <- list()
for(i in 1:length(detmats)){
  if(names(detmats[i]) %in% key_sp){
    indexes[i] <- i
  }else{
    indexes[i] <- NULL}
}
indexes <- do.call(rbind, indexes)

# List the models which will be run
model_list <- c("direct_effects",
                "total_effect_no_veg_path",
                "total_effect_veg_path")

#model_list <- "direct_effects"
#model_list <- "total_effect_no_veg_path"
#model_list <- "total_effect_veg_path"

site_data$volume_total <- as.numeric(site_data$volume_total)
site_data$volume_fruiting <- as.numeric(site_data$volume_fruiting)
site_data$volume_nonfruiting <- as.numeric(site_data$volume_nonfruiting)

grid_data <- site_data %>% filter(!is.na(volume_total))
sitedays_grid <- sitedays %>% filter(Site %in% grid_data$Site_ID)

dmat <- generate_distance_matrix(grid_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

temperature <- temperature %>% filter(Site %in% grid_data$Site_ID)
temperature_mat <- as.matrix(temperature[,-1])

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
  
  setwd(paste0("F:/ch3_post_new/grid_square/",model_list[m]))
  
  
  for(sp in 1:length(key_sp)){
    
    dd <- (detmats[indexes[sp,]])
    dd <- as.data.frame(dd[[1]])
    dd <- dd %>% filter(dd$site %in% grid_data$Site_ID)
    dd <- as.matrix(dd[,-1])
    dd[is.na(dd)] <- -9999
    mode(dd) <- "integer"
    
    
    # Prepare data list for Stan
    dlist <- list(
      # Number of sites and visits
      nsites = as.integer(nrow(dmat)),
      N_maxvisits = as.integer(max(sitedays_grid$Days)),
      V = as.integer(sitedays_grid$Days),
      # Observed data
      y = dd,
      # Occupancy covariates
      opuntia_vol = standardize(grid_data$volume_total),
      fruit = standardize(grid_data$volume_fruiting),
      d_water = standardize(grid_data$dist_river),
      d_road = standardize(grid_data$dist_road),
      livestock = standardize(grid_data$livestock_proportion),
      grass = standardize(grid_data$grass_total),
      forb = standardize(grid_data$forb_total),
      shrub = standardize(grid_data$shrub_total),
      succulent = standardize(grid_data$succulent_total),
      tree = standardize(grid_data$n_trees),
      # Detection covariates
      cam_model = as.integer(grid_data$Cam_model),
      temp = temperature_mat,
      
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
    trankplot(m1_nc, pars=p)
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


# Marginal effect plots - fine-scale ####

# Direct effects ####
setwd("F:/ch3_post_new/fine_scale/direct_effects")
post_direct_baboon <- get(load("baboon_direct_effects_post.Rdata")); rm(post); gc()
post_direct_elephant <- get(load("elephant_direct_effects_post.Rdata")); rm(post); gc()
post_direct_vervetmonkey <- get(load("vervetmonkey_direct_effects_post.Rdata")); rm(post); gc()
post_direct_zebragrevys <- get(load("zebragrevys_direct_effects_post.Rdata")); rm(post); gc()
post_direct_impala <- get(load("impala_direct_effects_post.Rdata")); rm(post); gc()
post_direct_dikdik <- get(load("dikdik_direct_effects_post.Rdata")); rm(post); gc()
post_direct_giraffe <- get(load("giraffe_direct_effects_post.Rdata")); rm(post); gc()
post_direct_hyenaspotted <- get(load("hyenaspotted_direct_effects_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_direct_baboon <- as.data.frame(cbind(post_direct_baboon$beta_opuntia, post_direct_baboon$beta_fruit, post_direct_baboon$k_bar, post_direct_baboon$beta_opuntia_fruit))
df_direct_elephant <- as.data.frame(cbind(post_direct_elephant$beta_opuntia, post_direct_elephant$beta_fruit, post_direct_elephant$k_bar, post_direct_elephant$beta_opuntia_fruit))
df_direct_vervetmonkey <- as.data.frame(cbind(post_direct_vervetmonkey$beta_opuntia, post_direct_vervetmonkey$beta_fruit, post_direct_vervetmonkey$k_bar, post_direct_vervetmonkey$beta_opuntia_fruit))
df_direct_zebragrevys <- as.data.frame(cbind(post_direct_zebragrevys$beta_opuntia, post_direct_zebragrevys$beta_fruit, post_direct_zebragrevys$k_bar, post_direct_zebragrevys$beta_opuntia_fruit))
df_direct_impala <- as.data.frame(cbind(post_direct_impala$beta_opuntia, post_direct_impala$beta_fruit, post_direct_impala$k_bar, post_direct_impala$beta_opuntia_fruit))
df_direct_dikdik <- as.data.frame(cbind(post_direct_dikdik$beta_opuntia, post_direct_dikdik$beta_fruit, post_direct_dikdik$k_bar, post_direct_dikdik$beta_opuntia_fruit))
df_direct_giraffe <- as.data.frame(cbind(post_direct_giraffe$beta_opuntia, post_direct_giraffe$beta_fruit, post_direct_giraffe$k_bar, post_direct_giraffe$beta_opuntia_fruit))
df_direct_hyenaspotted <- as.data.frame(cbind(post_direct_hyenaspotted$beta_opuntia, post_direct_hyenaspotted$beta_fruit, post_direct_hyenaspotted$k_bar, post_direct_hyenaspotted$beta_opuntia_fruit))

# Remove full posterior distributions to save memory
rm(post_direct_baboon)
rm(post_direct_elephant)
rm(post_direct_vervetmonkey)
rm(post_direct_zebragrevys)
rm(post_direct_impala)
rm(post_direct_dikdik)
rm(post_direct_giraffe)
rm(post_direct_hyenaspotted)
gc()

# Add species column
df_direct_baboon$species <- as.factor("baboon")
df_direct_elephant$species <- as.factor("elephant")
df_direct_vervetmonkey$species <- as.factor("vervetmonkey")
df_direct_zebragrevys$species <- as.factor("zebragrevys")
df_direct_impala$species <- as.factor("impala")
df_direct_dikdik$species <- as.factor("dikdik")
df_direct_giraffe$species <- as.factor("giraffe")
df_direct_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_direct_baboon) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_elephant) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_vervetmonkey) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_zebragrevys) <-c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_impala) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_dikdik) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_giraffe) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_hyenaspotted) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")

# Bind into one big dataframe
beta_direct_df <- rbind(df_direct_baboon, 
                        df_direct_elephant,
                        df_direct_vervetmonkey,
                        df_direct_zebragrevys,
                        df_direct_impala,
                        df_direct_dikdik,
                        df_direct_giraffe,
                        df_direct_hyenaspotted)

# Remove the separate df's to save memory
rm(df_direct_baboon)
rm(df_direct_elephant)
rm(df_direct_giraffe)
rm(df_direct_hyenaspotted)
rm(df_direct_impala)
rm(df_direct_dikdik)
rm(df_direct_vervetmonkey)
rm(df_direct_zebragrevys)
gc()

# Total effects without vegetation pathway ####
setwd("F:/ch3_post_new/fine_scale/total_effect_no_veg_path")
post_total_no_veg_path_baboon <- get(load("baboon_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_elephant <- get(load("elephant_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_vervetmonkey <- get(load("vervetmonkey_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_zebragrevys <- get(load("zebragrevys_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_impala <- get(load("impala_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_dikdik <- get(load("dikdik_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_giraffe <- get(load("giraffe_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_hyenaspotted <- get(load("hyenaspotted_total_effect_no_veg_path_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_total_no_veg_path_baboon <- as.data.frame(cbind(post_total_no_veg_path_baboon$beta_opuntia, post_total_no_veg_path_baboon$beta_fruit, post_total_no_veg_path_baboon$k_bar))
df_total_no_veg_path_elephant <- as.data.frame(cbind(post_total_no_veg_path_elephant$beta_opuntia, post_total_no_veg_path_elephant$beta_fruit, post_total_no_veg_path_elephant$k_bar))
df_total_no_veg_path_vervetmonkey <- as.data.frame(cbind(post_total_no_veg_path_vervetmonkey$beta_opuntia, post_total_no_veg_path_vervetmonkey$beta_fruit, post_total_no_veg_path_vervetmonkey$k_bar))
df_total_no_veg_path_zebragrevys <- as.data.frame(cbind(post_total_no_veg_path_zebragrevys$beta_opuntia, post_total_no_veg_path_zebragrevys$beta_fruit, post_total_no_veg_path_zebragrevys$k_bar))
df_total_no_veg_path_impala <- as.data.frame(cbind(post_total_no_veg_path_impala$beta_opuntia, post_total_no_veg_path_impala$beta_fruit, post_total_no_veg_path_impala$k_bar))
df_total_no_veg_path_dikdik <- as.data.frame(cbind(post_total_no_veg_path_dikdik$beta_opuntia, post_total_no_veg_path_dikdik$beta_fruit, post_total_no_veg_path_dikdik$k_bar))
df_total_no_veg_path_giraffe <- as.data.frame(cbind(post_total_no_veg_path_giraffe$beta_opuntia, post_total_no_veg_path_giraffe$beta_fruit, post_total_no_veg_path_giraffe$k_bar))
df_total_no_veg_path_hyenaspotted <- as.data.frame(cbind(post_total_no_veg_path_hyenaspotted$beta_opuntia, post_total_no_veg_path_hyenaspotted$beta_fruit, post_total_no_veg_path_hyenaspotted$k_bar))

# Remove full posterior distributions to save memory
rm(post_total_no_veg_path_baboon)
rm(post_total_no_veg_path_elephant)
rm(post_total_no_veg_path_vervetmonkey)
rm(post_total_no_veg_path_zebragrevys)
rm(post_total_no_veg_path_impala)
rm(post_total_no_veg_path_dikdik)
rm(post_total_no_veg_path_giraffe)
rm(post_total_no_veg_path_hyenaspotted)
gc()

# Add species column
df_total_no_veg_path_baboon$species <- as.factor("baboon")
df_total_no_veg_path_elephant$species <- as.factor("elephant")
df_total_no_veg_path_vervetmonkey$species <- as.factor("vervetmonkey")
df_total_no_veg_path_zebragrevys$species <- as.factor("zebragrevys")
df_total_no_veg_path_impala$species <- as.factor("impala")
df_total_no_veg_path_dikdik$species <- as.factor("dikdik")
df_total_no_veg_path_giraffe$species <- as.factor("giraffe")
df_total_no_veg_path_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_total_no_veg_path_baboon) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_elephant) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_vervetmonkey) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_zebragrevys) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_impala) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_dikdik) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_giraffe) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_hyenaspotted) <- c("beta_total1", "k_bar2", "species")

# Bind into one big dataframe
beta_total_no_veg_path_df <- rbind(df_total_no_veg_path_baboon, 
                                   df_total_no_veg_path_elephant,
                                   df_total_no_veg_path_vervetmonkey,
                                   df_total_no_veg_path_zebragrevys,
                                   df_total_no_veg_path_impala,
                                   df_total_no_veg_path_dikdik,
                                   df_total_no_veg_path_giraffe,
                                   df_total_no_veg_path_hyenaspotted)

# Remove the separate df's to save memory
rm(df_total_no_veg_path_baboon)
rm(df_total_no_veg_path_elephant)
rm(df_total_no_veg_path_giraffe)
rm(df_total_no_veg_path_hyenaspotted)
rm(df_total_no_veg_path_impala)
rm(df_total_no_veg_path_dikdik)
rm(df_total_no_veg_path_vervetmonkey)
rm(df_total_no_veg_path_zebragrevys)
gc()

# Total effects with vegetation pathway ####
setwd("F:/ch3_post_new/fine_scale/total_effect_veg_path")
post_total_veg_path_baboon <- get(load("baboon_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_elephant <- get(load("elephant_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_vervetmonkey <- get(load("vervetmonkey_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_zebragrevys <- get(load("zebragrevys_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_impala <- get(load("impala_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_dikdik <- get(load("dikdik_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_giraffe <- get(load("giraffe_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_hyenaspotted <- get(load("hyenaspotted_total_effect_veg_path_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_total_veg_path_baboon <- as.data.frame(cbind(post_total_veg_path_baboon$beta_opuntia, post_total_veg_path_baboon$beta_fruit, post_total_veg_path_baboon$k_bar))
df_total_veg_path_elephant <- as.data.frame(cbind(post_total_veg_path_elephant$beta_opuntia, post_total_veg_path_elephant$beta_fruit, post_total_veg_path_elephant$k_bar))
df_total_veg_path_vervetmonkey <- as.data.frame(cbind(post_total_veg_path_vervetmonkey$beta_opuntia, post_total_veg_path_vervetmonkey$beta_fruit, post_total_veg_path_vervetmonkey$k_bar))
df_total_veg_path_zebragrevys <- as.data.frame(cbind(post_total_veg_path_zebragrevys$beta_opuntia, post_total_veg_path_zebragrevys$beta_fruit, post_total_veg_path_zebragrevys$k_bar))
df_total_veg_path_impala <- as.data.frame(cbind(post_total_veg_path_impala$beta_opuntia, post_total_veg_path_impala$beta_fruit, post_total_veg_path_impala$k_bar))
df_total_veg_path_dikdik <- as.data.frame(cbind(post_total_veg_path_dikdik$beta_opuntia, post_total_veg_path_dikdik$beta_fruit, post_total_veg_path_dikdik$k_bar))
df_total_veg_path_giraffe <- as.data.frame(cbind(post_total_veg_path_giraffe$beta_opuntia, post_total_veg_path_giraffe$beta_fruit, post_total_veg_path_giraffe$k_bar))
df_total_veg_path_hyenaspotted <- as.data.frame(cbind(post_total_veg_path_hyenaspotted$beta_opuntia, post_total_veg_path_hyenaspotted$beta_fruit, post_total_veg_path_hyenaspotted$k_bar))

# Remove full posterior distributions to save memory
rm(post_total_veg_path_baboon)
rm(post_total_veg_path_elephant)
rm(post_total_veg_path_vervetmonkey)
rm(post_total_veg_path_zebragrevys)
rm(post_total_veg_path_impala)
rm(post_total_veg_path_dikdik)
rm(post_total_veg_path_giraffe)
rm(post_total_veg_path_hyenaspotted)
gc()

# Add species column
df_total_veg_path_baboon$species <- as.factor("baboon")
df_total_veg_path_elephant$species <- as.factor("elephant")
df_total_veg_path_vervetmonkey$species <- as.factor("vervetmonkey")
df_total_veg_path_zebragrevys$species <- as.factor("zebragrevys")
df_total_veg_path_impala$species <- as.factor("impala")
df_total_veg_path_dikdik$species <- as.factor("dikdik")
df_total_veg_path_giraffe$species <- as.factor("giraffe")
df_total_veg_path_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_total_veg_path_baboon) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_elephant) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_vervetmonkey) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_zebragrevys) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_impala) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_dikdik) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_giraffe) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_hyenaspotted) <- c("beta_total2", "k_bar3", "species")

# Bind into one big dataframe
beta_total_veg_path_df <- rbind(df_total_veg_path_baboon, 
                                df_total_veg_path_elephant,
                                df_total_veg_path_vervetmonkey,
                                df_total_veg_path_zebragrevys,
                                df_total_veg_path_impala,
                                df_total_veg_path_dikdik,
                                df_total_veg_path_giraffe,
                                df_total_veg_path_hyenaspotted)

# Remove the separate df's to save memory
rm(df_total_veg_path_baboon)
rm(df_total_veg_path_elephant)
rm(df_total_veg_path_giraffe)
rm(df_total_veg_path_hyenaspotted)
rm(df_total_veg_path_impala)
rm(df_total_veg_path_dikdik)
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
            "dikdik",
            "giraffe",
            "hyenaspotted")

species_names <- c("Olive baboon",
                   "Elephant",
                   "Vervet monkey",
                   "Grevy's zebra",
                   "Impala", 
                   "Dik-dik",
                   "Giraffe",
                   "Spotted hyena")

plot_titles <- c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)")

# Colours for shading CI's for each species
#species_colours <- viridis(7)
species_colours <- rep("#35B779FF", 8)
colouralpha <- 0.4

# Open new graphics device to save as TIFF
setwd("C:/Users/PeteS/OneDrive/Durham/Occupancy chapter")
#par(mfrow=c(2,4), mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1.5) + 0.1, tck = -0.02)
#pr <- par()
pr <- get(load("C:/Users/PeteS/OneDrive/Durham/Occupancy chapter/good_plot_par.Rdata")) # Good settings for 8-panel plots

tiff("fine_scale_direct_effects.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)

xseq <- seq(-0.7575, 4.2039, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- beta_all_df %>% filter(species == key_sp[i])
  
  # Calculate marginal effects using k_bar and the beta parameters
  p1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq)) # When fruit = -1 
  p2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq)) # When fruit = 2.5
  p3 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p4 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p1[,x] <- inv_logit(s$k_bar1 + s$beta_opuntia*xseq[x] + s$beta_fruit*-1 + s$beta_opuntia_fruit*xseq[x]*-1)  # When fruit = -1
    p2[,x] <- inv_logit(s$k_bar1 + s$beta_opuntia*xseq[x] + s$beta_fruit*2.5 + s$beta_opuntia_fruit*xseq[x]*2.5) # When fruit = 2.5
    p3[,x] <- inv_logit(s$k_bar2 + s$beta_total1*xseq[x])
    p4[,x] <- inv_logit(s$k_bar3 + s$beta_total2*xseq[x])
  }
  
  
  # Calculate mean and CI's
  mu1 <- apply(p1, 2, median)
  PI95_1 <- apply(p1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p1, 2, HPDI, prob=0.89)
  PI80_1 <- apply(p1, 2, HPDI, prob=0.80)
  PI70_1 <- apply(p1, 2, HPDI, prob=0.70)
  PI60_1 <- apply(p1, 2, HPDI, prob=0.60)
  PI50_1 <- apply(p1, 2, HPDI, prob=0.50)
  PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p2, 2, median)
  PI95_2 <- apply(p2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p2, 2, HPDI, prob=0.89)
  PI80_2 <- apply(p2, 2, HPDI, prob=0.80)
  PI70_2 <- apply(p2, 2, HPDI, prob=0.70)
  PI60_2 <- apply(p2, 2, HPDI, prob=0.60)
  PI50_2 <- apply(p2, 2, HPDI, prob=0.50)
  PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  mu3 <- apply(p3, 2, median)
  PI95_3 <- apply(p3, 2, HPDI, prob=0.95)
  PI89_3 <- apply(p3, 2, HPDI, prob=0.89)
  PI80_3 <- apply(p3, 2, HPDI, prob=0.80)
  PI70_3 <- apply(p3, 2, HPDI, prob=0.70)
  PI60_3 <- apply(p3, 2, HPDI, prob=0.60)
  PI50_3 <- apply(p3, 2, HPDI, prob=0.50)
  PI_all_3 <- rbind(PI95_3, PI89_3, PI80_3, PI70_3, PI60_3, PI50_3)
  
  mu4 <- apply(p4, 2, median)
  PI95_4 <- apply(p4, 2, HPDI, prob=0.95)
  PI89_4 <- apply(p4, 2, HPDI, prob=0.89)
  PI80_4 <- apply(p4, 2, HPDI, prob=0.80)
  PI70_4 <- apply(p4, 2, HPDI, prob=0.70)
  PI60_4 <- apply(p4, 2, HPDI, prob=0.60)
  PI50_4 <- apply(p4, 2, HPDI, prob=0.50)
  PI_all_4 <- rbind(PI95_4, PI89_4, PI80_4, PI70_4, PI60_4, PI50_4)
  
  # For p1 and p2 eliminate cover/fruit combos which don't exist in data
  id1 <- which(xseq < -0.7575 | xseq > 1.3098)
  id2 <- which(xseq < -0.65416 | xseq > 4.2039)
  
  xseq1 <- xseq
  xseq2 <- xseq
  
  xseq1[id1] <- NA
  xseq2[id2] <- NA
  
  xseq1 <- xseq1[!is.na(xseq1)]
  xseq2 <- xseq2[!is.na(xseq2)]
  
  mu1[id1] <- NA
  mu1 <- mu1[!is.na(mu1)]
  PI95_1[,id1] <- NA
  PI89_1[,id1] <- NA
  PI80_1[,id1] <- NA
  PI70_1[,id1] <- NA
  PI60_1[,id1] <- NA
  PI50_1[,id1] <- NA
  PI95_1 <- PI95_1[, !apply(is.na(PI95_1), 2, any)]
  PI89_1 <- PI89_1[, !apply(is.na(PI89_1), 2, any)]
  PI80_1 <- PI80_1[, !apply(is.na(PI80_1), 2, any)]
  PI70_1 <- PI70_1[, !apply(is.na(PI70_1), 2, any)]
  PI60_1 <- PI60_1[, !apply(is.na(PI60_1), 2, any)]
  PI50_1 <- PI50_1[, !apply(is.na(PI50_1), 2, any)]
  
  
  mu2[id2] <- NA
  mu2 <- mu2[!is.na(mu2)]
  PI95_2[,id2] <- NA
  PI89_2[,id2] <- NA
  PI80_2[,id2] <- NA
  PI70_2[,id2] <- NA
  PI60_2[,id2] <- NA
  PI50_2[,id2] <- NA
  PI95_2 <- PI95_2[, !apply(is.na(PI95_2), 2, any)]
  PI89_2 <- PI89_2[, !apply(is.na(PI89_2), 2, any)]
  PI80_2 <- PI80_2[, !apply(is.na(PI80_2), 2, any)]
  PI70_2 <- PI70_2[, !apply(is.na(PI70_2), 2, any)]
  PI60_2 <- PI60_2[, !apply(is.na(PI60_2), 2, any)]
  PI50_2 <- PI50_2[, !apply(is.na(PI50_2), 2, any)]
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = expression(psi),
       xlab="Opuntia cover", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  # Direct effects
  shade(PI95_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  shade(PI89_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  shade(PI80_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  shade(PI70_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  shade(PI60_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  shade(PI50_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  
  shade(PI95_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  shade(PI89_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  shade(PI80_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  shade(PI70_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  shade(PI60_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  shade(PI50_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  
  points(x = xseq1, y = mu1, type="l", lwd=2, col = "#21908CFF")
  points(x = xseq2, y = mu2, type="l", lwd=2, col = "#443A83FF")
  
  # Total effects no veg. path
  #shade(PI95_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI89_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI80_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI70_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI60_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI50_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  #points(x = xseq, y = mu3, type="l", lwd=2)
  
  # Total effects veg. path
  #shade(PI95_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI89_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI80_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI70_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI60_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI50_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #points(x = xseq, y = mu4, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Marginal effect plots - broad-scale ####

# Direct effects ####
setwd("F:/ch3_post_new/grid_square/direct_effects")
post_direct_baboon <- get(load("baboon_direct_effects_post.Rdata")); rm(post); gc()
post_direct_elephant <- get(load("elephant_direct_effects_post.Rdata")); rm(post); gc()
post_direct_vervetmonkey <- get(load("vervetmonkey_direct_effects_post.Rdata")); rm(post); gc()
post_direct_zebragrevys <- get(load("zebragrevys_direct_effects_post.Rdata")); rm(post); gc()
post_direct_impala <- get(load("impala_direct_effects_post.Rdata")); rm(post); gc()
post_direct_dikdik <- get(load("dikdik_direct_effects_post.Rdata")); rm(post); gc()
post_direct_giraffe <- get(load("giraffe_direct_effects_post.Rdata")); rm(post); gc()
post_direct_hyenaspotted <- get(load("hyenaspotted_direct_effects_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_direct_baboon <- as.data.frame(cbind(post_direct_baboon$beta_opuntia, post_direct_baboon$beta_fruit, post_direct_baboon$k_bar, post_direct_baboon$beta_opuntia_fruit))
df_direct_elephant <- as.data.frame(cbind(post_direct_elephant$beta_opuntia, post_direct_elephant$beta_fruit, post_direct_elephant$k_bar, post_direct_elephant$beta_opuntia_fruit))
df_direct_vervetmonkey <- as.data.frame(cbind(post_direct_vervetmonkey$beta_opuntia, post_direct_vervetmonkey$beta_fruit, post_direct_vervetmonkey$k_bar, post_direct_vervetmonkey$beta_opuntia_fruit))
df_direct_zebragrevys <- as.data.frame(cbind(post_direct_zebragrevys$beta_opuntia, post_direct_zebragrevys$beta_fruit, post_direct_zebragrevys$k_bar, post_direct_zebragrevys$beta_opuntia_fruit))
df_direct_impala <- as.data.frame(cbind(post_direct_impala$beta_opuntia, post_direct_impala$beta_fruit, post_direct_impala$k_bar, post_direct_impala$beta_opuntia_fruit))
df_direct_dikdik <- as.data.frame(cbind(post_direct_dikdik$beta_opuntia, post_direct_dikdik$beta_fruit, post_direct_dikdik$k_bar, post_direct_dikdik$beta_opuntia_fruit))
df_direct_giraffe <- as.data.frame(cbind(post_direct_giraffe$beta_opuntia, post_direct_giraffe$beta_fruit, post_direct_giraffe$k_bar, post_direct_giraffe$beta_opuntia_fruit))
df_direct_hyenaspotted <- as.data.frame(cbind(post_direct_hyenaspotted$beta_opuntia, post_direct_hyenaspotted$beta_fruit, post_direct_hyenaspotted$k_bar, post_direct_hyenaspotted$beta_opuntia_fruit))

# Remove full posterior distributions to save memory
rm(post_direct_baboon)
rm(post_direct_elephant)
rm(post_direct_vervetmonkey)
rm(post_direct_zebragrevys)
rm(post_direct_impala)
rm(post_direct_dikdik)
rm(post_direct_giraffe)
rm(post_direct_hyenaspotted)
gc()

# Add species column
df_direct_baboon$species <- as.factor("baboon")
df_direct_elephant$species <- as.factor("elephant")
df_direct_vervetmonkey$species <- as.factor("vervetmonkey")
df_direct_zebragrevys$species <- as.factor("zebragrevys")
df_direct_impala$species <- as.factor("impala")
df_direct_dikdik$species <- as.factor("dikdik")
df_direct_giraffe$species <- as.factor("giraffe")
df_direct_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_direct_baboon) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_elephant) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_vervetmonkey) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_zebragrevys) <-c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_impala) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_dikdik) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_giraffe) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")
colnames(df_direct_hyenaspotted) <- c("beta_opuntia", "beta_fruit","k_bar1","beta_opuntia_fruit", "species")

# Bind into one big dataframe
beta_direct_df <- rbind(df_direct_baboon, 
                        df_direct_elephant,
                        df_direct_vervetmonkey,
                        df_direct_zebragrevys,
                        df_direct_impala,
                        df_direct_dikdik,
                        df_direct_giraffe,
                        df_direct_hyenaspotted)

# Remove the separate df's to save memory
rm(df_direct_baboon)
rm(df_direct_elephant)
rm(df_direct_giraffe)
rm(df_direct_hyenaspotted)
rm(df_direct_impala)
rm(df_direct_dikdik)
rm(df_direct_vervetmonkey)
rm(df_direct_zebragrevys)
gc()

# Total effects without vegetation pathway ####
setwd("F:/ch3_post_new/grid_square/total_effect_no_veg_path")
post_total_no_veg_path_baboon <- get(load("baboon_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_elephant <- get(load("elephant_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_vervetmonkey <- get(load("vervetmonkey_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_zebragrevys <- get(load("zebragrevys_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_impala <- get(load("impala_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_dikdik <- get(load("dikdik_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_giraffe <- get(load("giraffe_total_effect_no_veg_path_post.Rdata")); rm(post); gc()
post_total_no_veg_path_hyenaspotted <- get(load("hyenaspotted_total_effect_no_veg_path_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_total_no_veg_path_baboon <- as.data.frame(cbind(post_total_no_veg_path_baboon$beta_opuntia, post_total_no_veg_path_baboon$beta_fruit, post_total_no_veg_path_baboon$k_bar))
df_total_no_veg_path_elephant <- as.data.frame(cbind(post_total_no_veg_path_elephant$beta_opuntia, post_total_no_veg_path_elephant$beta_fruit, post_total_no_veg_path_elephant$k_bar))
df_total_no_veg_path_vervetmonkey <- as.data.frame(cbind(post_total_no_veg_path_vervetmonkey$beta_opuntia, post_total_no_veg_path_vervetmonkey$beta_fruit, post_total_no_veg_path_vervetmonkey$k_bar))
df_total_no_veg_path_zebragrevys <- as.data.frame(cbind(post_total_no_veg_path_zebragrevys$beta_opuntia, post_total_no_veg_path_zebragrevys$beta_fruit, post_total_no_veg_path_zebragrevys$k_bar))
df_total_no_veg_path_impala <- as.data.frame(cbind(post_total_no_veg_path_impala$beta_opuntia, post_total_no_veg_path_impala$beta_fruit, post_total_no_veg_path_impala$k_bar))
df_total_no_veg_path_dikdik <- as.data.frame(cbind(post_total_no_veg_path_dikdik$beta_opuntia, post_total_no_veg_path_dikdik$beta_fruit, post_total_no_veg_path_dikdik$k_bar))
df_total_no_veg_path_giraffe <- as.data.frame(cbind(post_total_no_veg_path_giraffe$beta_opuntia, post_total_no_veg_path_giraffe$beta_fruit, post_total_no_veg_path_giraffe$k_bar))
df_total_no_veg_path_hyenaspotted <- as.data.frame(cbind(post_total_no_veg_path_hyenaspotted$beta_opuntia, post_total_no_veg_path_hyenaspotted$beta_fruit, post_total_no_veg_path_hyenaspotted$k_bar))

# Remove full posterior distributions to save memory
rm(post_total_no_veg_path_baboon)
rm(post_total_no_veg_path_elephant)
rm(post_total_no_veg_path_vervetmonkey)
rm(post_total_no_veg_path_zebragrevys)
rm(post_total_no_veg_path_impala)
rm(post_total_no_veg_path_dikdik)
rm(post_total_no_veg_path_giraffe)
rm(post_total_no_veg_path_hyenaspotted)
gc()

# Add species column
df_total_no_veg_path_baboon$species <- as.factor("baboon")
df_total_no_veg_path_elephant$species <- as.factor("elephant")
df_total_no_veg_path_vervetmonkey$species <- as.factor("vervetmonkey")
df_total_no_veg_path_zebragrevys$species <- as.factor("zebragrevys")
df_total_no_veg_path_impala$species <- as.factor("impala")
df_total_no_veg_path_dikdik$species <- as.factor("dikdik")
df_total_no_veg_path_giraffe$species <- as.factor("giraffe")
df_total_no_veg_path_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_total_no_veg_path_baboon) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_elephant) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_vervetmonkey) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_zebragrevys) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_impala) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_dikdik) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_giraffe) <- c("beta_total1", "k_bar2", "species")
colnames(df_total_no_veg_path_hyenaspotted) <- c("beta_total1", "k_bar2", "species")

# Bind into one big dataframe
beta_total_no_veg_path_df <- rbind(df_total_no_veg_path_baboon, 
                                   df_total_no_veg_path_elephant,
                                   df_total_no_veg_path_vervetmonkey,
                                   df_total_no_veg_path_zebragrevys,
                                   df_total_no_veg_path_impala,
                                   df_total_no_veg_path_dikdik,
                                   df_total_no_veg_path_giraffe,
                                   df_total_no_veg_path_hyenaspotted)

# Remove the separate df's to save memory
rm(df_total_no_veg_path_baboon)
rm(df_total_no_veg_path_elephant)
rm(df_total_no_veg_path_giraffe)
rm(df_total_no_veg_path_hyenaspotted)
rm(df_total_no_veg_path_impala)
rm(df_total_no_veg_path_dikdik)
rm(df_total_no_veg_path_vervetmonkey)
rm(df_total_no_veg_path_zebragrevys)
gc()

# Total effects with vegetation pathway ####
setwd("F:/ch3_post_new/grid_square/total_effect_veg_path")
post_total_veg_path_baboon <- get(load("baboon_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_elephant <- get(load("elephant_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_vervetmonkey <- get(load("vervetmonkey_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_zebragrevys <- get(load("zebragrevys_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_impala <- get(load("impala_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_dikdik <- get(load("dikdik_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_giraffe <- get(load("giraffe_total_effect_veg_path_post.Rdata")); rm(post); gc()
post_total_veg_path_hyenaspotted <- get(load("hyenaspotted_total_effect_veg_path_post.Rdata")); rm(post); gc()

# Get posterior samples for the estimands
df_total_veg_path_baboon <- as.data.frame(cbind(post_total_veg_path_baboon$beta_opuntia, post_total_veg_path_baboon$beta_fruit, post_total_veg_path_baboon$k_bar))
df_total_veg_path_elephant <- as.data.frame(cbind(post_total_veg_path_elephant$beta_opuntia, post_total_veg_path_elephant$beta_fruit, post_total_veg_path_elephant$k_bar))
df_total_veg_path_vervetmonkey <- as.data.frame(cbind(post_total_veg_path_vervetmonkey$beta_opuntia, post_total_veg_path_vervetmonkey$beta_fruit, post_total_veg_path_vervetmonkey$k_bar))
df_total_veg_path_zebragrevys <- as.data.frame(cbind(post_total_veg_path_zebragrevys$beta_opuntia, post_total_veg_path_zebragrevys$beta_fruit, post_total_veg_path_zebragrevys$k_bar))
df_total_veg_path_impala <- as.data.frame(cbind(post_total_veg_path_impala$beta_opuntia, post_total_veg_path_impala$beta_fruit, post_total_veg_path_impala$k_bar))
df_total_veg_path_dikdik <- as.data.frame(cbind(post_total_veg_path_dikdik$beta_opuntia, post_total_veg_path_dikdik$beta_fruit, post_total_veg_path_dikdik$k_bar))
df_total_veg_path_giraffe <- as.data.frame(cbind(post_total_veg_path_giraffe$beta_opuntia, post_total_veg_path_giraffe$beta_fruit, post_total_veg_path_giraffe$k_bar))
df_total_veg_path_hyenaspotted <- as.data.frame(cbind(post_total_veg_path_hyenaspotted$beta_opuntia, post_total_veg_path_hyenaspotted$beta_fruit, post_total_veg_path_hyenaspotted$k_bar))

# Remove full posterior distributions to save memory
rm(post_total_veg_path_baboon)
rm(post_total_veg_path_elephant)
rm(post_total_veg_path_vervetmonkey)
rm(post_total_veg_path_zebragrevys)
rm(post_total_veg_path_impala)
rm(post_total_veg_path_dikdik)
rm(post_total_veg_path_giraffe)
rm(post_total_veg_path_hyenaspotted)
gc()

# Add species column
df_total_veg_path_baboon$species <- as.factor("baboon")
df_total_veg_path_elephant$species <- as.factor("elephant")
df_total_veg_path_vervetmonkey$species <- as.factor("vervetmonkey")
df_total_veg_path_zebragrevys$species <- as.factor("zebragrevys")
df_total_veg_path_impala$species <- as.factor("impala")
df_total_veg_path_dikdik$species <- as.factor("dikdik")
df_total_veg_path_giraffe$species <- as.factor("giraffe")
df_total_veg_path_hyenaspotted$species <- as.factor("hyenaspotted")

colnames(df_total_veg_path_baboon) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_elephant) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_vervetmonkey) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_zebragrevys) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_impala) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_dikdik) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_giraffe) <- c("beta_total2", "k_bar3", "species")
colnames(df_total_veg_path_hyenaspotted) <- c("beta_total2", "k_bar3", "species")

# Bind into one big dataframe
beta_total_veg_path_df <- rbind(df_total_veg_path_baboon, 
                                df_total_veg_path_elephant,
                                df_total_veg_path_vervetmonkey,
                                df_total_veg_path_zebragrevys,
                                df_total_veg_path_impala,
                                df_total_veg_path_dikdik,
                                df_total_veg_path_giraffe,
                                df_total_veg_path_hyenaspotted)

# Remove the separate df's to save memory
rm(df_total_veg_path_baboon)
rm(df_total_veg_path_elephant)
rm(df_total_veg_path_giraffe)
rm(df_total_veg_path_hyenaspotted)
rm(df_total_veg_path_impala)
rm(df_total_veg_path_dikdik)
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
            "dikdik",
            "giraffe",
            "hyenaspotted")

species_names <- c("Olive baboon",
                   "Elephant",
                   "Vervet monkey",
                   "Grevy's zebra",
                   "Impala", 
                   "Dik-dik",
                   "Giraffe",
                   "Spotted hyena")

plot_titles <- c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)")

# Colours for shading CI's for each species
#species_colours <- viridis(7)
species_colours <- rep("#35B779FF", 8)
colouralpha <- 0.4

# Open new graphics device to save as TIFF
#par(mfrow=c(2,4))
#pr <- par()
setwd("C:/Users/PeteS/OneDrive/Durham/Occupancy chapter")
tiff("grid_square_total_novegpath.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)

#par(mfrow=c(length(key_sp),1), mgp=c(2,1,0), mar=c(3.5, 3.9, 1.5, 0.1))
xseq <- seq(-1.554, 3.585, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- beta_all_df %>% filter(species == key_sp[i])
  
  # Calculate marginal effects using k_bar and the beta parameters
  p1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq)) # When fruit = -1 
  p2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq)) # When fruit = 2.5
  p3 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p4 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p1[,x] <- inv_logit(s$k_bar1 + s$beta_opuntia*xseq[x] + s$beta_fruit*-1 + s$beta_opuntia_fruit*xseq[x]*-1)  # When fruit = -1
    p2[,x] <- inv_logit(s$k_bar1 + s$beta_opuntia*xseq[x] + s$beta_fruit*2.5 + s$beta_opuntia_fruit*xseq[x]*2.5) # When fruit = 2.5
    p3[,x] <- inv_logit(s$k_bar2 + s$beta_total1*xseq[x])
    p4[,x] <- inv_logit(s$k_bar3 + s$beta_total2*xseq[x])
  }
  
  
  # Calculate mean and CI's
  mu1 <- apply(p1, 2, median)
  PI95_1 <- apply(p1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p1, 2, HPDI, prob=0.89)
  PI80_1 <- apply(p1, 2, HPDI, prob=0.80)
  PI70_1 <- apply(p1, 2, HPDI, prob=0.70)
  PI60_1 <- apply(p1, 2, HPDI, prob=0.60)
  PI50_1 <- apply(p1, 2, HPDI, prob=0.50)
  PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p2, 2, median)
  PI95_2 <- apply(p2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p2, 2, HPDI, prob=0.89)
  PI80_2 <- apply(p2, 2, HPDI, prob=0.80)
  PI70_2 <- apply(p2, 2, HPDI, prob=0.70)
  PI60_2 <- apply(p2, 2, HPDI, prob=0.60)
  PI50_2 <- apply(p2, 2, HPDI, prob=0.50)
  PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  mu3 <- apply(p3, 2, median)
  PI95_3 <- apply(p3, 2, HPDI, prob=0.95)
  PI89_3 <- apply(p3, 2, HPDI, prob=0.89)
  PI80_3 <- apply(p3, 2, HPDI, prob=0.80)
  PI70_3 <- apply(p3, 2, HPDI, prob=0.70)
  PI60_3 <- apply(p3, 2, HPDI, prob=0.60)
  PI50_3 <- apply(p3, 2, HPDI, prob=0.50)
  PI_all_3 <- rbind(PI95_3, PI89_3, PI80_3, PI70_3, PI60_3, PI50_3)
  
  mu4 <- apply(p4, 2, median)
  PI95_4 <- apply(p4, 2, HPDI, prob=0.95)
  PI89_4 <- apply(p4, 2, HPDI, prob=0.89)
  PI80_4 <- apply(p4, 2, HPDI, prob=0.80)
  PI70_4 <- apply(p4, 2, HPDI, prob=0.70)
  PI60_4 <- apply(p4, 2, HPDI, prob=0.60)
  PI50_4 <- apply(p4, 2, HPDI, prob=0.50)
  PI_all_4 <- rbind(PI95_4, PI89_4, PI80_4, PI70_4, PI60_4, PI50_4)
  
  # For p1 and p2 eliminate cover/fruit combos which don't exist in data
  id1 <- which(xseq < -0.7575 | xseq > 1.3098)
  id2 <- which(xseq < -0.65416 | xseq > 4.2039)
  
  xseq1 <- xseq
  xseq2 <- xseq
  
  xseq1[id1] <- NA
  xseq2[id2] <- NA
  
  xseq1 <- xseq1[!is.na(xseq1)]
  xseq2 <- xseq2[!is.na(xseq2)]
  
  mu1[id1] <- NA
  mu1 <- mu1[!is.na(mu1)]
  PI95_1[,id1] <- NA
  PI89_1[,id1] <- NA
  PI80_1[,id1] <- NA
  PI70_1[,id1] <- NA
  PI60_1[,id1] <- NA
  PI50_1[,id1] <- NA
  PI95_1 <- PI95_1[, !apply(is.na(PI95_1), 2, any)]
  PI89_1 <- PI89_1[, !apply(is.na(PI89_1), 2, any)]
  PI80_1 <- PI80_1[, !apply(is.na(PI80_1), 2, any)]
  PI70_1 <- PI70_1[, !apply(is.na(PI70_1), 2, any)]
  PI60_1 <- PI60_1[, !apply(is.na(PI60_1), 2, any)]
  PI50_1 <- PI50_1[, !apply(is.na(PI50_1), 2, any)]
  
  
  mu2[id2] <- NA
  mu2 <- mu2[!is.na(mu2)]
  PI95_2[,id2] <- NA
  PI89_2[,id2] <- NA
  PI80_2[,id2] <- NA
  PI70_2[,id2] <- NA
  PI60_2[,id2] <- NA
  PI50_2[,id2] <- NA
  PI95_2 <- PI95_2[, !apply(is.na(PI95_2), 2, any)]
  PI89_2 <- PI89_2[, !apply(is.na(PI89_2), 2, any)]
  PI80_2 <- PI80_2[, !apply(is.na(PI80_2), 2, any)]
  PI70_2 <- PI70_2[, !apply(is.na(PI70_2), 2, any)]
  PI60_2 <- PI60_2[, !apply(is.na(PI60_2), 2, any)]
  PI50_2 <- PI50_2[, !apply(is.na(PI50_2), 2, any)]
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = expression(psi),
       xlab="Opuntia grid square vol.", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  # Direct effects
  #shade(PI95_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  #shade(PI89_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  #shade(PI80_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  #shade(PI70_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  #shade(PI60_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  #shade(PI50_1, xseq1, col=col.alpha("#35B779FF", colouralpha))
  
  #shade(PI95_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  #shade(PI89_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  #shade(PI80_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  #shade(PI70_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  #shade(PI60_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  #shade(PI50_2, xseq2, col=col.alpha("#443A83FF", colouralpha))
  
  #points(x = xseq1, y = mu1, type="l", lwd=2, col = "#21908CFF")
  #points(x = xseq2, y = mu2, type="l", lwd=2, col = "#443A83FF")
  
  # Total effects no veg. path
  shade(PI95_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI89_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI80_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI70_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI60_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  shade(PI50_3, xseq, col=col.alpha(species_colours[i], colouralpha))
  points(x = xseq, y = mu3, type="l", lwd=2)
  
  # Total effects veg. path
  #shade(PI95_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI89_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI80_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI70_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI60_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #shade(PI50_4, xseq, col=col.alpha(species_colours[i], colouralpha))
  #points(x = xseq, y = mu4, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Activity analysis ####
# For all key species, plot activity kernel for each and make 1 plot to compare overall activity levels
site_data$Pair <- as.factor(site_data$Pair)
dd <- site_data %>% select(Pair, grid_square, Site_ID)
high_sites <- dd %>% filter(Pair == "high")
low_sites <- dd %>% filter(Pair == "low")
high_sitenames <- matrix(NA, nrow = nrow(high_sites), 1)
low_sitenames <- matrix(NA, nrow = nrow(low_sites), 1)

for(i in 1:nrow(high_sites)){
  site_prefix <- NULL
  if(high_sites[i,3] < 10){
    site_prefix <- "Site_0"
  }else{
    site_prefix <- "Site_"
  }
  high_sitenames[i,1] <- paste0(site_prefix, high_sites[i,3])
}

for(i in 1:nrow(low_sites)){
  site_prefix <- NULL
  if(low_sites[i,3] < 10){
    site_prefix <- "Site_0"
  }else{
    site_prefix <- "Site_"
  }
  low_sitenames[i,1] <- paste0(site_prefix, low_sites[i,3])
}

# Baboon 
df1 <- consensus_classifications %>% filter(species == "baboon") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "baboon") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m1 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m2 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Elephant 
df1 <- consensus_classifications %>% filter(species == "elephant") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "elephant") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m3 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m4 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Vervet monkey
df1 <- consensus_classifications %>% filter(species == "vervetmonkey") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "vervetmonkey") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m5 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m6 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Grevy's zebra
df1 <- consensus_classifications %>% filter(species == "zebragrevys") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "zebragrevys") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m7 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m8 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Impala
df1 <- consensus_classifications %>% filter(species == "impala") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "impala") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m9 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m10 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)

# Giraffe
df1 <- consensus_classifications %>% filter(species == "giraffe") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "giraffe") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m11 <- fitact(dat = t_rad1,
              sample = "data",
              reps = 1000,
              show = TRUE)
m12 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)


# Spotted hyena
df1 <- consensus_classifications %>% filter(species == "hyenaspotted") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "hyenaspotted") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m13 <- fitact(dat = t_rad1,
              sample = "data",
              reps = 1000,
              show = TRUE)
m14 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)

# Dik-dik
df1 <- consensus_classifications %>% filter(species == "dikdik") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "dikdik") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m15 <- fitact(dat = t_rad1,
              sample = "data",
              reps = 1000,
              show = TRUE)
m16 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)

# Save results
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data/activity_analysis_output")
save(m1, file = "baboon_high.Rdata")
save(m2, file = "baboon_low.Rdata")
save(m3, file = "elephant_high.Rdata")
save(m4, file = "elephant_low.Rdata")
save(m5, file = "vervet_high.Rdata")
save(m6, file = "vervet_low.Rdata")
save(m7, file = "zebragrevys_high.Rdata")
save(m8, file = "zebragrevys_low.Rdata")
save(m9, file = "impala_high.Rdata")
save(m10, file = "impala_low.Rdata")
save(m11, file = "giraffe_high.Rdata")
save(m12, file = "giraffe_low.Rdata")
save(m13, file = "hyenaspotted_high.Rdata")
save(m14, file = "hyenaspotted_low.Rdata")
save(m15, file = "dikdik_high.Rdata")
save(m16, file = "dikdik_low.Rdata")

# Loading saved data 
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data/activity_analysis_output")
m1 <- get(load("baboon_high.Rdata"))
m2 <- get(load("baboon_low.Rdata"))
m3 <- get(load("elephant_high.Rdata"))
m4 <- get(load("elephant_low.Rdata"))
m5 <- get(load("vervet_high.Rdata"))
m6 <- get(load("vervet_low.Rdata"))
m7 <- get(load("zebragrevys_high.Rdata"))
m8 <- get(load("zebragrevys_low.Rdata"))
m9 <- get(load("impala_high.Rdata"))
m10 <- get(load("impala_low.Rdata"))
m11 <- get(load("giraffe_high.Rdata"))
m12 <- get(load("giraffe_low.Rdata"))
m13 <- get(load("hyenaspotted_high.Rdata"))
m14 <- get(load("hyenaspotted_low.Rdata"))
m15 <- get(load("dikdik_high.Rdata"))
m16 <- get(load("dikdik_low.Rdata"))

# Plots
# Activity comparison
c1 <- compareAct(c(m2,m1))
c2 <- compareAct(c(m4,m3))
c3 <- compareAct(c(m6,m5))
c4 <- compareAct(c(m8,m7))
c5 <- compareAct(c(m10,m9))
c6 <- compareAct(c(m12,m11))
c7 <- compareAct(c(m14,m13))
c8 <- compareAct(c(m16,m15))

plot(NULL, xlim = c(1,7), ylim = c(-0.5,0.5), xlab = "Species", ylab = "Activity difference when Opuntia is high", xaxt="n")
axis(1, 
     at = 1:7,
     labels = c("Olive baboon","Elephant", "Vervet monkey", "Grevy's zebra", "Impala", "Dik-dik", "Giraffe", "Spotted hyena"))
points(x = 1, y = c1[1,1], pch=16); lines(x = c(1,1), y = c(c1[1,1]+c1[1,2],c1[1,1]-c1[1,2]))
points(x = 2, y = c2[1,1], pch=16); lines(x = c(2,2), y = c(c2[1,1]+c2[1,2],c2[1,1]-c2[1,2]))
points(x = 3, y = c3[1,1], pch=16); lines(x = c(3,3), y = c(c3[1,1]+c3[1,2],c3[1,1]-c3[1,2]))
points(x = 4, y = c4[1,1], pch=16); lines(x = c(4,4), y = c(c4[1,1]+c4[1,2],c4[1,1]-c4[1,2]))
points(x = 4, y = c8[1,1], pch=16); lines(x = c(4,4), y = c(c8[1,1]+c8[1,2],c8[1,1]-c8[1,2]))
points(x = 5, y = c5[1,1], pch=16); lines(x = c(5,5), y = c(c5[1,1]+c5[1,2],c5[1,1]-c5[1,2]))
points(x = 6, y = c6[1,1], pch=16); lines(x = c(6,6), y = c(c6[1,1]+c6[1,2],c6[1,1]-c6[1,2]))
points(x = 7, y = c7[1,1], pch=16); lines(x = c(7,7), y = c(c7[1,1]+c7[1,2],c7[1,1]-c7[1,2]))
abline(h=0, lty=2)

# Activity plots for each species
#par(mfrow=c(2,4))
#pr <- par()
setwd("C:/Users/PeteS/OneDrive/Durham/Occupancy chapter")
tiff("activity_plots_new.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)
# Baboon
clean_activity_plot(m1, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("A)", adj=0, line = 0.7)
clean_activity_plot(m2, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Elephant 
clean_activity_plot(m3, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("B)", adj=0, line = 0.7)
clean_activity_plot(m4, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Vervet monkey 
clean_activity_plot(m5, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("C)", adj=0, line = 0.7)
clean_activity_plot(m6, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Grevy's zebra
clean_activity_plot(m7, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("D)", adj=0, line = 0.7)
clean_activity_plot(m8, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Impala
clean_activity_plot(m9, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("E)", adj=0, line = 0.7)
clean_activity_plot(m10, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Dik-dik
clean_activity_plot(m15, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("F)", adj=0, line = 0.7)
clean_activity_plot(m16, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Giraffe
clean_activity_plot(m11, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("G)", adj=0, line = 0.7)
clean_activity_plot(m12, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Spotted hyena
clean_activity_plot(m13, 
                    species_title = "",
                    colour = "darkgreen",
                    alpha = 0.5)
title("H)", adj=0, line = 0.7)
clean_activity_plot(m14, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

dev.off()

# Activity analysis split by site ####
site_list <- unique(consensus_classifications$site)

key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "dikdik",
            "dikdik",
            "giraffe",
            "hyenaspotted")

results_list <- list()

for(s in 1:length(key_sp)){
  act_results <- matrix(NA, nrow=length(site_list), ncol = 4)
  for(i in 1:length(site_list)){
    df <- consensus_classifications %>% filter(species == key_sp[s])
    df_sub <- df %>% filter(site == site_list[i])
    if(nrow(df_sub) == 0){
      act_results[i,] <- 0
    }else{
      times1 <- df_sub$DateTimeLub
      t_rad1 <- gettime(x = times1,
                        scale = "radian")
      m1 <- fitact(dat = t_rad1,
                   sample = "data",
                   reps = 1000,
                   show = TRUE)
      save(m1, file = paste0(key_sp[s],"_",site_list[i],".Rdata"))
      act_results[i,] <- m1@act
      rm(m1); rm(t_rad1); rm(times1)
    }
    results_list[[s]] <- act_results
  }
}

names(results_list) <- key_sp

# Save activity results for each site
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data/activity_analysis_output")
save(results_list, file = "activity_sitelevel_all.Rdata")

# Load activity results for each site
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data/activity_analysis_output")
results_list <- get(load("activity_sitelevel_all_new.Rdata"))

# Parameters for running model
key_sp <- "dikdik"
model_list <- c("total_novegpath","total_vegpath") # List of models to run
n_chains <- 4 # Number of chains
n_cores <- 4 # Number of computer cores
n_warmup <- 3000 # Number of warmup iterations per chain
n_iter <- 4000 # Total number of iterations (warmup + sample) per chain

# Fit hurdle model for each species - fine-scale
dmat <- generate_distance_matrix(site_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

for(m in 1:length(model_list)){
  setwd(paste0("C:/Users/PeteS/OneDrive/Durham/PhD Data/activity_analysis_output/fine_scale/",model_list[m]))
  for(sp in 1:length(key_sp)){
    act_results <- results_list[key_sp[sp]]
    act_results <- as.data.frame(act_results)
    colnames(act_results) <- c("V1","V2","V3","V4")
    act_results <- cbind(site_list, act_results)
    
    act_results$Site_ID <- as.numeric(gsub("Site_", "", act_results$site_list))
    act_results2 <- merge(act_results, site_data, by="Site_ID", all.y = TRUE)
    
    act_results2$V1[is.na(act_results2$V1)] <- 0
    
    dlist <- list(
      n_obs = nrow(act_results2),
      y_obs = act_results2$V1*10,
      opuntia = standardize(act_results2$opuntia_total_cover),
      surveys = standardize(sitedays$Days),
      d_water = standardize(act_results2$dist_river),
      d_road = standardize(act_results2$dist_road),
      livestock = standardize(act_results2$livestock_proportion),
      grass = standardize(act_results2$grass_total),
      forb = standardize(act_results2$forb_total),
      shrub = standardize(act_results2$shrub_total),
      succulent = standardize(act_results2$succulent_total),
      tree = standardize(act_results2$n_trees),
      dmat = dmat)
    
    m1 <- cstan(file = paste0("C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/hurdle_test/hurdle_",model_list[m],".stan"),
                data = dlist,
                chains = n_chains, 
                cores = n_cores,
                warmup = n_warmup,
                iter = n_iter)
    
    # Save diagnostic plots
    png(file = paste0(key_sp[sp],"_",model_list[m],"_diagnostics.png"), width = 804, height = 500, units = "px")
    dashboard(m1)
    dev.off()
    
    # Save posterior samples
    post <- extract.samples(m1)
    save(post, file = paste0(key_sp[sp],"_hurdle_",model_list[m],".Rdata"))
    
    # Parameters to save in traceplots and trankplots
    p <- names(post)[grep("beta", names(post))] # Beta parameters
    p2 <- names(post)[grep("gamma", names(post))] # Gamma parameters
    p3 <- c("sigma", "etasq", "rhosq","etasq2", "rhosq2",  "k_bar","omega_bar") # Other parameters
    
    # Save traceplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_traceplots.png"), width = 804, height = 500, units = "px")
    p1 <- rstan::traceplot(m1, pars=c(p, p2, p3), inc_warmup = TRUE)
    print(p1)
    dev.off()
    
    # Save trankplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_trankplots.png"), width = 804, height = 500, units = "px")
    trankplot(m1, pars=p3)
    dev.off()
    
    # Save hist of centred marginal energy distribution and first-differenced distribution overlaid
    color_scheme_set("darkgray") # Set colour scheme for Bayesplot 
    np <- nuts_params(m1) # Extract NUTS parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_HMC_energy.png"), width = 804, height = 500, units = "px")
    p2 <- mcmc_nuts_energy(np)
    print(p2)
    dev.off()
    
    # Clean up between iterations
    rm(post)
    rm(np)
    rm(p1)
    rm(p2)
    rm(m1)
    rm(dlist)
    rm(act_results2)
    rm(act_results)
    gc()
  }
}

# Fit hurdle model for each species - grid square-scale
grid_data <- site_data %>% filter(!is.na(volume_total))
sitedays_grid <- sitedays %>% filter(Site %in% grid_data$Site_ID)

dmat <- generate_distance_matrix(grid_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

for(m in 1:length(model_list)){
  setwd(paste0("C:/Users/PeteS/OneDrive/Durham/PhD Data/activity_analysis_output/grid_square/",model_list[m]))
  for(sp in 1:length(key_sp)){
    act_results <- results_list[key_sp[sp]]
    act_results <- as.data.frame(act_results)
    colnames(act_results) <- c("V1","V2","V3","V4")
    act_results <- cbind(site_list, act_results)
    
    act_results$Site_ID <- as.numeric(gsub("Site_", "", act_results$site_list))
    act_results2 <- merge(act_results, site_data, by="Site_ID", all.y = TRUE)
    
    act_results2$V1[is.na(act_results2$V1)] <- 0
    
    act_results2$volume_total <- as.numeric(act_results2$volume_total)
    act_results2 <- act_results2[!is.na(act_results2$volume_total),]
    
    dlist <- list(
      n_obs = nrow(act_results2),
      y_obs = act_results2$V1*10,
      opuntia = standardize(act_results2$opuntia_total_cover),
      surveys = standardize(sitedays_grid$Days),
      d_water = standardize(act_results2$dist_river),
      d_road = standardize(act_results2$dist_road),
      livestock = standardize(act_results2$livestock_proportion),
      grass = standardize(act_results2$grass_total),
      forb = standardize(act_results2$forb_total),
      shrub = standardize(act_results2$shrub_total),
      succulent = standardize(act_results2$succulent_total),
      tree = standardize(act_results2$n_trees),
      dmat = dmat)
    
    m1 <- cstan(file = paste0("C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/hurdle_test/hurdle_",model_list[m],".stan"),
                data = dlist,
                chains = n_chains, 
                cores = n_cores,
                warmup = n_warmup,
                iter = n_iter)
    
    # Save diagnostic plots
    png(file = paste0(key_sp[sp],"_",model_list[m],"_diagnostics.png"), width = 804, height = 500, units = "px")
    dashboard(m1)
    dev.off()
    
    # Save posterior samples
    post <- extract.samples(m1)
    save(post, file = paste0(key_sp[sp],"_hurdle_",model_list[m],".Rdata"))
    
    # Parameters to save in traceplots and trankplots
    p <- names(post)[grep("beta", names(post))] # Beta parameters
    p2 <- names(post)[grep("gamma", names(post))] # Gamma parameters
    p3 <- c("sigma", "etasq", "rhosq","etasq2", "rhosq2",  "k_bar","omega_bar") # Other parameters
    
    # Save traceplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_traceplots.png"), width = 804, height = 500, units = "px")
    p1 <- rstan::traceplot(m1, pars=c(p, p2, p3), inc_warmup = TRUE)
    print(p1)
    dev.off()
    
    # Save trankplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_trankplots.png"), width = 804, height = 500, units = "px")
    trankplot(m1, pars=p3)
    dev.off()
    
    # Save hist of centred marginal energy distribution and first-differenced distribution overlaid
    color_scheme_set("darkgray") # Set colour scheme for Bayesplot 
    np <- nuts_params(m1) # Extract NUTS parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_HMC_energy.png"), width = 804, height = 500, units = "px")
    p2 <- mcmc_nuts_energy(np)
    print(p2)
    dev.off()
    
    # Clean up between iterations
    rm(post)
    rm(np)
    rm(p1)
    rm(p2)
    rm(m1)
    rm(dlist)
    rm(act_results2)
    rm(act_results)
    gc()
  }
}

# Hurdle model plots for all species in a loop ####
# Species names
key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "dikdik",
            "giraffe",
            "hyenaspotted")

plot_titles <- c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)")

# Colours for shading CI's for each species
#species_colours <- viridis(7)
species_colours <- rep("#35B779FF", 8)
colouralpha <- 0.4

# Open new graphics device to save as TIFF
#par(mfrow=c(2,4))
#pr <- par()
setwd("C:/Users/PeteS/OneDrive/Durham/PhD Data/activity_analysis_output/fine_scale/total_novegpath")
tiff("hurdle_plots_test3.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)

# Plot loop
for(sp in 1:length(key_sp)){
  
  act_results <- results_list[key_sp[sp]]
  act_results <- as.data.frame(act_results)
  colnames(act_results) <- c("V1","V2","V3","V4")
  act_results <- cbind(site_list, act_results)
  
  act_results$Site_ID <- as.numeric(gsub("Site_", "", act_results$site_list))
  act_results2 <- merge(act_results, site_data, by="Site_ID", all.y = TRUE)
  
  act_results2$V1[is.na(act_results2$V1)] <- 0
  
  dlist <- list(
    n_obs = nrow(act_results2),
    y_obs = act_results2$V1*10,
    opuntia = standardize(act_results2$opuntia_total_cover),
    surveys = standardize(sitedays$Days),
    d_water = standardize(act_results2$dist_river),
    d_road = standardize(act_results2$dist_road),
    livestock = standardize(act_results2$livestock_proportion),
    grass = standardize(act_results2$grass_total),
    forb = standardize(act_results2$forb_total),
    shrub = standardize(act_results2$shrub_total),
    succulent = standardize(act_results2$succulent_total),
    tree = standardize(act_results2$n_trees),
    dmat = dmat
  )
  
  post <- get(load(paste0(key_sp[sp],"_hurdle_total_novegpath.Rdata")))
  
  x_seq <- seq(min(dlist$opuntia), max(dlist$opuntia), by = 0.01)
  y_sim <- matrix(NA, nrow = length(x_seq), ncol=length(post$beta_opuntia))
  for(i in 1:length(x_seq)){
    y_sim[i,] <- rbinom(n = length(post$beta_opuntia), 
                        size = 1,
                        prob = 1 - (inv_logit(post$omega_bar + post$gamma_opuntia*x_seq[i])))*rlnorm(n = length(post$beta_opuntia), 
                                                                                                     meanlog = post$k_bar + post$beta_opuntia*x_seq[i], 
                                                                                                     sdlog = post$sigma)
  }
  
  y_sim_mu <- apply(y_sim, 1, mean)
  y_sim_mu2 <- apply(y_sim, 1, median)
  y_sim_ci <- apply(y_sim, 1, HPDI, prob = 0.95)
  y_sim_ci2 <- apply(y_sim, 1, HPDI, prob = 0.89)
  y_sim_ci3 <- apply(y_sim, 1, HPDI, prob = 0.90)
  y_sim_ci4 <- apply(y_sim, 1, HPDI, prob = 0.80)
  y_sim_ci5 <- apply(y_sim, 1, HPDI, prob = 0.70)
  y_sim_ci6 <- apply(y_sim, 1, HPDI, prob = 0.60)
  y_sim_ci7 <- apply(y_sim, 1, HPDI, prob = 0.50)
  
  plot( NULL , xlim=range(x_seq) , ylim=c(0,max(dlist$y_obs+1)) , 
        xlab="Opuntia" , 
        ylab="Activity (rescaled)", 
        main = "")
  title(paste(plot_titles[sp]), adj=0, line = 0.7)
  #lines(x_seq, y_sim_mu, lwd=2, lty=2)
  shade(y_sim_ci, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci2, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci3, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci4, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci5, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci6, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci7, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  lines(x_seq, y_sim_mu2, lwd=2, lty=1)
  points(x = dlist$opuntia, y = dlist$y_obs, pch = 16)
}
dev.off() # Close graphics device
