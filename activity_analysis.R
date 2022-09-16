# First get list of sites which are high Opuntia density
dd <- site_data %>% group_by(grid_square) %>% summarise(opuntia_total_cover = max(opuntia_total_cover))
ds <- site_data %>% select(opuntia_total_cover, grid_square, Site_ID)
di <- merge(dd, ds, by=c("grid_square", "opuntia_total_cover"))
high_sites <- di$Site_ID
high_sitenames <- matrix(NA, nrow = length(high_sites), 1)
for(i in 1:length(high_sites)){
  site_prefix <- NULL
  if(high_sites[i] < 10){
    site_prefix <- "Site_0"
  }else{
    site_prefix <- "Site_"
  }
  high_sitenames[i,1] <- paste0(site_prefix, high_sites[i])
}

# Now loop through all key species and run model 
key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "giraffe",
            "hyenaspotted")

results_list_m1 <- list()
results_list_m2 <- list()

for(sp in 1:length(key_sp)){
  df1 <- consensus_classifications %>% filter(species == key_sp[sp]) %>% filter(site %in% high_sitenames)
  df2 <- consensus_classifications %>% filter(species == key_sp[sp]) %>% filter(site %notin% high_sitenames)
  
  times1 <- df1$DateTimeLub
  times2 <- df2$DateTimeLub
  
  t_rad1 <- gettime(x = times1,
                    scale = "radian")
  t_rad2 <- gettime(x = times2,
                    scale = "radian")
  
  m1 <- fitact(dat = t_rad1,
               sample = "data",
               reps = 1000,
               show = TRUE
  )
  m2 <- fitact(dat = t_rad2,
               sample = "data",
               reps = 1000,
               show = TRUE
  )
  results_list_m1[[i]] <- m1
  results_list_m2[[i]] <- m2
}
names(results_list_m1) <- key_sp
names(results_list_m2) <- key_sp

# Plot activity kernel for each species and make 1 plot to compare overall activity levels
