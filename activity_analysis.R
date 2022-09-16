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

# Baboon 
df1 <- consensus_classifications %>% filter(species == "baboon") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "baboon") %>% filter(site %notin% high_sitenames)
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
df2 <- consensus_classifications %>% filter(species == "elephant") %>% filter(site %notin% high_sitenames)
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
df2 <- consensus_classifications %>% filter(species == "vervetmonkey") %>% filter(site %notin% high_sitenames)
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
df2 <- consensus_classifications %>% filter(species == "zebragrevys") %>% filter(site %notin% high_sitenames)
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
df2 <- consensus_classifications %>% filter(species == "impala") %>% filter(site %notin% high_sitenames)
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
df2 <- consensus_classifications %>% filter(species == "giraffe") %>% filter(site %notin% high_sitenames)
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
df2 <- consensus_classifications %>% filter(species == "hyenaspotted") %>% filter(site %notin% high_sitenames)
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


# Plots
# Activity comparison
c1 <- compareAct(c(m1,m2))
c2 <- compareAct(c(m3,m4))
c3 <- compareAct(c(m5,m6))
c4 <- compareAct(c(m7,m8))
c5 <- compareAct(c(m9,m10))
c6 <- compareAct(c(m11,m12))
c7 <- compareAct(c(m13,m14))

plot(NULL, xlim = c(1,7), ylim = c(-0.5,0.5), xlab = "Species", ylab = "Change in activity", xaxt="n")
axis(1, 
     at = 1:7,
     labels = c("Olive baboon","Elephant", "Vervet monkey", "Grevy's zebra", "Impala", "Giraffe", "Spotted hyena"))
points(x = 1, y = c1[1,1], pch=16); lines(x = c(1,1), y = c(c1[1,1]+c1[1,2],c1[1,1]-c1[1,2]))
points(x = 2, y = c2[1,1], pch=16); lines(x = c(2,2), y = c(c2[1,1]+c2[1,2],c2[1,1]-c2[1,2]))
points(x = 3, y = c3[1,1], pch=16); lines(x = c(3,3), y = c(c3[1,1]+c3[1,2],c3[1,1]-c3[1,2]))
points(x = 4, y = c4[1,1], pch=16); lines(x = c(4,4), y = c(c4[1,1]+c4[1,2],c4[1,1]-c4[1,2]))
points(x = 5, y = c5[1,1], pch=16); lines(x = c(5,5), y = c(c5[1,1]+c5[1,2],c5[1,1]-c5[1,2]))
points(x = 6, y = c6[1,1], pch=16); lines(x = c(6,6), y = c(c6[1,1]+c6[1,2],c6[1,1]-c6[1,2]))
points(x = 7, y = c7[1,1], pch=16); lines(x = c(7,7), y = c(c7[1,1]+c7[1,2],c7[1,1]-c7[1,2]))
abline(h=0, lty=2)

# Activity plots for each species
# Baboon
clean_activity_plot(m1, 
                    species_title = "Olive baboon",
                    colour = "darkgreen",
                    alpha = 0.5)
clean_activity_plot(m2, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Elephant 
clean_activity_plot(m3, 
                    species_title = "Elephant",
                    colour = "darkgreen",
                    alpha = 0.5)
clean_activity_plot(m4, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Vervet monkey 
clean_activity_plot(m5, 
                    species_title = "Vervet monkey",
                    colour = "darkgreen",
                    alpha = 0.5)
clean_activity_plot(m6, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Grevy's zebra
clean_activity_plot(m7, 
                    species_title = "Grevy's zebra",
                    colour = "darkgreen",
                    alpha = 0.5)
clean_activity_plot(m8, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Impala
clean_activity_plot(m9, 
                    species_title = "Impala",
                    colour = "darkgreen",
                    alpha = 0.5)
clean_activity_plot(m10, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Giraffe
clean_activity_plot(m11, 
                    species_title = "Giraffe",
                    colour = "darkgreen",
                    alpha = 0.5)
clean_activity_plot(m12, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)

# Spotted hyena
clean_activity_plot(m13, 
                    species_title = "Spotted hyena",
                    colour = "darkgreen",
                    alpha = 0.5)
clean_activity_plot(m14, 
                    colour = "lightgreen",
                    alpha = 0.5, 
                    add = TRUE)
