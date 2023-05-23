# Function to make a nice clean plot of activity kernel ####
clean_activity_plot <- function(mod, species_title = "", colour = "black", alpha = 0.2, add = FALSE, lwd = 1.7){
  if(add == FALSE){
    plot(NULL, 
         ylim = c(0, max(mod@pdf[,5]+0.1)), 
         xlim = c(0, max(mod@pdf[,1])), 
         ylab = "Density", 
         xlab = "Time", 
         xaxt = "n", 
         main = species_title)
    axis(1, 
         at = c(0, 0.5*pi, pi, 1.5*pi, 2*pi),
         labels = c("00:00","06:00", "12:00", "18:00", "24:00"))
  }
  points(x = mod@pdf[,1], y = mod@pdf[,2], type = "l", lwd=lwd)
  modci <- t(mod@pdf[,4:5])
  shade(modci, mod@pdf[,1], col=col.alpha(colour, alpha))
}

# Expands a dataframe so that sites are sitedays (i.e. each site is one day at one site, each visit is one hour) ####
expand_to_sitedays <- function(df, remove_sitedays_column = FALSE, rownames = FALSE){
  # Stop and warn if no sitedays dataframe
  if(!exists("sitedays")){
    stop("Please load the sitedays dataframe")
  }
  
  # Main body of function
  sites <- df %>% select(contains("site") & !contains("description"))
  colnames(sites) <- c("Site")
  df <- df %>% rename(Site = contains("site") & !contains("description"))
  df <- as.data.frame(df)
  df <- merge(df, sitedays, by = "Site")
  ff <- df %>% uncount(Days)
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
  df <- ff %>% select(-Site, -gg)
  
  # Optionally change rownames to sitedays (default is FALSE)
  if(rownames == TRUE){
    rownames(df) <- df$site_day
  }
  
  # Optionally remove the sitedays column (default is FALSE)
  if(remove_sitedays_column == TRUE){
    df <- df %>% select(-site_day)
  }
  return(df)
}

# Function to generate detection matrix where columns are days ####
generate_detection_matrix_days <- function(sp, binary = FALSE, sites_as_integers = TRUE){
  
  # Check if consensus classifications dataframe exists, stop and warn if not
  if(!exists("consensus_classifications")){
    stop("Please load the consensus_classifications dataframe")
  }
  
  # Check if startends dataframe exists, stop and warn if not
  if(!exists("startends")){
    stop("Please load the startends dataframe")
  }
  
  # Create sitedays dataframe if it doesn't already exist
  if(!exists("sitedays")){
    warning("sitedays not found - creating from startends")
    sitedays <- startends %>% select(Site, Days) %>% filter(!is.na(Days))
  }
  
  # Subset to the correct species
  df <- consensus_classifications %>% filter(species==sp)
  
  # Warn and return NA if species was never observed 
  if(nrow(df)==0){
    warning(paste(sp, "was never observed"))
    return(NA)
  }
  
  # Add indicator for use later
  df$indicator <- 1L
  
  # Prepare startends dataframe into sites_k
  sites_k <- startends %>% select(Site, Deploy_date_lub, Days) %>% 
    filter(!is.na(Days)) %>%
    uncount(Days)
  sites_k$Site <- ifelse(sites_k$Site < 10, paste0("0", sites_k$Site), sites_k$Site)
  sites_k$Site <- paste0("Site_",sites_k$Site)
  sites_k$Site_f <- as.factor(sites_k$Site)
  k <- rep(NA, nrow(sites_k))
  k[1] <- 1
  sites_k <- cbind(sites_k,k)
  for(i in 2:nrow(sites_k)){
    if(sites_k$Site[i]==sites_k$Site[i-1])
      sites_k$k[i] <-  sites_k$k[i-1] + 1L 
    else
      sites_k$k[i] <- 1L
  }
  sites_k$k_date <- sites_k$Deploy_date_lub + (sites_k$k - 1)
  
  # Merge sites_k with consensus classifications so we know which visit (day) k each detection was on
  templist <- list()
  for(i in 1:length(unique(sites_k$Site))){
    isite <- unique(sites_k$Site)[i]
    temp_df <- df %>% filter(site==isite)
    temp_sites_k <- sites_k %>% filter(Site==isite)
    temp_df$DateLub <- as_date(temp_df$DateTimeLub)
    temp_df2 <- merge(temp_df, temp_sites_k, by.x = "DateLub", by.y = "k_date")
    temp_df2 <- temp_df2 %>% select(-Site_f, -Site)
    templist[[i]] <- temp_df2
  }
  df <- do.call(rbind, templist)
  
  # Group by site and k
  df <- df %>% group_by(site, k) %>% summarise(n = sum(indicator), .groups = "keep")
  
  # Make from long to wide (i.e. make columns days)
  df$k <- ifelse(df$k < 10, paste0("0", df$k), df$k)
  df$k <- as.factor(paste0("V",df$k))
  df <- df %>% rename(visit = k)
  df <- df %>% pivot_wider(names_from = visit, values_from = n) 
  df <- df %>% select(order(colnames(df)))
  
  # Add rows for the days which were surveyed but had zero detections
  sites_names <- sites_k %>% select(Site_f) %>% unique()
  df <- merge(sites_names, df, by.x = "Site_f", by.y = "site", all.x = TRUE)
  df <- df %>% rename(site = Site_f)
  
  # Optionally (default behaviour) turn the site ID's into integers
  if(sites_as_integers == TRUE){
    df$site <- gsub("Site_","",df$site)
    df$site <- as.integer(df$site)
    df <- df[order(df$site),]
  }
  
  # Add columns up to max value of k (columns missing when sp not seen on last visits) 
  # Also adds columns for visits in which no sites saw the species
  col_list <- list()
  max_k <- sitedays %>% group_by(Site) %>% summarise(tot_k = sum(Days))
  
  for(i in 1:max(max_k$tot_k)){
    temp <- rep(NA, nrow(df))
    temp <- as.data.frame(temp)
    colnames(temp) <- ifelse(i < 10, paste0("V","0",i), paste0("V",i))
    if(!colnames(temp) %in% colnames(df)){
      col_list[[i]] <- temp
    }
  }
  col_list<-col_list[!sapply(col_list,is.null)]
  col_df <- do.call(cbind, col_list)
  df <- cbind(df, col_df)
  df <- df %>% select(order(colnames(df))) # Re-order columns
  
  # Make each visit value observed if any observed, 0 if visited but unobserved, NA if not visited
  df[is.na(df)] <- 0 # First make every NA zero
  
  # Then fill from right with NA for k_max - k[i] cells
  for(i in 1:nrow(df)){
    for(k in 1:max(max_k$tot_k) + 1L){
      if(k <= max_k$tot_k[i] + 1L)
        df[i, k] <- df[i, k]
      else
        df[i, k] <- NA
    }
  }
  
  # Optionally return a binary detection matrix (1 = detected, 0 = not detected, NA = not surveyed)
  if(binary == TRUE){
    df[,-1] <- ifelse(df[,-1] > 0, 1, 0)
  }
  
  return(df)
  
}

# Generates detection matrix where the rows are site-days and the columns are hours ####
generate_detection_matrix_hours <- function(sp, binary = FALSE){
  
  # Check if consensus classifications dataframe exists, stop and warn if not
  if(!exists("consensus_classifications")){
    stop("Please load the consensus_classifications dataframe")
  }
  
  # Check if sitedays dataframe exists, stop and warn if not
  if(!exists("sitedays")){
    stop("Please load the sitedays dataframe")
  }
  
  # Check if hours dataframe exists, create if not
  if(!exists("hrs")){
    warning("hours dataframe does not exist, creating temporary version")
    hrs <- as.data.frame(0:23) 
    colnames(hrs) <- "hr"
  }
  
  # Subset to the correct species
  df <- consensus_classifications %>% filter(species==sp)
  
  # Warn and return NA if species was never observed 
  if(nrow(df)==0){
    warning(paste(sp, "was never observed"))
    return(NA)
  }
  
  # Add indicator for use later
  df$indicator <- 1L
  
  # Group by day
  df <- df %>% group_by(site, day = day(DateTimeLub), hour = hour(DateTimeLub)) %>% summarise(n = sum(indicator), .groups = "keep")
  df <- merge(df, hours, by.x="hour", by.y="hr", all = TRUE)
  # Make from long to wide (i.e. make columns hours)
  df <- unite(df, site_day, c(site, day), remove = FALSE)
  df$hour <- ifelse(df$hour < 10, paste0("0", df$hour), df$hour)
  df$hour <- as.factor(paste0("V",df$hour))
  df <- df %>% rename(visit = hour)
  df <- df %>% pivot_wider(names_from = visit, values_from = n) 
  df <- df %>% select(order(colnames(df)))
  
  # Add rows for the days which were surveyed but had zero detections
  ff <- sitedays %>% uncount(Days)
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
  df <- merge(ff, df, by="site_day", all.x = TRUE)
  df <- df %>% select(-site, -day, -gg)
  
  # Turn NA values into 0
  df[is.na(df)] <- 0
  
  if(binary == TRUE){
    df[,-(1:2)] <- ifelse(df[,-(1:2)] > 0, 1, 0)
  }
  
  return(df)
}

# Generates distance matrix from dataframe with columns that contain "lat" or "long" ####
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


# Opposite of %in% ####
'%notin%' <- function(x,y)!('%in%'(x,y))

# Function to take covariates defined on day/sub-day level (e.g., weather data) and arrange them into same format as detection matrix. ####
bind_daily_covs <- function(startends, day_data, day_cov, summary_type = "mean", standardise = FALSE, standardize = FALSE, date_format = "ymd", sites_as_integers = TRUE){
  
  # Select date format (default is year-month-day)
  if(date_format == "dmy"){
    warning("Using day-month-year date format. Set date_format argument to change.")
    day_data$DateLub <- dmy(weather$Date)
  }else if(date_format == "mdy"){
    warning("Using month-day-year date format. Set date_format argument to change.")
    day_data$DateLub <- mdy(weather$Date)
  }else if(date_format == "ymd"){
    warning("Using year-month-day date format. Set date_format argument to change.")
    day_data$DateLub <- ymd(weather$Date)
  }
  
  # Select the day_data to use
  cov_daily <- day_data %>% select(DateLub, day_cov)
  
  # Group and summarise based on the selected option  
  if(summary_type == "mean"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = mean(!!sym(day_cov)))
  }else if(summary_type == "median"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = median(!!sym(day_cov)))
  }else if(summary_type == "total"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = sum(!!sym(day_cov)))
  }else if(summary_type == "min"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = min(!!sym(day_cov)))
  }else if(summary_type == "max"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = max(!!sym(day_cov)))
  }
  
  # Create dataframe with date for each visit (k)
  sites_k <- startends %>% select(Site, Deploy_date_lub, Days) %>% 
    filter(!is.na(Days)) %>%
    uncount(Days)
  sites_k$Site <- ifelse(sites_k$Site < 10, paste0("0", sites_k$Site), sites_k$Site)
  sites_k$Site <- paste0("Site_",sites_k$Site)
  sites_k$Site_f <- as.factor(sites_k$Site)
  k <- rep(NA, nrow(sites_k))
  k[1] <- 1
  sites_k <- cbind(sites_k,k)
  for(i in 2:nrow(sites_k)){
    if(sites_k$Site[i]==sites_k$Site[i-1])
      sites_k$k[i] <-  sites_k$k[i-1] + 1L 
    else
      sites_k$k[i] <- 1L
  }
  sites_k$k_date <- sites_k$Deploy_date_lub + (sites_k$k - 1)
  
  # Merge by date
  k_covs <- merge(sites_k, cov_daily, by.x = "k_date", by.y = "DateLub", all.x = TRUE)
  k_covs <- k_covs %>% select(Site, k, var)
  
  df <- k_covs
  
  # Optionally standardise the covariate (subtract mean and divide by sd)
  if(standardise == TRUE | standardize == TRUE){
    df$var <- standardize(df$var)
  }
  
  # Optionally (default behaviour) turn the site ID's into integers
  if(sites_as_integers == TRUE){
    df$Site <- gsub("Site_","",df$Site)
    df$Site <- as.integer(df$Site)
    df <- df[order(df$Site),]
  }
  
  # Make from long to wide (i.e. make columns days)
  df$k <- ifelse(df$k < 10, paste0("0", df$k), df$k)
  df$k <- as.factor(paste0("V",df$k))
  df <- df %>% rename(visit = k)
  df <- df %>% pivot_wider(names_from = visit, values_from = var) 
  df <- df %>% select(order(colnames(df)))
  
  # Return df
  return(df)
}
