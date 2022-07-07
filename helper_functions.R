# Generates detection matrix where the rows are site-days and the columns are hours
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


# Generates distance matrix from columns that contain "lat" or "long"
generate_distance_matrix <- function(df, center = FALSE, rescale = FALSE, sites_as_days = FALSE, squared = FALSE){
  
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

  # Calculate distance matrix
  dmat <- dist(coords, diag=T, upper=T)
  dmat <- as.matrix(dmat)
  
  if(squared == TRUE){
    dmat2 <- dmat^2
    return(dmat2)
  }
  else{
    return(dmat)
  } 
}

# Expands a dataframe so that sites are sitedays
expand_to_sitedays <- function(df, remove_sitedays_column = FALSE){
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
  rownames(df) <- df$site_day
  
  # Optionally remove the sitedays column (default is FALSE)
  if(remove_sitedays_column == TRUE){
    df <- df %>% select(-site_day)
  }
 return(df)
}
