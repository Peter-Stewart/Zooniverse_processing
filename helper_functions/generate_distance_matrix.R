# Generates distance matrix from dataframe with columns that contain "lat" or "long"

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
