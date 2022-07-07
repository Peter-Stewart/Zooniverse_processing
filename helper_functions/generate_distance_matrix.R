# Generates distance matrix from dataframe with columns that contain "lat" or "long"

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
