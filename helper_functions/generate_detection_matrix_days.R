generate_detection_matrix_hours <- function(sp, binary = FALSE){
  
  # Check if consensus classifications dataframe exists, stop and warn if not
  if(!exists("consensus_classifications")){
    stop("Please load the consensus_classifications dataframe")
  }
  
  # Check if sitedays dataframe exists, stop and warn if not
  if(!exists("startends")){
    stop("Please load the startends dataframe")
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
  
}
