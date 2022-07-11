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
  
  # Add rows for the days which were surveyed but had zero detections
  sites_names <- sites_k %>% select(Site_f) %>% unique()
  df <- merge(sites_names, df, by.x = "Site_f", by.y = "site", all.x = TRUE)
  
  # Add columns up to max value of k (columns missing when sp not seen on last visits) 
  # Also adds columns for visits in which no sites saw the species
  col_list <- list()
  for(i in 1:max(sites_k$k)){
    temp <- rep(NA, nrow(df))
    temp <- as.data.frame(temp)
    colnames(temp) <- ifelse(i < 10, paste0("V","0",i), paste0("V",i))
    if(!colnames(temp) %in% colnames(df)){
      col_list[[i]] <- temp
      }
    }
  col_list2 <- col_list %>% compac
  col_df <- do.call(rbind, col_list)
  
  # Make each visit value observed if any observed, 0 if visited but unobserved, NA if not visited
  df[is.na(df)] <- 0 # Make every NA zero
  
  
  # Fill from right with NA for k_max - k[i] cells
  
  
}
