# Function to take covariates defined on day/sub-day level (e.g., weather data) and arrange them into same format as detection matrix.
bind_daily_covs <- function(startends, day_data, day_cov, summary_type = "mean", date_format = "ymd", sites_as_integers = TRUE){
  
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
  cov_daily <- weather %>% select(DateLub, day_cov)
    
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
