bind_daily_covs <- function(startends, day_data, day_cov, summary_type = "mean", date_format = "ymd"){
  
  # Stop and warn if date column not found
  if("Date" %notin% colnames(day_data)){
    stop("Error: date column not found. Please make sure column is named 'Date'.")
  }
  
  # Stop and warn if date column is in wrong format
  if(!is.Date(day_data$Date){
    stop("Error: wrong date format. Please make sure column is in lubridate's Date format.")
  }
  
  # Select date format (default is year-month-day)
  if(date_format == "dmy"){
    warn("Using day-month-year date format. Set date_format argument to change.")
    day_data$DateLub <- dmy(weather$Date)
  }else if(date_format == "mdy"){
    warn("Using month-day-year date format. Set date_format argument to change.")
    day_data$DateLub <- mdy(weather$Date)
  }else if(date_format == "ymd"){
    warn("Using year-month-day date format. Set date_format argument to change.")
    day_data$DateLub <- ymd(weather$Date)
  }
  
  # Select the day_data to use
  cov_daily <- weather %>% select(DateLub, day_cov)
    
  # Group and summarise based on the selected option  
  if(summary_type == "mean"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = mean(day_cov))
  }else if(summary_type == "median"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = median(day_cov))
  }else if(summary_type == "total"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = sum(day_cov))
  }else if(summary_type == "min"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = min(day_cov))
  }else if(summary_type == "max"){
    cov_daily <- cov_daily %>% group_by(DateLub) %>% summarise(var = max(day_cov))
  }
  
  cov_daily <- cov_daily %>% rename(paste0(day_cov,"_",summary_type) = var)
  
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
  
  # Return k_covs
  return(k_covs)
}
