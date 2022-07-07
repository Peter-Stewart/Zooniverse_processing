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
