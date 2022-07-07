# Expands a dataframe so that sites are sitedays (i.e. each site is one day at one site, each visit is one hour)

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
