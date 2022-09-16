# Function to make a nice clean plot of activity kernel
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
