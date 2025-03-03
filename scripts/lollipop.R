# libraries
library(lubridate)
minMAX <- function(x){
  m <- min(x) -5
  M <- max(x) +10
  mM <- c(m,M)
  return(mM)
  }

# import the dataframe
df <- read.csv("C:/Users/srenne/Dropbox/R/R_Projects/PathologyAI/input/timelineDNN.txt")

# Prepare the data as dates
#df$time <- ymd(df$time, truncated = 2L)


png("figures/lollipop.png", 
    width     = 6.5,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 5.5)

# Plot setup
plot(NULL, bty= "n",  ylim = c(0, 0.8), yaxt = "n",
     xlim = minMAX(df$time) ,
     xlab = "Year", ylab = "", 
     main = "Timeline of AI Milestones", axes = TRUE)

abline(h = 0, lwd=2)
# Draw lollipop sticks
segments(df$time, 0, df$time, 0.3, lwd = 2)

# Draw lollipop heads
points(df$time, rep(0.3, length(df$time)), pch = 19, col = 1:nrow(df), cex = 2)

# Add event labels
o <- rep(0, times = nrow(df)) #create offset to avoid overlap
o[5:7] <- c(-0.5,0,1)
text(
  df$time + o -1, 
  rep(0.32, length(df$time)), 
  labels = paste0(df$event," (",df$time,")"),
  pos = 4, cex = 1.2, srt = 45, adj = 1)

dev.off()