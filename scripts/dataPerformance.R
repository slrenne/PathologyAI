# Define sigmoid function
sigmoid <- function(x, k, x0) {
  return(1 / (1 + exp(-k * (x - x0))))
}

# Set x range
x <- seq(0, 10, length.out = 100)

# Generate sigmoid curves with different parameters
y1 <- sigmoid(x, k = 1, x0 = 3)  # Symbolic AI
y2 <- sigmoid(x, k = 1.5, x0 = 4) * 1.5  # Machine Learning
y3 <- sigmoid(x, k = 2, x0 = 5) * 2.5 # Deep Learning



png("figures/dataPerf.png", 
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 5.5)
# canvas
plot(NULL, ylim = c(0, 2.5), xlim = c(0,10), bty = "L",
     xlab = "Data", ylab = "Performance", 
     xaxt = "n", yaxt = "n",
     main = "AI Performance Over Time")

# Add lines
lines(x, y1, col = 2, lwd = 3)
lines(x, y2, col = 3, lwd = 3)
lines(x, y3, col = 4, lwd = 3)

# Add labels
lbls <-  c("Symbolic AI", "Machine\nLearning", "Deep\nLearning")
text( x = 9, y = c(1,1.5,2.5)-0.01, labels = lbls, cex = 2, pos = 1)
dev.off()
