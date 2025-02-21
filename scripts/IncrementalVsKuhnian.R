# Load necessary library
library(viridis)

# Generate data for Kuhnian knowledge (non-cumulative)
set.seed(250221)

# Generate data for Newtonian (incremental knowledge)
time <- 80  # Time points
knowledge <- rexp(time,2)
newtonian_df <- matrix(rep(knowledge, times = time), nrow = time, ncol = time )
newtonian_df[lower.tri(newtonian_df)] <- 0

png("figures/newton.png", 
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 10)
barplot(newtonian_df, 
        col = viridis(time), 
        axes = FALSE, 
        border = NA, 
        xlab = "Time", 
        ylab = "Knowledge", 
        main = "We stand on the shoulder of giants")
dev.off()

#simulate communities
sci_com_n <- 5 #number of scientific communities
max_com_size <- 20
min_com_size <- round(max_com_size/2) 
# com_time <- runif(sci_com_n,0,time) # mean historical time of sci_com
# com_time <- com_time[order(com_time)] # ordered
increment_com_time <- rexp(sci_com_n,0.05)
com_time <- cumsum(increment_com_time) 
scientists_n <- sample(min_com_size:max_com_size, size =  sci_com_n, replace = TRUE) # number of scientist per com
increment_com_knowledge <- rexp(sci_com_n,2)
com_knowledge <- cumsum(increment_com_knowledge)
com <- data.frame(1:sci_com_n, scientists_n)
scientist <- matrix( data = NA, nrow = sum(scientists_n), ncol = 3)

sc <-list()
for(i in 1:sci_com_n) sc[[i]] <- rep(com[i,1], times = com[i,2])

s_comm <- unlist(sc)
scientist <- data.frame(s_com = s_comm, s_time = NA, s_knowledge = NA)


for(i in 1:length(s_comm)){
  scientist[i,2] <- rnorm(1,com_time[scientist[i,1]],6)
  scientist[i,3] <- rnorm(1,com_knowledge[scientist[i,1]],0.1)
}

# Plot Kuhnian knowledge evolution

png("figures/Kuhn.png", 
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 10)

with(scientist,{
  plot(NULL, bty = "n",
       xlim = c(min(s_time), max(s_time)), 
       ylim = c(min(s_knowledge), max(s_knowledge)),
       xaxt = "n",
       yaxt = "n",
       xlab = "Time", 
       ylab = "Knowledge", 
       main = "Kuhnian Knowledge Evolution")
  
})


with(scientist,{
for(i in 1:sci_com_n){ 
  x0 <- s_time[s_com == i]
  y0 <- s_knowledge[s_com == i]
  x1 <- s_time[s_com == i]
  y1 <- s_knowledge[s_com == i]
  for(j in 1:length(x0)){
    for(k in 1:length(y0)){
      segments(x0[j], y0[j] , x1[k] , y1[k], 
               col = i+1, lwd = 0.2)
    } }}})


lines(com_time, com_knowledge, lwd = 1)

with(scientist,{
  points(s_time, s_knowledge, pch = 20, cex = 0.1)
})
dev.off()

