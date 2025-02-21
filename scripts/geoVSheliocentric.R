# examples taken from https://github.com/rmcelreath/stat_rethinking_2023/blob/492bede99d5ed909eb1167534dd3fbe5d07b3c8e/scripts/03_ptolemaic_model.R
# library loading and settings

library(rethinking)
library(animation)
library(magick)
dir.create("figures/frames/")
setwd("figures/frames/")

# new functions
ani.save <- function (list, dpi = 150, location = "frame_", prefix = 1000) 
{
  require(animation)
  if (missing(list)) 
    list = animation:::.ani.env$.images
  lapply(1:length(list), function(x) {
    dev.hold()
    replayPlot(list[[x]])
    ani.pause()
    
    # Windows-compatible way to save PNG
    file_name <- paste0(location, prefix + x, ".png")
    png(file = file_name, width = 7, height = 7, units = "in", res = dpi)  # Adjust size as needed
    replayPlot(list[[x]])  # Replot for the PNG device
    dev.off()  # Close the PNG device
  })
  invisible(NULL)
}

my_circle <- function(x=0,y=0,r=1,angle=0,...) {
  a <- seq(angle, angle + 2 * pi, length = 360)
  lines( r*cos(a)+x , r*sin(a)+y , ... )
}
scatter <- rnorm(360, 0, 1)
my_sky <- function(x=0,y=0,r=1,angle=0,...) {
  a <- seq(angle, angle + 2 * pi, length = 360)
  points( r*cos(a)+x+scatter , r*sin(a)+y ,  ... )
}
t_vals <- runif(100,0,2)
scatter <- rnorm(100, 0, 1)
my_sky <- function(x_start=-6, y_start=-3, x_end=3, y_end=6, n_stars=100, ...) {
  # Generate random scatter along the diagonal
  star_x <- x_start + t_vals * (x_end - x_start) + scatter
  star_y <- y_start + t_vals * (y_end - y_start) + scatter

  points(star_x, star_y, pch=8, col="khaki", ...)  # Draw scattered stars
}

p2c <- function(a) c(cos(a) , sin(a))

# simulation of simple ptolemaic epicycles model to show retrograde motion
# two body version
# earth at center
# mars orbits an orbit around earth


# need updating for radial position of (1) mars epicycle center (2) mars itself

par(bg = 'black')
plot(NULL,xlim=c(-1,1),ylim=c(-1,1),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")

draw_ptolemy <- function(tx,history=FALSE,r1=1,r2=0.2) {
  
  if ( history==TRUE ) {
    xs <- rep(NA,tx)
    ys <- rep(NA,tx)
    for ( ttx in 1:tx ) {
      xy <- p2c( k1[ttx] )*r1
      xy2 <- xy + p2c( k2[ttx] )*r2
      xs[ttx] <- xy2[1]
      ys[ttx] <- xy2[2]
    }
    lines( xs , ys , lty=1 , lwd=2 , col=2 )
  }
  
  my_circle(0,0,r=r1,lty=2,lwd=0.5,col="white")
  
  xy <- p2c( k1[tx] )*r1
  my_circle( xy[1] , xy[2] , angle=k1[tx] , r=r2 , lty=2 , lwd=0.5 , col="white" )
  
  xy2 <- xy + p2c( k2[tx] )*r2
  points( xy2[1] , xy2[2] , pch=16 , col=2 , cex=2 ) # mars
  points( 0 , 0 , pch=16 , col=4 , cex=4 ) # earth
}

mpts <- 300
k1 <- seq( 0 , 2 * pi , length = mpts)
mu <- 2
k2 <- seq( 0 , mu * 2 * pi , length = mpts )

ani.record(reset = TRUE)  # clear history before recording
for ( i in 1:length(k1) ) {
  par(bg = 'black')
  par(xpd=NA)
  plot(c(-1, 1), c(-1, 1), type = "n", asp = 1, axes = FALSE, 
       xlab = "", ylab = "")
  draw_ptolemy( i , history=TRUE , r1=0.77 , r2=0.5 )
  ani.record()  # record the current frame
  par(bg = 'white')
}

oopts = ani.options(interval = 0.01)
ani.replay()



ani.save(dpi=150)


# Ensure images are in the working directory
files <- list.files(pattern = "frame.*\\.png")

# Check if any files were found
if (length(files) == 0) {
  stop("No PNG files matching the pattern 'frame.*\\.png' were found.")
}

# Read all frames
frames <- image_read(files)

# Check if frames were successfully loaded
if (length(frames) == 0) {
  stop("No frames were successfully loaded.")
}

# Combine frames into a GIF with a 4/100s delay (fps = 25)
gif <- image_animate(frames, fps = 25)

# Set white background and remove transparency
gif <- image_background(gif, "white")

# Save the GIF and the last .png
image_write(gif, "../geocentric.gif")
file.copy("frame_1300.png", "../geocentric.png")


############################################

# now kopernican with earth and mars


par(bg = 'black')
plot(NULL,xlim=c(-3,1),ylim=c(-1,1),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")


kop_drawtime <- function(tx,history=FALSE,r1=0.5,r2=1,r3=7) {
  
  my_circle(0,0,r=r1,lty=2,lwd=0.5,col="white")
  my_circle(0,0,r=r2,lty=2,lwd=0.5,col="white")
  
  # earth
  xy <- p2c( k1[tx] )*r1
  
  # mars
  xy2 <- p2c( k2[tx] )*r2
  
  # line between earth-mars
  lines( c(xy[1],xy2[1]) , c(xy[2],xy2[2]) , lwd=2 , col="white" )
  
  points( xy2[1] , xy2[2] , pch=16 , col=2 , cex=2 ) # mars
  points( xy[1] , xy[2] , pch=16 , col=4 , cex=4 ) # earth
  
  # sky
  my_circle(0,0,r=r3,lty=3,lwd=0.5,col="yellow")
  
  # now need line projecting out to sky "orbit" with same slope as line btw earth-mars
  # equation for the sky is x^2 + y^2 = r3^2
  # line has slope m = (xy2[2]-xy[2])/(xy2[1]-xy[1])
  # y1 - y2 = m*(x1 - x2)
  # let (X,Y) be our solution points, then:
  # Y - xy[2] = m*(X - xy[1]) , X^2 + Y^2 = r^2
  # will have quadratic form
  # X = -((m (-m x + y) + Sqrt[(1 + m^2) r^2 - (-m x + y)^2])/(1 + m^2))
  # Y = (y - m (x + Sqrt[(1 + m^2) r^2 - (-m x + y)^2]))/(1 + m^2)
  m <- (xy2[2]-xy[2])/(xy2[1]-xy[1])
  x <- xy2[1]
  y <- xy2[2]
  X <- -((m*(-m*x + y) + sqrt((1 + m^2)*r3^2 - (-m*x + y)^2))/(1 + m^2))
  Y <- (y - m*(x + sqrt((1 + m^2)*r3^2 - (-m*x + y)^2)))/(1 + m^2)
  if ( x < 0 ) lines( c(X,x) , c(Y,y) , lty=2 , col="white" )
  
  # ghost mars on sky
  if ( x < 0 ) points( X , Y , pch=1 , lwd=3 , col=2 , cex=2 ) # mars
  
}

mpts <- 300
k2 <- seq( pi/2 , 2 * pi + pi/2 , length = mpts)
mu <- 2
k1 <- seq( 0 , mu * 2 * pi , length = mpts )

ani.record(reset = TRUE)  # clear history before recording
for ( i in 1:length(k1) ) {
  par(bg = 'black')
  par(xpd=NA)
  plot(c(-7, 2), c(-2, 2), type = "n", asp = 1, axes = FALSE, 
       xlab = "", ylab = "")
  kop_drawtime( i , history=TRUE , r1=0.8 , r2=1.2 , r3=7.5 )
  ani.record()  # record the current frame
  par(bg = 'white')
}
# 
# par(bg = 'black')
# plot(NULL,xlim=c(-1,10),ylim=c(-1,1),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")

# 
# kop_drawtime2 <- function(tx, history=FALSE, r1=0.5, r2=1, r3=7) {
#   
#   my_circle(0, 0, r=r1, lty=2, lwd=0.5, col="white")  # Earth orbit
#   my_circle(0, 0, r=r2, lty=2, lwd=0.5, col="white")  # Mars orbit
#   
#   # Earth position
#   xy <- p2c(k1[tx]) * r1
#   
#   # Mars position
#   xy2 <- p2c(k2[tx]) * r2
#   
#   # Line between Earth and Mars
#   lines(c(xy[1], xy2[1]), c(xy[2], xy2[2]), lwd=2, col="white")
#   
#   points(xy2[1], xy2[2], pch=16, col=2, cex=2)  # Mars
#   points(xy[1], xy[2], pch=16, col=4, cex=4)    # Earth
#   
#   # the sky
#   
#   # Sky orbit
#   my_sky(0, 0, r=r3, pch= 8, col="khaki")
# 
#   
#   # Projection of Mars onto the sky
#   m <- (xy2[2] - xy[2]) / (xy2[1] - xy[1])
#   x <- xy2[1]
#   y <- xy2[2]
#   X <- -((m * (-m * x + y) + sqrt((1 + m^2) * r3^2 - (-m * x + y)^2)) / (1 + m^2))
#   Y <- (y - m * (x + sqrt((1 + m^2) * r3^2 - (-m * x + y)^2))) / (1 + m^2)
#   
#   if (x < 0) lines(c(X, x), c(Y, y), lty=2, col="white")
#   
#   # Ghost Mars on the sky orbit
#   if (x < 0) points(X, Y, pch=1, lwd=3, col=2, cex=2)  # Mars projection on sky
#   
#   # Track Mars' projection on the sky over time
#   if (history && tx > 1) {
#     xs <- numeric(tx)
#     ys <- numeric(tx)
#     for (i in 1:tx) {
#       x_i <- p2c(k2[i]) * r2
#       m_i <- (x_i[2] - p2c(k1[i])[2]) / (x_i[1] - p2c(k1[i])[1])
#       X_i <- -((m_i * (-m_i * x_i[1] + x_i[2]) + sqrt((1 + m_i^2) * r3^2 - (-m_i * x_i[1] + x_i[2])^2)) / (1 + m_i^2))
#       Y_i <- (x_i[2] - m_i * (x_i[1] + sqrt((1 + m_i^2) * r3^2 - (-m_i * x_i[1] + x_i[2])^2))) / (1 + m_i^2)
#       
#       # Collect only valid projections (Mars on the correct side)
#       if (x_i[1] < 0) {
#         xs[i] <- X_i
#         ys[i] <- Y_i
#       } else {
#         xs[i] <- NA
#         ys[i] <- NA
#       }
#     }
#     lines(xs, ys, lty=1, col=2, lwd=2)  # Trace of Mars' retrograde motion on sky
#   }
# }
# # 
# kop_drawtime2 <- function(tx, history=FALSE, r1=0.5, r2=1, r3=7) {
#   
#   my_circle(0, 0, r=r1, lty=2, lwd=0.5, col="white")  # Earth orbit
#   my_circle(0, 0, r=r2, lty=2, lwd=0.5, col="white")  # Mars orbit
#   
#   # Earth position
#   xy <- p2c(k1[tx]) * r1
#   
#   # Mars position
#   xy2 <- p2c(k2[tx]) * r2
#   
#   # Line between Earth and Mars
#   lines(c(xy[1], xy2[1]), c(xy[2], xy2[2]), lwd=2, col="white")
#   
#   points(xy2[1], xy2[2], pch=16, col=2, cex=2)  # Mars
#   points(xy[1], xy[2], pch=16, col=4, cex=4)    # Earth
#   
#   # **Draw the new diagonal sky with scattered stars**
#   my_sky(x_start=7, y_start=-3, x_end=2, y_end=6, n_stars=100)
#   
#   # **Projection of Mars onto the diagonal sky**
#   # Define sky line equation: y = mx + b
#   sky_m <- (6 - (-3)) / (2 - (7))  # Slope of diagonal sky
#   sky_b <- -3 - sky_m * (7)  # Intercept of sky line
#   
#   # Find Mars' projection onto the sky line
#   proj_x <- (xy2[1] + sky_m * (xy2[2] - sky_b)) / (1 + sky_m^2)
#   proj_y <- sky_m * proj_x + sky_b
#   
#   lines(c(xy2[1], proj_x), c(xy2[2], proj_y), lty=2, col="white")  # Projection line
#   points(proj_x, proj_y, pch=1, lwd=3, col=2, cex=2)  # Mars projection on sky
#   
#   # **Track Mars' motion on the diagonal sky**
#   if (history && tx > 1) {
#     xs <- numeric(tx)
#     ys <- numeric(tx)
#     for (i in 1:tx) {
#       xy_i <- p2c(k2[i]) * r2
#       proj_x_i <- (xy_i[1] + sky_m * (xy_i[2] - sky_b)) / (1 + sky_m^2)
#       proj_y_i <- sky_m * proj_x_i + sky_b
#       xs[i] <- proj_x_i
#       ys[i] <- proj_y_i
#     }
#     lines(xs, ys, lty=1, col=2, lwd=2)  # Mars' retrograde motion on sky
#   }
# }
# 

# 
# ani.record(reset = TRUE)  # clear history before recording
# for ( i in 1:length(k1) ) {
#   par(bg = 'black')
#   par(xpd=NA)
#   plot(c(-1, 5), c(-2, 2), type = "n", asp = 1, axes = FALSE, 
#        xlab = "", ylab = "")
#   kop_drawtime2( i , history=TRUE , r1=0.8 , r2=1.2 , r3=7.5 )
#   ani.record()  # record the current frame
#   par(bg = 'white')
# }
# 

oopts = ani.options(interval = 0.03)

ani.replay()


ani.save(dpi=150)

# Ensure images are in the working directory
files <- list.files(pattern = "frame.*\\.png")

# Check if any files were found
if (length(files) == 0) {
  stop("No PNG files matching the pattern 'frame.*\\.png' were found.")
}

# Read all frames
frames <- image_read(files)

# Check if frames were successfully loaded
if (length(frames) == 0) {
  stop("No frames were successfully loaded.")
}

# Combine frames into a GIF with a 4/100s delay (fps = 25)
gif <- image_animate(frames, fps = 25)

# Set white background and remove transparency
gif <- image_background(gif, "white")

# Save the GIF and png
image_write(gif, "../heliocentric.gif")
file.copy("frame_1300.png", "../heliocentric.png")


#reset WD
setwd("../..")
# delete the frames folder 
unlink("figures/frames", recursive = TRUE)
unlink("figures/frames", recursive = TRUE)

