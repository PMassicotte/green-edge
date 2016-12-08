# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Process the ice-cam images,
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# http://neondataskills.org/R/Multi-Band-Rasters-In-R/

rm(list = ls())

source("R/ice_cam_functions.R")

parameter <- read_parameter("data/ice-cam/Camera360_20160611134049_header.txt")
pixel_distance <- read_pixel_distance("data/ice-cam/Camera360_20160611134049_calibration.txt")

# r <- brick("data/ice-cam/Camera360_20160611134049_panorama.jpg")
# plotRGB(r)

r <- raster("data/ice-cam/Camera360_20160611134049_panorama.jpg")

v <- getValues(r)

# v <- v[,1]*0.21 + v[,2]*0.72 + v[,3]*0.07

# values(r) <- v

v <- matrix(v, nrow = r@nrows, ncol = r@ncols, byrow = TRUE)
v <- pracma::flipdim(v, 1)
v <- pracma::flipdim(v, 2)

M <- rmatio::read.mat("data/ice-cam/Calibration_coefficient_camera360_20160821.mat")

xpos <- M$x_pos
ypos <- M$y_pos

## Used to only keep pixels that are at a maximum of 250 m
cal_distance_max <- 250

# i <- which(abs(pixel_distance$pixel_distance - cal_distance_max) < 20)
# v <- v[1:i[1], ]
# xpos <- -xpos[1:i[1], ]
# ypos <- -ypos[1:i[1], ]


i <- which(abs(pixel_distance$pixel_distance) <= cal_distance_max)
v <- v[i, ]
xpos <- -xpos[i, ]
ypos <- -ypos[i, ]

v2 <- ifelse(v < 175, 1, 0)

# plot3D::image2D(x = xpos, y = ypos, z = v)

png("simpleIm.png", res = 240, width = 1204, height = 1024)
plot3D::image2D(x = xpos, y = ypos, z = v2, axes = T, col  = gray((0:32)/32), 
                xlab = "distance (m)", ylab = "distance (m)")
axis(1)
axis(2)
dev.off()



df <- data_frame(
  v = as.vector(v)
) %>% 
  filter(v > 0)

df %>% 
  ggplot(aes(x = v)) +
  geom_histogram(binwidth = 1)


jpeg::writeJPEG(pracma::flipdim(v, 1) / 255, "test.jpg", quality = 1)
