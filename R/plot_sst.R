# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Script to plot daily SST map (requested by Dany Dumont).
# 
# ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/2016/AVHRR/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/read_sst.R")

plot_sst <- function(df, baffin, grat) {
  
  proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  # *************************************************************************
  # Create a raster from the XYY points.
  # *************************************************************************
  
  r <- raster::rasterFromXYZ(
    df[, 1:3],
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  ) %>% 
    crop(baffin) %>% 
    projectRaster(crs = proj) %>% 
    rasterToPoints(spatial = TRUE) %>% 
    as.data.frame()

  baffin <- spTransform(baffin, CRS = proj)
  
  # *************************************************************************
  # Create the plot.
  # *************************************************************************
  
  p <- ggplot(data = baffin, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "gray") +
    geom_raster(
      data = r,
      aes(x = x, y = y, fill = sst),
      inherit.aes = FALSE,
      interpolate = FALSE
    ) +
    # geom_path(data = track, aes(x = long, y = lat), color = "red") +
    geom_path(
      data = grat,
      aes(x = long, y = lat, group = group),
      color = "gray50",
      lwd = 0.1
    ) +
    coord_fixed(ylim  = c(-4.5e06, -1e06),
                xlim = c(-2600000, -100000)) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "white")) +
    xlab("Longitude") +
    ylab("Latitude") +
    scale_fill_viridis(na.value = "gray", limits = c(-2, 15), oob = scales::squish) +
    ggtitle(unique(df$date)) 
  
  fn <- sprintf("graphs/sst/sst_%s.pdf", unique(df$date))
  ggsave(fn, p)
  
}

baffin <- readOGR("data/shapefiles/baffin/", "baffin")

proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

grat <- readOGR("data/shapefiles/ne_10m_graticules_all/", "ne_10m_graticules_10") %>% 
  spTransform(CRS = proj) %>% 
  fortify()

track <- readOGR("data/doc.kml", "Tracks") %>% 
  spTransform(CRS = proj) %>% 
  fortify()

files <- list.files("data/sst/", pattern = ".nc$", recursive = TRUE, full.names = TRUE)

res <- lapply(files, read_sst)
p <- lapply(res, plot_sst, baffin = baffin, grat = grat)


# Animation ---------------------------------------------------------------

files <- list.files("graphs/sst/", ".pdf", full.names = TRUE)

lapply(files, function(x){
  
  xx <- tools::file_path_sans_ext(x)
  
  cmd <- sprintf("convert -density 72 %s -quality 50 %s.png", x, xx)
  system(cmd)
})

system("convert graphs/sst/*.png -delay 3 -loop 0 /home/pmassicotte/Desktop/annimation.gif")

# Remove png files

files <- list.files("graphs/sst/", ".png", full.names = TRUE)
unlink(files)
