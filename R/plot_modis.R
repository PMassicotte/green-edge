rm(list = ls())

modis <- read_feather("data/modis/modis.feather") %>% 
  group_by(date) %>% 
  nest()

plot_modis <- function(df, baffin, grat) {
  
  proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  grat <- grat %>% 
    spTransform(CRS = proj) %>% 
    fortify()
  
  # track <- readOGR("data/doc.kml", "Tracks") %>% 
  #   spTransform(CRS = proj) %>% 
  #   fortify()
  
  # *************************************************************************
  # Cloud optical thickness
  # ************************************************************************
  
  cot <- df %>%
    dplyr::select(long, lat, cot) %>%
    raster::rasterFromXYZ(crs = unique(df$proj4string)) %>%
    raster::crop(baffin, snap = "in") %>%
    raster::mask(baffin, inverse = TRUE) %>%
    raster::projectRaster(crs = proj)
  
  cot <- data.frame(long = coordinates(cot)[, 1],
                    lat = coordinates(cot)[, 2],
                    cot = getValues(cot)) %>%
    na.omit()
  
  p_cot <- baffin %>%
    spTransform(CRSobj = proj) %>%
    ggplot(aes(x = long, y = lat)) +
    geom_raster(data = cot, aes(fill = cot)) +
    geom_polygon(aes(group = group), fill = "gray") +
    # geom_path(data = track, aes(x = long, y = lat), color = "red") +
    scale_fill_viridis() +
    coord_fixed(ylim  = c(-4.5e06, -1e06),
                xlim = c(-2600000, -100000)) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "white")) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_path(
      data = grat,
      aes(x = long, y = lat, group = group),
      color = "gray50",
      lwd = 0.1
    ) +
    ggtitle("Cloud optical thickness")
  
  # *************************************************************************
  # Cloud fraction
  # ************************************************************************
  
  cf <- df %>% 
    dplyr::select(long, lat, cf) %>% 
    raster::rasterFromXYZ(crs = unique(df$proj4string)) %>% 
    raster::crop(baffin, snap = "in") %>% 
    raster::mask(baffin, inverse = TRUE) %>% 
    raster::projectRaster(crs = proj)
  
  cf <- data.frame(
    long = coordinates(cf)[, 1],
    lat = coordinates(cf)[, 2],
    cf = getValues(cf)
  ) %>% 
    na.omit()
  
  p_cf <- baffin %>% 
    spTransform(CRSobj = proj) %>% 
    ggplot(aes(x = long, y = lat)) +
    geom_raster(data = cf, aes(fill = cf)) +
    geom_polygon(aes(group = group),fill = "gray") +
    # geom_path(data = track, aes(x = long, y = lat), color = "red") +
    scale_fill_viridis() +
    coord_fixed(ylim  = c(-4.5e06, -1e06),
                xlim = c(-2600000, -100000)) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "white")) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_path(
      data = grat,
      aes(x = long, y = lat, group = group),
      color = "gray50",
      lwd = 0.1
    ) +
    ggtitle("Cloud fraction")
  
  # *************************************************************************
  # Combine plots
  # ************************************************************************
  
  p <- cowplot::plot_grid(p_cf, p_cot, align = "hv")
  
  invisible(p)
  
}

baffin <- readOGR("data/shapefiles/baffin/", "baffin") %>% 
  spTransform(CRSobj = unique(modis$data[[1]]$proj4string))

grat <- readOGR("data/shapefiles/ne_10m_graticules_all/", "ne_10m_graticules_10") %>% 
  spTransform(CRSobj = unique(modis$data[[1]]$proj4string))

p <- lapply(modis$data, plot_modis, baffin = baffin, grat = grat)

map2(p, modis$date, function(x, y) {ggsave(paste0("graphs/modis/", y, ".png"), x)})
