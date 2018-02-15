proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

grat <- readOGR("data/shapefiles/ne_10m_graticules_all", "ne_10m_graticules_10") %>% 
  spTransform(CRS = proj) %>% 
  fortify()

track <- readOGR("data/doc.kml", "Tracks") %>% 
  spTransform(CRS = proj) %>% 
  fortify()

baffin <- readOGR("data/shapefiles/baffin/", "baffin") %>%
  spTransform(proj) %>%
  fortify()

load("data/ice_cover/shapefiles.rda")
  
i <- which(ice_cover$date >= "2016-05-01" & ice_cover$date <= "2016-07-27")
ice_cover$shapefile <- ice_cover$shapefile[i]
ice_cover$date <- ice_cover$date[i]

# plot --------------------------------------------------------------------

plot_ice <- function(ice, date) {
  
  ice <- ice %>% 
    spTransform(proj) %>% 
    fortify()
  
  station <- data_frame(longitude = -63.78953333333333,
                        latitude = 67.47973333333333) %>%
    SpatialPointsDataFrame(
      cbind(.$longitude, .$latitude),
      data = .,
      proj4string = CRS(
        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
      )
    ) %>%
    spTransform(proj) %>%
    as.data.frame()
  
  p <- baffin %>%
    ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "gray") +
    geom_polygon(data = ice, fill = "blue") +
    coord_fixed(ylim  = c(-3.6e06, -1e06),
                xlim = c(-2600000, -100000)) +
    geom_path(data = track, aes(x = long, y = lat), color = "red") +
    ggtitle(date) +
    geom_point(data = station, aes(x = coords.x1, y = coords.x2), color = "red", inherit.aes = FALSE) +
    geom_label(data = station, aes(x = coords.x1, y = coords.x2, label = "Qik"), inherit.aes = FALSE, hjust = 1.2)
  
  fn <- paste("/home/pmassicotte/Desktop/animation/", format(date, "%j"), ".png")
  
  ggsave(fn)
}

p <- map2(ice_cover$shapefile, ice_cover$date, plot_ice)
