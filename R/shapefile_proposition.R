# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Script to create the shapefile representing the area of
#               interest in the Baffin bay.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

baffin <- readOGR("data/shapefiles/baffin/", "baffin")
st <- readOGR("data/doc.kml")

p1 <- data_frame(
  long = c(-65, -50, -50, -65),
  lat = c(65, 65, 72, 72)
) %>% Polygon() %>% 
  list() %>% 
  Polygons(1) %>% 
  list() %>% 
  SpatialPolygons(proj4string = CRS(proj4string(baffin))) %>% 
  SpatialPolygonsDataFrame(data = data.frame(fake = 1))

writeOGR(p1,
         "data/shapefiles/",
         "area_of_interest",
         "ESRI Shapefile",
         overwrite_layer = TRUE)

# Plot --------------------------------------------------------------------

ggplot() +
  geom_polygon(data = baffin,
               aes(x = long, y = lat, group = group),
               fill = "gray") +
  geom_path(data = st, aes(x = long, y = lat), col = "red") +
  geom_polygon(
    data = p1,
    aes(x = long, y = lat),
    color = "blue",
    fill = NA,
    lwd = 0.5
  ) +
  scale_x_continuous(breaks = seq(-200, 0, by = 5), expand = c(0.1, 1)) +
  scale_y_continuous(breaks = seq(40, 90, by = 2)) +
  xlab("Longitude") +
  ylab("Latitude") 
  # +
  # coord_map(projection = "stereo")

ggsave("graphs/shapefile_proposition.png")

