file <- "~/Downloads/CCMP_Wind_Analysis_20050828_V02.0_L3.0_RSS.nc"

ws <- ncdf4::nc_open(file)

r <- raster::raster(file, var = "uwnd", band = 2)
plot(r, col = viridis::viridis(n = 256))

baffin <- rgdal::readOGR("/media/work/projects/green-edge/data/shapefiles/baffin/", "baffin")
ex <- raster::extent(baffin)

ws <- ncdf4::nc_open(file)

uwnd <- ncvar_get(ws, "uwnd")
vwnd <- ncvar_get(ws, "vwnd")

long <- ncvar_get(ws, "longitude")
lat <- ncvar_get(ws, "latitude")

longlat <- expand.grid(long, lat)

df <- data_frame(
  lon = longlat$Var1,
  lat = longlat$Var2,
  u = as.vector(uwnd[, , 1]),
  v = as.vector(vwnd[, , 1])
) %>%
  mutate(speed = sqrt(u ^ 2 + v ^ 2)) %>% 
  mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
  dplyr::select(lon, lat, speed) %>% 
  # filter(lon >= ex@xmin & lon <= ex@xmax) %>%
  # filter(lat >= ex@ymin & lat <= ex@ymax) %>%
  SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# r <- raster::rasterFromXYZ(df, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# raster::projection(r) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# plot(r)

df %>% 
  as.data.frame() %>% 
  # filter(lat > 45) %>% 
  ggplot(aes(x = lon, y = lat, fill = speed)) +
  geom_raster(size = 0.1) +
  geom_path(data = baffin, aes(x = long, y = lat, group = group), inherit.aes = FALSE, color = "gray") +
  viridis::scale_fill_viridis() 


rr <- df %>% 
  as.data.frame()

rr <-   rasterFromXYZ(rr[, c("lon", "lat", "speed")], crs = CRS(proj))

# define jet colormap
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

plot(rr, col = jet.colors(n = 256), asp = 1)
