
# ftp://ftp-projects.zmaw.de/seaice/AMSR2/

rm(list = ls())

baffin <- readOGR("data/shapefiles/baffin/", "baffin")

# rr2 <- crop(rr, baffin)

# ftp://ftp-projects.zmaw.de/seaice/AMSR2/

# dat <- ncdf4::nc_open("/home/pmassicotte/Downloads/Arc_20140429_res3.125_pyres.nc")
# sic <- ncdf4::ncvar_get(dat, "sea_ice_concentration") %>% 
#   as.vector()
# 
# latlong <- ncdf4::nc_open("/home/pmassicotte/Downloads/LongitudeLatitudeGrid_3.125km_Arctic.nc")
# 
# lat <- ncdf4::ncvar_get(latlong, "latitude") %>% 
#   as.vector()
# 
# long <- ncdf4::ncvar_get(latlong, "longitude") %>% 
#   as.vector()

# longlat <- expand.grid(long, lat)

land_mask <- raster("/home/pmassicotte/Downloads/Arc_20140429_res3.125_pyres.nc", 
                    varname = "land")

sic <- raster("/home/pmassicotte/Downloads/Arc_20140429_res3.125_pyres.nc", 
              varname = "sea_ice_concentration") %>% 
  mask(land_mask, maskvalue = 0) %>% 
  rasterToPoints() %>% 
  data.frame()

long <- raster("/home/pmassicotte/Downloads/LongitudeLatitudeGrid_3.125km_Arctic.nc", 
                  varname = "longitude") %>% 
  mask(land_mask, maskvalue = 0) %>% 
  rasterToPoints() %>% 
  data.frame()

lat <- raster("/home/pmassicotte/Downloads/LongitudeLatitudeGrid_3.125km_Arctic.nc", 
               varname = "latitude") %>% 
  mask(land_mask, maskvalue = 0) %>% 
  rasterToPoints() %>% 
  data.frame()

df <- data_frame(
  long = long$longitude,
  lat = lat$latitude,
  sic = sic$daily.averaged.total.ice.concentration
) 


# r <- rasterFromXYZ(df, digits = 0.25)
# 
# df %>% 
#   ggplot(aes(x = long, y = lat, fill = sic)) +
#   geom_raster()

# crs(sic) <- proj4string(baffin)
# 
# extent(sic) <- extent(min(long$longitude), max(long$longitude), min(lat$latitude), max(lat$latitude))
# 
# proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# 
# sic2 <- sic %>% 
#   crop(baffin) %>% 
#   projectRaster(crs = proj)

# df <- raster::crop(df, baffin)


# df %>% 
#   ggplot(aes(x = long, y = lat, fill = sic)) +
#   geom_raster() +
#   scale_fill_viridis()

sic %>%
  ggplot(aes(x = x, y = y, fill = daily.averaged.total.ice.concentration)) +
  geom_raster() +
  scale_fill_viridis() +
  geom_polygon(data = baffin,
               aes(x = long, y = lat, group = group),
               inherit.aes = FALSE) +
  scale_x_continuous(labels = long$longitude)
