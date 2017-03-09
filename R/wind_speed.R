# http://www.remss.com/measurements/ccmp
# https://answers.yahoo.com/question/index?qid=20130910172911AAKdpmt
# http://stackoverflow.com/questions/23922776/quiver-arrow-plot-in-r
# http://stackoverflow.com/questions/19050481/how-to-map-wind-direction-and-speed-velocity-plot-with-r
# http://stackoverflow.com/questions/21484558/how-to-calculate-wind-direction-from-u-and-v-wind-components-in-r

rm(list = ls())

# *************************************************************************
# Projections
# *************************************************************************

proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
area <- readOGR("data/shapefiles/", "area_of_interest")

# *************************************************************************
# Amundsen geographical positions
# *************************************************************************
amundsen <- readxl::read_excel("data/GE_Amundsen_Station_Coordinates.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::select(cruise,
                code_operation,
                
                date_utc,
                lat = lat_deg_n,
                lon = long_deg_w,
                station,
                station_type) %>% 
  mutate(date_utc = as.Date(date_utc, format = "%d-%b-%Y")) %>% 
  mutate(lon = -lon) %>% 
  filter(lat >= 65) %>% 
  SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS(proj4string(area))) 

extract_ws <- function(file) {
  
  ex <- extent(amundsen)
  
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
    # mutate(direction = atan2(u / speed, v / speed )) %>% 
    # mutate(direction = direction * 180 / pi) %>% 
    mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
    filter(lon >= ex@xmin & lon <= ex@xmax) %>%
    filter(lat >= ex@ymin & lat <= ex@ymax) %>%
    SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS(proj4string(area))) %>% 
    spTransform(proj)
  
  snap <- apply(spDists(amundsen %>% spTransform(proj), df), 1, function(x) {
    data_frame(distance_to_pixel = min(x),
               which_min = which.min(x))
  }) %>%
    bind_rows()
  
  df <- data.frame(df) %>% 
    dplyr::select(
      lon_wind = lon, 
      lat_wind = lat,
      u,
      v,
      speed)
  
  res <- data.frame(amundsen) %>% 
    bind_cols(df[snap$which_min, ]) %>% 
    bind_cols(snap)
  
  return(res)
  
  
}

file <- "data/wind_speed/M03/CCMP_Wind_Analysis_20160301_V02.0_L3.0_RSS.nc"

df <- extract_ws(file)

df %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_point(aes(color = "station")) +
  ggrepel::geom_text_repel(aes(label = station)) +
  geom_point(aes(x = lon_wind, y = lat_wind, color = "wind observation"))

df %>% 
  ggplot(aes(x = distance_to_pixel / 1000)) +
  geom_histogram(binwidth = 0.25)
