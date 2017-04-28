# http://www.remss.com/measurements/ccmp
# https://answers.yahoo.com/question/index?qid=20130910172911AAKdpmt
# http://stackoverflow.com/questions/23922776/quiver-arrow-plot-in-r
# http://stackoverflow.com/questions/19050481/how-to-map-wind-direction-and-speed-velocity-plot-with-r
# http://stackoverflow.com/questions/21484558/how-to-calculate-wind-direction-from-u-and-v-wind-components-in-r

rm(list = ls())

library(raster)


extract_wind_speed <- function(file, proj, amundsen) {
  

  uwnd <- raster::brick(file, var = "uwnd") %>% 
    rasterToPoints() %>% 
    as_data_frame() %>% 
    rename(lon = x, lat = y) %>% 
    mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
    filter(lat >= 45) %>% 
    filter(lon >= -90 & lon <= -40) %>% 
    rasterFromXYZ(crs = CRS(proj)) 
  
  vwnd <- raster::brick(file, var = "vwnd") %>% 
    rasterToPoints() %>% 
    as_data_frame() %>% 
    rename(lon = x, lat = y) %>% 
    mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
    filter(lat >= 45) %>% 
    filter(lon >= -90 & lon <= -40) %>% 
    rasterFromXYZ(crs = CRS(proj)) 
  
  uwnd <- extract(uwnd, amundsen_sp) %>% 
    as_data_frame() %>% 
    bind_cols(amundsen) %>% 
    gather(wind_date, uwnd, starts_with("X")) %>% 
    mutate(wind_date = str_sub(wind_date, 2, -1)) %>% 
    mutate(wind_date = anytime::anytime(wind_date))
  
  vwnd <- extract(vwnd, amundsen_sp) %>% 
    as_data_frame() %>% 
    bind_cols(amundsen) %>% 
    gather(wind_date, vwnd, starts_with("X")) %>% 
    mutate(wind_date = str_sub(wind_date, 2, -1)) %>% 
    mutate(wind_date = anytime::anytime(wind_date))
  
  df <-
    inner_join(
      uwnd,
      vwnd,
      by = c(
        "cruise",
        "code_operation",
        "date_utc",
        "lat",
        "lon",
        "station",
        "station_type",
        "wind_date"
      )) %>% 
    mutate(speed = sqrt(uwnd ^ 2 + vwnd ^ 2))
  
  return(df)
  
}

download_wind <- function(date) {
  
  file <- sprintf(
    "ftp://ftp2.remss.com/ccmp/v02.0/Y2016/M05/CCMP_Wind_Analysis_%s_V02.0_L3.0_RSS.nc",
    format(date, "%Y%m%d")
  )
  
  tf <- paste0(tempdir(), "/", basename(file))
  
  file <- curl::curl_download(file, tf)
  
  return(file)
  
}

proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

curl::curl_download("http://www.obs-vlfr.fr/proof/php/GREENEDGE/docs/GE_Amundsen_Station_Coordinates.xlsx",
                    "data/GE_Amundsen_Station_Coordinates.xlsx")

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
  mutate(lon = -lon)

amundsen_sp <- amundsen %>%  
  SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS(proj)) 

# ****************************************************************************
# download all the data
# ****************************************************************************

date <- seq(as.Date("2016-05-01"), as.Date("2016-05-30"), by = 1)

file <- lapply(date, download_wind)

# ****************************************************************************
# Extract the data
# ****************************************************************************

res <- lapply(file, extract_wind_speed, proj = proj, amundsen = amundsen) %>% 
  bind_rows()

write_csv(res, "data/clean/wind_speed.csv")


# maps --------------------------------------------------------------------

plot_wind_speed <- function(file) {
  
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
    mutate(lon = ifelse(lon > 180, lon - 360, lon)) 
  
  date <- paste(as.Date(str_extract(file, "\\d{8}"), format = "%Y%m%d"), "00:00:00")
  
  map <- ggplot2::map_data(map = "world")
  
  p <- df %>%
    as.data.frame() %>%
    ggplot(aes(x = lon, y = lat, fill = speed)) +
    geom_raster() +
    geom_path(data = map, aes(x = long, y = lat, group = group), inherit.aes = FALSE, color = "gray75", size = 0.1) +
    viridis::scale_fill_viridis() +
    coord_fixed(expand = FALSE, xlim = c(-80, -25), ylim = c(45, 78), ratio = 1) +
    labs(title = date) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(fill = bquote(atop(Wind~speed, "("*m%*%s^{-1}*")")))
  
  fn <- paste0("graphs/wind_speed/", as.Date(date), ".pdf")
  ggsave(fn, p)
  
}

lapply(file, plot_wind_speed)



