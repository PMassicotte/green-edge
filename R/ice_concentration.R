
# ftp://ftp-projects.zmaw.de/seaice/AMSR2/
# ftp://ftp-projects.cen.uni-hamburg.de/seaice/AMSR2/3.125km/

rm(list = ls())

extract_sic <- function(file, lon, lat, amundsen, proj) {
  
  date <- as.Date(stringr::str_match(file, "\\d{8}"), format = "%Y%m%d")
  
  # file <- "data/ice_concentration/Arc_20150301_res3.125_pyres.nc"
  nc <- ncdf4::nc_open(file)
  
  sic <- ncdf4::ncvar_get(nc, "sea_ice_concentration") %>% 
    as.vector()
  
  df <- data_frame(
    lon = lon,
    lat = lat,
    sic = sic *  0.00999999977648258
  ) %>% 
    filter(lon >= -65 & lon <= -50) %>%
    filter(lat >= 65 & lat <= 72) %>% 
    mutate(date = date) %>% 
    as.data.frame() %>% 
    SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS(proj4string(area))) %>% 
    spTransform(proj)
  
  ## Find the closest pixel for each station
  snap <- apply(spDists(amundsen, df), 1, function(x) {
    data_frame(distance_to_pixel = min(x),
               which_min = which.min(x))
  }) %>%
    bind_rows()

  df <- data.frame(df) %>% 
    dplyr::select(
      lon_ice = lon, 
      lat_ice = lat,
      sic,
      ice_date = date)
    
  res <- data.frame(amundsen) %>% 
    bind_cols(df[snap$which_min, ]) %>% 
    bind_cols(snap)
  
  return(res)
}

## Have a look to spatial coverage
area <- readOGR("data/shapefiles/", "area_of_interest")
plot(rworldmap::getMap())
plot(area, add = TRUE, col = "red")

latlong <- ncdf4::nc_open("data/ice_concentration/LongitudeLatitudeGrid_3.125km_Arctic.nc")

lat <- ncdf4::ncvar_get(latlong, "latitude") %>%
  as.vector()

lon <- ncdf4::ncvar_get(latlong, "longitude") %>%
  as.vector()

proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

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
  SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS(proj4string(area))) %>% 
  spTransform(proj)

## Add two stations for Leo LAcour

# leo <- tibble(
#   date_utc = as.Date(c("2016-06-14", "2016-07-09")),
#   lat = c(68.585, 67.677),
#   lon = c(-59.924, -59.902),
#   station = c("leo1", "leo2"),
#   cruise = "GE_AN201601",
#   code_operation = "leo",
#   station_type = "leo"
# ) %>% 
#   SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS(proj4string(area))) %>% 
#   spTransform(proj)
# 
# amundsen <-  rbind(amundsen, leo)

files <- list.files("data/ice_concentration/amundsen/", "2016\\d{4}\\S+pyres.nc$", full.names = TRUE)

res <- pbmclapply(files, extract_sic, mc.cores = detectCores() - 1, lon = lon, lat = lat, amundsen = amundsen, proj = proj) %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  dplyr::select(-coords.x1, -coords.x2)

## 2 extra stations for Leo

# amundsen2 <- tibble(
#   lon = -59.902,
#   lat = 67.677
# ) %>% 
#   SpatialPointsDataFrame(cbind(.$lon, .$lat), data = ., proj4string = CRS(proj4string(area))) %>% 
#   spTransform(proj)
# 
# extract_sic("data/ice_concentration/amundsen/Arc_20160709_res3.125_pyres.nc", lon = lon, lat = lat, amundsen = amundsen2, proj = proj)

# *************************************************************************
# Format the data so it fits with what Flavienne asked.
# *************************************************************************

final <- res %>%
  mutate(lon = -lon) %>%
  dplyr::select(cruise,
    code_operation,
    date_utc,
    lat_deg_n = lat,
    long_deg_w = lon,
    station,
    station_type,
    ice_date,
    sic = sic
  )

write_csv(final, "data/clean/ice_concentration_history_amundsen.csv")


## TEST

# rm(list = ls())
# 
# df <- read_csv("data/clean/ice_concentration_history_amundsen.csv")
# 
# unique(df$station)
# 
# 
# df %>%
#   filter(station %in% c("Crossby", "G713", "DIC1")) %>%
#   ggplot(aes(x = ice_date, y = sic, color = station)) +
#   geom_point() +
#   geom_line() +
#   theme(legend.justification = c(0, 0), legend.position = c(0.01, 0.01)) +
#   scale_x_date(date_labels = "%d-%b-%Y") +
#   xlab("Date") +
#   ylab("Sea ice concentration (%)")
# 
# ggsave("graphs/sea_ice_concentration.pdf")
