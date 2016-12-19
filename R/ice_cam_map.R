rm(list = ls())

source("R/ice_cam_functions.R")

files <-
  list.files(
    "/media/data4tb/Camera_360/",
    pattern = "header.txt",
    full.names = TRUE,
    recursive = TRUE
  )

proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

dat <- lapply(files, read_parameter)
dat <- bind_rows(dat)

dat2 <- SpatialPointsDataFrame(
  cbind(dat$longitude, dat$latitude), 
  data = dat,
  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

dat2 <- spTransform(dat2, proj) %>% 
  as.data.frame() 

map <- rworldmap::getMap(resolution = "high") %>% 
  spTransform(proj) %>% 
  fortify()

dat2 %>%
  ggplot(aes(x = coords.x1, y = coords.x2)) +
  geom_point(color = "red", size = 0.1) +
  geom_polygon(
    data = map,
    aes(x = long, y = lat, group = group),
    inherit.aes = FALSE,
    fill = "gray75",
    color = "gray25"
  ) +
  coord_fixed(ylim  = c(-4.5e06, -1e06),
              xlim = c(-2600000, -100000)) +
  xlab("Longitude") +
  ylab("Latitude")

ggsave("graphs/ice_cam_positions.png")


# histogram ---------------------------------------------------------------

dat %>% 
  mutate(date = as.Date(start_time)) %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%B-%Y") +
  xlab("Acquisition date") +
  ylab("Number of images")

ggsave("graphs/ice_cam_histo.png")
