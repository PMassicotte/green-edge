# devtools::install_github("hafen/trelliscopejs")

library(trelliscopejs)

rm(list = ls())

baffin <- readOGR("data/shapefiles/baffin/", "baffin")

# ogrInfo("data/pro_ice_float/route5B.kml")

files <- list.files("data/pro_ice_float/", "*.kml", full.names = TRUE)

df <- lapply(files, readOGR) %>% 
  lapply(as.data.frame) %>% 
  setNames(files) %>% 
  bind_rows(.id = "float_id") %>% 
  as_tibble() %>% 
  mutate(date = stringr::str_extract(X4, "\\d{2}\\S+\\W\\S+")) %>% 
  mutate(date = lubridate::parse_date_time(date, "ymd HMS")) %>% 
  filter(coords.x2 > 60) %>% 
  arrange(date) %>% 
  mutate(xend = c(coords.x1[2:nrow(.)], NA)) %>% 
  mutate(yend = c(coords.x2[2:nrow(.)], NA))

df %>%
  ggplot(aes(x = coords.x1, y = coords.x2, color = float_id)) +
  geom_polygon(data = baffin, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  # geom_path() +
  geom_point() +
  coord_fixed(xlim = c(-65, -55), ylim = c(65, 72.5)) +
  facet_trelliscope( ~ float_id, scales = "free") +
  theme(legend.position = "none") 
# +
#   geom_segment(
#     data = df[seq(1, nrow(df), by = 25),],
#     aes(
#       x = coords.x1,
#       y = coords.x2,
#       color = float_id,
#       xend = xend,
#       yend = yend
#     ),
#     arrow = arrow(length = unit(0.25,"cm"))
#   )
