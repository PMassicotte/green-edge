# http://www.remss.com/measurements/ccmp
# https://answers.yahoo.com/question/index?qid=20130910172911AAKdpmt
# http://stackoverflow.com/questions/23922776/quiver-arrow-plot-in-r
# http://stackoverflow.com/questions/19050481/how-to-map-wind-direction-and-speed-velocity-plot-with-r
# http://stackoverflow.com/questions/21484558/how-to-calculate-wind-direction-from-u-and-v-wind-components-in-r

rm(list = ls())

ws <- ncdf4::nc_open("data/windspeed/CCMP_Wind_Analysis_20160518_V02.0_L3.0_RSS.nc")

uwnd <- ncvar_get(ws, "uwnd")
vwnd <- ncvar_get(ws, "vwnd")

long <- ncvar_get(ws, "longitude")
lat <- ncvar_get(ws, "latitude")

longlat <- expand.grid(long, lat)

df <- data_frame(
  x = longlat$Var1,
  y = longlat$Var2,
  u = as.vector(uwnd[, , 1]),
  v = as.vector(vwnd[, , 1])
) %>%
  mutate(speed = sqrt(u ^ 2 + v ^ 2)) %>% 
  # mutate(direction = atan2(u / speed, v / speed )) %>% 
  # mutate(direction = direction * 180 / pi) %>% 
  mutate(x = ifelse(x > 180, x - 360, x)) %>%
  filter(x >= -75 & x <= -40) %>% 
  filter(y >= 40 & y <= 80) %>% 
  sample_frac(0.5)

scale = 0.5

p <- df %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(
    data = rworldmap::getMap(resolution = "low"),
    aes(x = long, y = lat, group = group),
    inherit.aes = F,
    fill = "gray75"
  ) +
  # geom_spoke(aes(
  #   angle = direction,
  #   radius = 1,
  #   color = speed
  # ),
  # arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(
    xend = x + u * scale,
    yend = y + v * scale,
    color = speed
  ),
  arrow = arrow(length = unit(0.1, "cm")),
  lwd = 0.1,
  alpha = 1) +
  scale_color_viridis() +
  coord_equal(xlim = c(-75, -40), ylim = c(40, 80)) +
  xlab("Longitude") +
  ylab("Latitude")

print(p)
ggsave("graphs/windspeed.png")
