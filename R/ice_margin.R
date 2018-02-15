rm(list = ls())

area <- readOGR("data/shapefiles/", "area_of_interest")

kml <- readOGR("/home/pmassicotte/Downloads/arctic_line_2016011.kmz") %>% 
  spTransform(CRSobj = CRS(proj4string(area)))

df <- SpatialLinesDataFrame(kml, data = data.frame(name = kml$Name)) %>% 
  fortify()

r <- rle(df$id)

df <- df %>% 
  mutate(type = rep(kml@data$Name, times = r$lengths))

# kml <- rgeos::gIntersection(df, area)

plot(kml, col = "red")
plot(area, add = T)


df %>%
  fortify() %>%
  # filter(type == "CT18") %>%
  ggplot(aes(
    x = long,
    y = lat,
    group = group,
    color = type
  )) +
  geom_polygon(
    data = rworldmap::getMap(resolution = "li") %>% spTransform(proj4string(kml)),
    aes(x = long, y = lat, group = group),
    inherit.aes = FALSE,
    fill = "gray"
  ) +
  geom_path() +
  coord_map(xlim = c(-70, -50), ylim = c(65, 72))


