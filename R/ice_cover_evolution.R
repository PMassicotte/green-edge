rm(list = ls())


# Load data ---------------------------------------------------------------

load("data/ice_cover/shapefiles.rda")

area <- lapply(ice_cover$shapefile, rgeos::gArea) %>%
  unlist()

# Plot --------------------------------------------------------------------

df <- data_frame(
  date = ice_cover$date,
  area = area / 1000
) %>% # m2 -> km2
  filter(date != "2016-07-13") # problem with this observation

mission <- data_frame(
  xmin = as.Date(c("2016-06-03", "2016-05-01")),
  xmax = as.Date(c("2016-07-14", "2016-07-27")),
  ymin = c(-Inf, -Inf),
  ymax = c(Inf, Inf),
  label = c("Amundsen", "Ice camp")
)

df %>%
  ggplot() +
  geom_rect(
    data = mission,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = label
    ),
    alpha = 0.25
  ) +
  geom_line(data = df, aes(x = date, y = area)) +
  scale_x_date(date_labels = "%B-%Y") +
  xlab("Date") +
  ylab(bquote("Ice sheet cover (km"^2 ~ ")")) +
  scale_fill_manual(values = c("Amundsen" = "gray25", "Ice camp" = "gray75")) +
  labs(fill = "Green Edge missions") +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.99, 0.99)
  ) +
  labs(
    title = "Ice cover in 2016 (4 km resolution)",
    subtitle = "Data source: https://nsidc.org/"
  )

ggsave("graphs/ice_cover_2016.pdf", device = cairo_pdf)

## Export for MEOPAR poster
last_plot() + theme_bw(base_size = 20) + theme(
  legend.justification = c(1, 1),
  legend.position = c(0.99, 0.99)
) +
  labs(
    title = "Ice cover in 2016 (4 km resolution)",
    subtitle = "Data source: https://nsidc.org/"
  )

ggsave("~/Desktop/ice_cover_2016.png", dpi = 300)
