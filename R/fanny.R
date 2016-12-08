
library(isin)
library(rhdf5)

# *************************************************************************
# Download the chla files.
# *************************************************************************

## Dates of interest
dates <- as.Date(c(
  "2016-06-28",
  "2016-07-01",
  "2016-07-06",
  "2016-07-07",
  "2016-07-08"
)) %>%
  format("%j")


files <- paste0("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2016", dates, ".L3b_DAY_CHL.nc")
destfiles <- paste0("data/modis-chla/", basename(files))

# map2(files, destfiles, download.file)


# Open the files ----------------------------------------------------------

# source("https://bioconductor.org/biocLite.R")
# biocLite("rhdf5")

get_modis <- function(file) {
  
  # h5ls("data/modis-chla/A2016180.L3b_DAY_CHL.nc")
  
  lonlat <- h5read(file, "/level-3_binned_data/BinList")
  
  chl <- h5read(file, "/level-3_binned_data/chlor_a") %>% 
    bind_cols(lonlat) %>% 
    bind_cols(bin2latlon(.$bin_num)) %>% 
    as_tibble() %>% 
    mutate(chla = sum / weights) %>% 
    filter(longitude > -90 & longitude < -50) %>% 
    filter(latitude > 40 & latitude < 90)
  
  proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  baffin <- readOGR("data/shapefiles/baffin/", "baffin")
  track <- readOGR("data/doc.kml", "Tracks")
  
  title <- stringr::str_match(file, "A(\\d{7})")[, 2] %>% 
    as.Date(., format = "%Y%j")
  
  p <- chl %>% 
    as.data.frame() %>% 
    ggplot(aes(x = longitude, y = latitude, color = log(chla))) +
    geom_point(size = 0.1) +
    geom_polygon(data = baffin, aes(x = long, y = lat, group = group), inherit.aes = FALSE, alpha = 0.2) +
    viridis::scale_color_viridis() +
    coord_map(projection = "stereo", xlim = c(-90, -50)) +
    geom_path(data = track, aes(x = long, y = lat), color = "red") +
    ggtitle(title)
  
  invisible(p)
  
}


p <- lapply(destfiles, get_modis)

p <- cowplot::plot_grid(plotlist = p, ncol = 3)

cowplot::save_plot("graphs/chla_fanny.pdf", p, base_height = 20, base_width = 20)

