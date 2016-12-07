download_icecover <- function(date, resolution, destination) {
  
  stopifnot(class(date) == "Date",
            resolution == 1 | resolution == 4,
            dir.exists(destination))
  
  file_name <- paste0("masie_ice_r00_v01_", format(date, "%Y%j"), "_", resolution, "km.zip")
  
  base_url <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02186/shapefiles"
  base_url <- paste0(base_url, "/",resolution, "km/", format(date, "%Y"), "/", file_name)
  
  print(base_url)
  
  destfile <- paste(destination, file_name, sep = "/")
  download.file(base_url, destfile = destfile, method = "libcurl")
  
  return(destfile)
  
}

dates <- seq(as.Date("2016-01-01"), as.Date(Sys.time()), by = 1)

res <- lapply(dates,
       download_icecover,
       resolution = 4,
       destination = "data/ice_cover/")

# Unzip -------------------------------------------------------------------

files <- list.files("data/ice_cover/", full.names = TRUE)

dir.create("data/ice_cover/extracted")

layer <- lapply(files, unzip, exdir = "data/ice_cover/extracted/") %>% 
  do.call(rbind, .)

# Save a rda file ---------------------------------------------------------

layer <- layer[grepl(".shp", layer)]
layer <- tools::file_path_sans_ext(basename(layer))
dsn <- "data/ice_cover/extracted/"

dat <- lapply(layer, readOGR, dsn = dsn)

ice_cover <- list(
  shapefile = unlist(dat, recursive = FALSE),
  date = as.Date(stringr::str_extract(layer, "\\d{7}"), "%Y%j")
)

save(ice_cover, file = "data/ice_cover/shapefiles.rda")

