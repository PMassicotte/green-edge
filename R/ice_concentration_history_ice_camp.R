# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract sea ice history for the 2015 and 2016 ice camp.
# 
# ftp://ftp-projects.cen.uni-hamburg.de/seaice/AMSR2/3.125km/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# srcfile <- "http://www.obs-vlfr.fr/proof/php/GREENEDGE/docs/GE2016-ICECAMP_IceSample_DilutionFactor.xlsx"
# destfile <- tempfile(fileext = ".xlsx")
# 
# curl::curl_download(srcfile, destfile)
# 
# ## Read the coordinates and compute the mean position
# station <- readxl::read_excel(destfile) %>% 
#   janitor::clean_names() %>% 
#   select(latitude = lat_n, longitude = long_w) %>% 
#   summarise(latitude = mean(latitude), longitude = mean(longitude))

station <- data_frame(
  longitude = -63.78953333333333,
  latitude = 67.47973333333333
) %>% 
  SpatialPointsDataFrame(
    cbind(.$longitude, .$latitude),
    data = .,
    proj4string = CRS("+init=epsg:4326")
  )

extract_sic <- function(file, index) {
  
  sic <- nc_open(file) %>% 
    ncvar_get("sea_ice_concentration") %>% 
    as.vector()
  
  sic <- sic[index]
  
  res <- data_frame(
    sic,
    date = as.Date(str_extract(file, "\\d{8}"), format = "%Y%m%d")
  )
  
  return(res)
  
}

## Find the pixel index closes to the ice camp station
latitude <- nc_open("data/ice_concentration/LongitudeLatitudeGrid_3.125km_Arctic.nc") %>% 
  ncvar_get("latitude") %>% 
  as.vector()

longitude <- nc_open("data/ice_concentration/LongitudeLatitudeGrid_3.125km_Arctic.nc") %>% 
  ncvar_get("longitude") %>% 
  as.vector()

df <- data_frame(
  latitude,
  longitude,
) %>% 
  SpatialPointsDataFrame(cbind(.$longitude, .$latitude), data = ., proj4string = CRS("+init=epsg:4326"))

tree <- SearchTrees::createTree(coordinates(df))
inds <- SearchTrees::knnLookup(tree, newdat = coordinates(station), k = 1)

## Extract all SIC
files <- list.files("data/ice_concentration/ice-camp/", ".nc$", full.names = TRUE, recursive = TRUE)

res <- pbmclapply(files, extract_sic, index = inds, mc.cores = detectCores() - 1) %>% 
  bind_rows() %>% 
  mutate(latitude = coordinates(station)[2]) %>% 
  mutate(longitude = coordinates(station)[1])

## Export data
write_csv(filter(res, str_detect(date, "2014")), "data/clean/ice_concentration_history_ice_camp_2014.csv")
write_csv(filter(res, str_detect(date, "2015")), "data/clean/ice_concentration_history_ice_camp_2015.csv")
write_csv(filter(res, str_detect(date, "2016")), "data/clean/ice_concentration_history_ice_camp_2016.csv")

# Plot --------------------------------------------------------------------

title <-
  stringr::str_wrap(
    sprintf(
      "Sea ice concentration at Qikiqtarjuaq (lat = %2.6f, lon = %2.6f)",
      coordinates(station)[2],
      coordinates(station)[1]
    ))

caption <- "Source: ftp://ftp-projects.cen.uni-hamburg.de/seaice/AMSR2/3.125km/\nSee Data under 'List Parameters', 'param' = Sea Ice Concentration, 'resp' = Philippe Massicotte."

res %>% 
  mutate(year = format(date, "%Y")) %>% 
  ggplot(aes(x = date, y = sic)) +
  geom_point(size = 1) +
  geom_line() +
  facet_wrap(~year, scales = "free") +
  xlab("Date") +
  ylab("SIC (%)") +
  labs(title = title,
       caption = caption)

ggsave("graphs/ice_concentration_ice_camp_2015_2016.pdf", height = 4)
embed_fonts("graphs/ice_concentration_ice_camp_2015_2016.pdf")

