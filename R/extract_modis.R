# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract specific MODIS product from an hdf4 file.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#' Extract MODIS layer
#' 
#' @param fn File name of the hdf4
#' @param layer The sds layer to extract (Cloud_Fraction_Mean or
#'   Cloud_Optical_Thickness_Combined_Mean)
#'   
#' @return A RasterLayer
#' @export
#' 
#' @examples
modis_extract <- function(fn, layer) {
  
  # scientific dataset
  sds <- MODIS::getSds(fn)
  
  stopifnot(layer %in% sds$SDSnames)
  
  index <- which(grepl(layer, sds$SDSnames))
  
  r <- raster(sds$SDS4gdal[index]) %>% 
    raster2df() %>% 
    mutate(date = stringr::str_match(fn, "A(\\S{7})")[, 2]) %>% 
    mutate(date = as.Date(date, format = "%Y%j"))
  
  
  return(r)
}

raster2df <- function(r) {
  
  coord <- coordinates(r)
  value <- getValues(r)
  
  df <- data_frame(
    long = coord[, 1],
    lat = coord[, 2],
    value = value,
    proj4string = proj4string(r)
  )
  
  return(df)
  
}

# Process data ------------------------------------------------------------

files <- list.files(
  "data/modis/",
  pattern = ".hdf",
  full.names = TRUE,
  recursive = TRUE
)

cot <- map(files, modis_extract, layer = "Cloud_Optical_Thickness_Combined_Mean") %>% 
  bind_rows() %>% 
  rename(cot = value)

cf <- map(files, modis_extract, layer = "Cloud_Fraction_Mean") %>% 
  bind_rows() %>% 
  rename(cf = value)

res <- inner_join(cot, cf, by = c("long", "lat", "date", "proj4string")) %>% 
  dplyr::select(long, lat, date, cot, cf, proj4string)

write_feather(res, "data/modis/modis.feather")
