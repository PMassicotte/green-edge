#' Extract SST from a netCDF file
#'
#' @param file Full filepath of the netCDF file.
#'
#' @return
#' @export
#'
#' @examples
read_sst <- function(file) {
  
  df <- nc_open(file)
  
  sst <- ncvar_get(df, "sst")
  lat <- ncvar_get(df, "lat")
  time <- ncvar_get(df, "time")
  long <- ncvar_get(df, "lon")
  
  longlat <- expand.grid(long, lat)
  
  df <- data_frame(long = longlat$Var1,
                   lat = longlat$Var2,
                   sst = as.vector(sst)) %>%
    mutate(long = ifelse(long > 180, long - 360, long)) %>% 
    mutate(date = as.Date(time, origin = "1978-01-01"))
  
  return(df)
  
}

# files <- list.files("data/sst/", pattern = ".nc$", recursive = TRUE, full.names = TRUE)
# 
# res <- lapply(files, read_sst) %>% 
#   bind_rows()
# 
# 
# write_feather(res, "/home/pmassicotte/Desktop/test.feather")
