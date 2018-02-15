library(tidyverse)

rm(list = ls())

files <- list.files("data/bottles/1601_btl/", full.names = TRUE)

# header <- c(
#   "bottle", 
#   "time",
#   "sva",
#   "density",
#   "sigma_t",
#   "svcm",
#   "prdm",
#   "depsm",
#   "t090c",
#   "oxsat_ml_l",
#   "flsp",
#   "wet_cdom",
#   "par",
#   "spar",
#   "cstar_tr0",
#   "upoly0",
#   "sbeox0_ml_l",
#   "sal00"
# )

header2 <- c(
  "temp1",
  "bottle",
  "month",
  "day",
  "year",
  "sva",
  "sigma_t",
  "density",
  "sigma_theta",
  "svcm",
  "prdm",
  "depsm",
  "t090c",
  "oxsat_ml_l",
  "flsp",
  "wet_cdom",
  "par",
  "spar",
  "cstar_tr0",
  "upoly0",
  "sbeox0_ml_l",
  "sal00",
  "temp2",
  "temp3",
  "time",
  paste0("temp", 4:16)
)

## Read 2 lines by two lines and then create a data.frame
read_bottle <- function(file) {
  
  res <- read_lines(file)
  start <- which(str_detect(res, "Bottle"))
  
  res <- read_lines(file, skip = start + 1)
  
  res <- str_split(res, " +") 
  
  l <- list()
  cpt <- 1
  
  for (i in seq(1, length(res), by = 2)) {
    a <- str_split(res[[i]], " +") %>% unlist()
    b <- str_split(res[[i + 1]], " +") %>% unlist()
    line <- c(a, b)
    
    l[[cpt]] <- line
    cpt <- cpt + 1
    
  }
  
  res <- do.call(rbind, l) %>% 
    as_tibble() %>% 
    set_names(header2) %>% 
    select(-starts_with("temp")) %>% 
    mutate(file_name = basename(file))
  
  
  return(res)
  
}

# files <- "data/bottles/Amundsen_CTD_Leg1_FLUO/1601008.btl"

df <- map(files, read_bottle) %>% ## file 8 is wrong, skip it for now
  bind_rows() %>% 
  mutate(date = lubridate::parse_date_time(paste(year, month, day, time), order = "%Y %b %d %H:%M:%S", tz = "UTC")) %>% 
  select(-(month:year), -time) %>% 
  mutate(cast = str_extract(file_name, "(\\d{3})\\.")) %>% 
  mutate_at(.vars = vars(-date, -file_name), parse_number)

write_csv(df, "data/clean/bottle_list.csv")


