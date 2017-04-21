# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Take a file generated for calculating primary production and
#               convert it to Excel.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(tidyverse)
library(xlsx)

export_excel <- function(file, overwrite = FALSE) {
  
  destfile <- paste0(file, ".xlsx")
  
  if (file.exists(destfile) & overwrite == FALSE) {
    stop("File already exsists. Use overwrite = TRUE to force it.")
  }
  
  df <- suppressWarnings(suppressMessages(read_csv(file, skip = 5, na = c("", "*****")))) %>% 
    janitor::clean_names() 
  
  write.xlsx(as.data.frame(df), file = destfile, sheetName = basename(file), showNA = FALSE, row.names = FALSE)
  
  df <- df %>% 
    dplyr::select(-p, -pid, -s, dpm1) %>% 
    mutate(station = NA) %>% 
    mutate(cast = NA) %>% 
    mutate(niskin = NA) %>% 
    mutate(curve_id = NA) %>% 
    mutate(measurement_type = NA) %>%
    dplyr::select(station:measurement_type, dpm1) %>% 
    mutate(light = NA)
  
  destfile <- paste0(file, "_input.xlsx")
  
  write.xlsx(as.data.frame(df), file = destfile, sheetName = basename(file), showNA = FALSE, row.names = FALSE)
  
}

pvse_config_file <- function(dir, overwrite = FALSE) {
  
  if (dir.exists(dir) & overwrite == FALSE) {
    stop("File already exsists. Use overwrite = TRUE to force it.")
  }
  
  df <- data_frame(
    station = NA,
    cast = NA,
    niskin = NA,
    curve_id = NA,
    volume_incubation_ml = NA,
    volume_pipette_tc_ul = NA,
    incubation_time_min = NA
  )
  
  destfile <- paste0(dir, "/config_pvse_input.xlsx")
  write.xlsx(as.data.frame(df), file = destfile, sheetName = "sheet1", showNA = FALSE, row.names = FALSE)
  
}

# file <- "/home/pmassicotte/Downloads/Takuvik_14C_dpm.011"
# export_excel(file, overwrite = TRUE)
# pvse_config_file("/home/pmassicotte/Downloads/", overwrite = TRUE)
