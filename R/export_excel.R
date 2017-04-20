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
  
  
}


# file <- "/home/pmassicotte/Downloads/Takuvik_14C_dpm.011"
# export_excel(file, overwrite = TRUE)