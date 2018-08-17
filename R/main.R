library(tidyverse)
library(readxl)
library(extrafont)
library(rgdal)
library(RCurl)
library(ncdf4)
library(viridis)
# library(MODIS) # devtools::install_github("MatMatt/MODIS")
library(feather)
library(sp)
library(sf)
library(stringr)
library(parallel)
library(pbmcapply)

## Clear the workspace
rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font family
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

# source("R/extract_modis.R") # ATTENTION: takes few minutes