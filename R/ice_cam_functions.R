# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Function to read settings and parameters associated with each
#               ice-cam image.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#' Read the parameter file associated to ice-cam images
#'
#' @param file Filename (ex.: Camera360_20160611134049_header.txt)
#'
#' @return A data frame with parameters.
#' @export
#'
#' @examples
read_parameter <- function(file) {
  
  library(stringr)
  
  # file <- "data/ice-cam/Camera360_20160611134049_header.txt"
  
  data <- read_lines(file)
  
  cruise_number <- str_match(data[1], "Cruise_Number: (\\S+)")[, 2]
  acquisition_number <- str_match(data[2], "Acquisition_Number: (\\S+)")[, 2]
  original_filename <- str_match(data[3], "Original_Filename: (\\S+)")[, 2]
  start_time <- str_match(data[4], "Start_Date_Time \\S+: (\\S+ \\S+)")[, 2]
  latitude <- str_match(data[5], "Initial_Latitude \\S+: (\\S+)")[, 2]
  longitude <- str_match(data[6], "Initial_Longitude \\S+: (\\S+)")[, 2]
  altitude <- str_match(data[7], "Altitude \\S+: (\\S+)")[, 2]
  roll <- str_match(data[8], "Roll \\S+: (\\S+)")[, 2]
  pitch <- str_match(data[9], "Pitch \\S+: (\\S+)")[, 2]
  heading <- str_match(data[10], "Heading \\S+: (\\S+)")[, 2]
  track <- str_match(data[11], "Track \\S+: (\\S+)")[, 2]
  speed <- str_match(data[12], "Speed \\S+: (\\S+)")[, 2]
  sun_azimut <- str_match(data[13], "Sun azimut \\S+: (\\S+)")[, 2]
  sun_zenith <- str_match(data[14], "Sun zenith \\S+: (\\S+)")[, 2]
 
  df <- data_frame(
    cruise_number,
    acquisition_number,
    original_filename,
    start_time = anytime::anytime(start_time),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    altitude = as.numeric(altitude),
    roll = as.numeric(roll),
    pitch = as.numeric(pitch),
    heading = as.numeric(heading),
    track = as.numeric(track),
    speed = as.numeric(speed),
    sun_azimut = as.numeric(sun_azimut),
    sun_zenith = as.numeric(sun_zenith)
  )
  
  return(df)
  
}

#' Read estimated angle for pixels at each row of an ice-cam image
#'
#' @param file File name (ex.: Camera360_20160611134049_calibration.txt)
#'
#' @return A data frame with distances.
#' @export
#'
#' @examples
read_pixel_distance <- function(file) {
  
  df <- read_lines(file) %>% 
    data_frame(pixel_distance = .) %>% 
    mutate(pixel_distance = parse_number(pixel_distance))
}