rm(list = ls())

# source("R/read_cdom.R")

# ****************************************************************************
# CDOM from the Amundsen
# ****************************************************************************

cdom_amundsen <- read_feather("data/cdom/cdom_amundsen_ge2016.feather")

cv <- cdom_amundsen %>%
  group_by(wavelength,
           mission,
           station,
           cast,
           bottle,
           depth,
           sample_type) %>%
  summarise(
    mean_absorption = mean(absorption),
    cv_absorption = abs(raster::cv(absorption))
  ) %>% 
  ungroup()

cdom <- cv %>% 
  dplyr::select(-cv_absorption) %>% 
  spread(sample_type, mean_absorption) %>% 
  mutate(absorption = s - r) %>% 
  group_by(mission, station, cast, bottle, depth) %>% 
  mutate(median_absorption_683_687 = median(absorption[between(wavelength, 683, 687)])) %>% 
  mutate(absorption = absorption - median_absorption_683_687) %>% 
  dplyr::select(-median_absorption_683_687)

cdom %>% 
  filter(station == "stnG100", bottle == "bottle007") %>% 
  ggplot(aes(x = wavelength, y = absorption)) +
  geom_line()

## Only keep CV from the samples (remove the reference samples)
cv <- cv %>% 
  filter(sample_type == "s") %>% 
  dplyr::select(-mean_absorption, -sample_type)

absorption_threshold_001 <- 2.303 * 0.01 / 2
absorption_threshold_0005 <- 2.303 * 0.005 / 2

cdom <- cdom %>% 
  ungroup() %>% 
  left_join(cv) %>% 
  group_by(mission, station, cast, bottle, depth) %>% 
  mutate(qf = 0) %>% 
  mutate(qf = ifelse(any(cv_absorption[between(wavelength, 350, 500)] > 5), 7, qf)) %>% 
  mutate(qf = ifelse(is.na(qf), 0, qf)) %>% # If CV was at 0, the previous line return NA, so replace it with 0
  # mutate(qf = ifelse(max(abs(r[between(wavelength, 350, 500)])) < absorption_threshold_0005, 8, qf)) %>% 
  mutate(qf = ifelse(between(median(abs(r[between(wavelength, 350, 500)])), absorption_threshold_0005, absorption_threshold_001), 1, qf)) %>%
  mutate(qf = ifelse(median(abs(r[between(wavelength, 350, 500)])) > absorption_threshold_001, 8, qf)) %>%
  mutate(qf = ifelse(absorption[wavelength == 350] > 1.8, 6, qf)) %>% 
  mutate(qf = ifelse(any(absorption[between(wavelength, 350, 500)] < 0), 9, qf))

write_csv(cdom, "data/clean/cdom_am.csv")

# ****************************************************************************
# CDOM from the ice camp
# ****************************************************************************

cdom_ice_camp <- read_feather("data/cdom/cdom_ice_camp_ge2015.feather")

cv <- cdom_ice_camp %>%
  group_by(wavelength,
           mission,
           year,
           jday,
           water_type,
           snow_type,
           from_depth,
           to_depth,
           sample_type) %>%
  summarise(
    mean_absorption = mean(absorption),
    cv_absorption = abs(raster::cv(absorption))
  ) %>% 
  ungroup()

cdom <- cv %>% 
  dplyr::select(-cv_absorption) %>% 
  spread(sample_type, mean_absorption) %>% 
  mutate(absorption = s - r) %>% 
  group_by(mission, year, jday, water_type, snow_type, from_depth, to_depth) %>% 
  mutate(median_absorption_683_687 = median(absorption[between(wavelength, 683, 687)])) %>% 
  mutate(absorption = absorption - median_absorption_683_687) %>% 
  dplyr::select(-median_absorption_683_687) %>% 
  ungroup()

# write_csv(cdom, "/home/pmassicotte/Desktop/uncorrected_spectra_IC2016.csv")

cv <- cv %>% 
  filter(sample_type == "s") %>% 
  dplyr::select(-mean_absorption, -sample_type)

cdom %>% 
  filter(water_type == "wat" & snow_type == "xx", jday == "112", to_depth == "0005m", from_depth == "x") %>% 
  ggplot(aes(x = wavelength, y = absorption)) +
  geom_line()

## Data needed to account for the diluation of the ice cores
dilution_info <- read_delim("data/cdom/ge2015_svfile_for_database_temp_20170512.csv", delim = "\t", locale = locale(encoding = "UTF-7")) %>% 
  janitor::clean_names() %>% 
  select(cast, jday = stnid, filtered_volumn_ml, added_fsw_ml, total_volumn_ml) %>% 
  mutate(jday = as.character(jday)) %>% 
  mutate(snow_type = str_match(cast, "HS|LS|xx")[, 1]) %>% 
  mutate(from_depth = str_match(cast, "(\\d)\\d*cm")[, 2]) %>% 
  mutate(to_depth = str_match(cast, "\\d(\\d*)cm")[, 2]) %>% 
  mutate(to_depth = ifelse(to_depth == "3", "003cm", "010cm")) %>% 
  mutate(f = (total_volumn_ml - added_fsw_ml) / total_volumn_ml)

# %>% 
#   filter(grepl("^ICE", cast)) %>% 
#   filter(!grepl("24h", cast))

## Merge dilution information with cdom data
r <- cdom %>% 
  left_join(dilution_info, by = c("jday", "from_depth", "to_depth", "snow_type")) 
  
dilution <- function(df) {
  
  ## Extract the spectra at the minumum depth
  a_fws <- df %>% 
    filter(is.na(cast)) %>% 
    mutate(to_depth = parse_number(to_depth)) %>% 
    filter(to_depth == min(to_depth)) %>% 
    select(wavelength, a_fws = absorption)

  res <- df %>% 
    left_join(a_fws, by = "wavelength") %>% 
    mutate(a = (absorption - a_fws * (1 - f)) / f) %>% 
    mutate(a = ifelse(is.na(a), absorption, a)) %>% 
    rename(absorption = a)
  
  return(res)
}

cdom <- r %>% 
  group_by(jday) %>% 
  nest() %>% 
  mutate(dilution = map(data, dilution)) %>% 
  unnest(dilution)

cdom <- cdom %>% 
  ungroup() %>% 
  left_join(cv) %>% 
  select(-a_fws) %>% 
  group_by(mission, year, jday, water_type, snow_type, from_depth, to_depth) %>% 
  distinct(wavelength, absorption, .keep_all = TRUE) %>% 
  mutate(qf = 0) %>% 
  mutate(qf = ifelse(any(cv_absorption[between(wavelength, 350, 500)] > 5), 7, qf)) %>% 
  mutate(qf = ifelse(is.na(qf), 0, qf)) %>% # If CV was at 0, the previous line return NA, so replace it with 0
  # mutate(qf = ifelse(max(abs(r[between(wavelength, 350, 500)])) < absorption_threshold_0005, 8, qf)) %>% 
  mutate(qf = ifelse(between(median(abs(r[between(wavelength, 350, 500)])), absorption_threshold_0005, absorption_threshold_001), 1, qf)) %>%
  mutate(qf = ifelse(median(abs(r[between(wavelength, 350, 500)])) > absorption_threshold_001, 8, qf)) %>%
  mutate(qf = ifelse(absorption[wavelength == 350] > 1.8, 6, qf)) %>% 
  mutate(qf = ifelse(any(absorption[between(wavelength, 350, 500)] < 0), 9, qf))

write_csv(cdom, "data/clean/cdom_ic_2105.csv")
