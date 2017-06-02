rm(list = ls())

read_ultrapath <- function(file) {
  
  # print(file)
  
  # file <- "data/raw/cdom/hly1401_UP_acdom/stn002_cn01_seaw/stn002_cn01_bot01_045_seaw_s.TXT"
  
  # file <- "data/raw/cdom/hly1401_UP_acdom/stn033_cn01_seaw/stn033_cn01_bot07_041_seaw_s_1.TXT"
  cdom <- read_lines(file)
  
  data_start <- which(grepl("DATA", cdom)) + 1
  
  ## Read the raw data
  cdom <- cdom[data_start:length(cdom)] %>% 
    strsplit("\t") %>% 
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    mutate_each(funs = funs(as.numeric))
  
  ## Meta data
  
  if (str_detect(file, "AM")) { ## Amundsen data
    
    into <- c("mission",
              "station",
              "cast",
              "bottle",
              "depth",
              "sample_type")
  } else { ## ice camp data
    
    into = c(
      "mission",
      "year",
      "jday",
      "water_type",
      "snow_type",
      "from_depth",
      "to_depth",
      "sample_type"
    )
  }
  
  
  meta <-
    data_frame(file_name = tools::file_path_sans_ext(basename(file))) %>%
    separate(file_name,
             into = into,
             sep = "_")
  
  ## Check if first column is wavelength
  
  has_wl <- !is.unsorted(cdom$V1, na.rm = TRUE)
  stopifnot(has_wl)
  
  cdom <- cdom %>% 
    drop_na()
  
  # stopifnot(nrow(cdom) == 528)
  
  cdom <- cdom %>% 
    set_names(c("wavelength", paste0("replicate_", 1:(ncol(.) - 1))))
  
  ## gather the data and fill metadata
  
  cdom <- cdom %>%
    gather(replicate, absorbance, -wavelength) %>%
    mutate(replicate = str_match(replicate, "_(\\S+)")[, 2]) %>% 
    mutate(replicate = parse_number(replicate)) %>% 
    bind_cols(meta[rep(1, nrow(.)), ]) %>% 
    mutate(absorption = 2.303 * absorbance / 2) %>% 
    dplyr::select(-absorbance) 
  
  return(cdom)
  
}

# Amundsen ----------------------------------------------------------------

files <- list.files("data/cdom/GE2016_AM/", "stn.+.TXT", full.names = TRUE, recursive = TRUE)
files <- files[!grepl("ori", files)]
# files[str_count(basename(files), "_") != 5]

cdom_amundsen <- purrr::map(files, read_ultrapath) %>% 
  bind_rows() %>% 
  filter(wavelength >= 250) %>% 
  filter(sample_type %in% c("r", "s"))

write_feather(cdom_amundsen, "data/cdom/cdom_amundsen_ge2016.feather")

# Ice camp 2016 ------------------------------------------------------------

files <- list.files("data/cdom/GE2016_IC/", "GE\\S+.TXT", full.names = TRUE, recursive = TRUE)
files <- files[!grepl("ori", files)]
# files[str_count(basename(files), "_") != 7]

cdom_ice_camp <- purrr::map(files, read_ultrapath) %>% 
  bind_rows() %>% 
  filter(wavelength >= 250) %>% 
  filter(sample_type %in% c("r", "s"))

write_feather(cdom_ice_camp, "data/cdom/cdom_ice_camp_ge2016.feather")


# Ice camp 2015 -----------------------------------------------------------

files <- list.files("data/cdom/GE2015_IC/", "GE\\S+.TXT$", full.names = TRUE, recursive = TRUE)
files <- files[!grepl("ori|other", files)]
files[str_count(basename(files), "_") != 7]

cdom_ice_camp <- purrr::map(files, read_ultrapath) %>% 
  bind_rows() %>% 
  filter(wavelength >= 250 & wavelength <= 722) %>% 
  filter(sample_type %in% c("r", "s"))

write_feather(cdom_ice_camp, "data/cdom/cdom_ice_camp_ge2015.feather")


# Plot Amundsen -----------------------------------------------------------

dat <- cdom_amundsen %>%
  group_by(mission, station, cast, bottle, depth) %>% 
  nest()

## Make sure we have the right number of observations
all(lapply(dat$data, nrow) %>% unlist() == 15296)

plot_spectra <- function(df, mission, station, cast, bottle, depth) {
  
  limits <- data_frame(
    y = c(-0.01, 0.01, -0.005, 0.005),
    type = c("dotted", "dotted", "dashed", "dashed"),
    sample_type = "r"
  )
  
  p1 <- df %>%
    ggplot(aes(x = wavelength, y = absorption)) +
    geom_line(aes(color = factor(replicate))) +
    facet_wrap(~sample_type, scales = "free") +
    ggtitle(paste(station, cast, bottle, depth, sep = " - ")) +
    labs(color = "Replicate") +
    xlab("Wavelength (nm)") +
    ylab(bquote(Absorption~(m^{-1}))) +
    geom_hline(data = limits, aes(yintercept = y, linetype = type), show.legend = FALSE) +
    guides(color = guide_legend(ncol = 2)) +
    theme(legend.key.size = )
  
  ## Plot the signal/noise ratio
  
  sn <- df %>% 
    mutate(absorption = ifelse(sample_type == "r", abs(absorption), absorption)) %>% 
    group_by(wavelength, sample_type) %>% 
    summarise(absorption = mean(absorption)) %>% 
    spread(sample_type, absorption) %>% 
    mutate(signal_to_noise = s / r) %>% 
    mutate(sample_type = "s") 
  
  p2 <- ggplot() + 
    geom_line(data = sn, aes(x = wavelength, y = signal_to_noise)) +
    scale_y_log10() +
    geom_hline(aes(yintercept = 100), lty = 1) +
    geom_hline(aes(yintercept = 10), lty = 2) +
    annotation_logticks(sides = "l") +
    xlab("Wavelength (nm)")
  
  p <- cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1))
  
  return(p)
  
}

pdf("graphs/cdom/cdom_amundsen_raw_spectra.pdf", width = 10, height = 7)

pmap(list(dat$data, dat$mission, dat$station, dat$cast, dat$bottle, dat$depth), plot_spectra)

dev.off()

# Plot ice camp -----------------------------------------------------------

dat <- cdom_ice_camp %>%
  group_by(mission, year, jday, water_type, snow_type, from_depth, to_depth) %>% 
  nest()

## Make sure we have the right number of observations (15296 for IC 2106)
which(lapply(dat$data, nrow) %>% unlist() != 15136)

plot_spectra <- function(df, mission, year, jday, water_type, snow_type, from_depth, to_depth) {
  
  limits <- data_frame(
    y = c(-0.01, 0.01, -0.005, 0.005),
    type = c("dotted", "dotted", "dashed", "dashed"),
    sample_type = "r"
  )
  
  p1 <- df %>%
    ggplot(aes(x = wavelength, y = absorption)) +
    geom_line(aes(color = factor(replicate))) +
    facet_wrap(~sample_type, scales = "free") +
    ggtitle(paste(mission, year, jday, water_type, snow_type, from_depth, to_depth, sep = " - ")) +
    labs(color = "Replicate") +
    xlab("Wavelength (nm)") +
    ylab(bquote(Absorption~(m^{-1}))) +
    geom_hline(data = limits, aes(yintercept = y, linetype = type), show.legend = FALSE)
  
  ## Plot the signal/noise ratio
  
  sn <- df %>%
    mutate(absorption = ifelse(sample_type == "r", abs(absorption), absorption)) %>%
    group_by(wavelength, sample_type) %>%
    summarise(absorption = mean(absorption)) %>%
    spread(sample_type, absorption) %>%
    mutate(signal_to_noise = s / r) %>%
    mutate(sample_type = "s")

  p2 <- ggplot() +
    geom_line(data = sn, aes(x = wavelength, y = signal_to_noise)) +
    scale_y_log10() +
    geom_hline(aes(yintercept = 100), lty = 1) +
    geom_hline(aes(yintercept = 10), lty = 2) +
    annotation_logticks(sides = "l") +
    xlab("Wavelength (nm)")

  p <- cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1))
  
  return(p)
  
}

pdf("graphs/cdom/cdom_icecamp2015_raw_spectra.pdf", width = 10, height = 7)

pmap(
  list(
    dat$data,
    dat$mission,
    dat$year,
    dat$jday,
    dat$water_type,
    dat$snow_type,
    dat$from_depth,
    dat$to_depth
  ),
  plot_spectra
)

dev.off()

