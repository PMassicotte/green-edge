rm(list = ls())

source("../cops/R/read_cops.R")

files <- list.files("data/cops/", full.names = TRUE)

df <- lapply(files, read_cops) %>% 
  setNames(basename(files)) %>% 
  bind_rows(.id = "file") %>% 
  filter(type == "edz") %>%
  # filter(wavelength >= 400 & wavelength <= 700) %>% 
  dplyr::select(-position, -degree) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(file, depth) %>% 
  nest()


# df %>% 
#   unnest() %>% 
#   filter(depth < 1.5) %>% 
#   ggplot(aes(x = wavelength, y = irradiance, group = depth)) +
#   geom_line() +
#   facet_wrap(~file)

myspline <- function(df) {
  
  wavelength <- seq(400, 700, by = 1)
  irradiance <- splinefun(df$wavelength, df$irradiance)(wavelength)
  
  res <- data_frame(wavelength, irradiance)
  
  return(res)
  
}

df <- df %>%
  mutate(splined = purrr::map(data, myspline)) %>% 
  mutate(par = purrr::map(splined, ~pracma::trapz(.$wavelength, .$irradiance) * 0.045998 / 10000))

# df %>% 
#   unnest(splined) %>% 
#   filter(depth < 1.5) %>% 
#   ggplot(aes(x = wavelength, y = irradiance, group = depth)) +
#   geom_line() +
#   facet_wrap(~file)

df <- df %>% 
  unnest(par)

df %>%
  ggplot(aes(x = depth, y = par)) +
  geom_point() +
  # scale_y_reverse() +
  
  facet_wrap(~file, scales = "free_x")

fit_nls <- function(df) {
  
  mod <- nls(
    par ~ a0 * exp(-kd * (depth - 10)) + k,
    data = df,
    start = list(a0 = 0.1, kd = 0.08, k = 0.5),
    control = list(maxiter = 300)
  )
  
  return(mod)
}

fit_lm <- function(df) {
  
  mod <- lm(log(par) ~ depth, data = df)
  
  return(mod)
}

df2 <- df %>%
  group_by(file) %>%
  nest() %>%
  mutate(mod_nls = purrr::map(data, fit_nls)) %>% 
  mutate(mod_lm = purrr::map(data, fit_lm)) %>% 
  mutate(predicted_nls = purrr::map(mod_nls, predict)) %>% 
  mutate(predicted_lm = purrr::map(mod_lm, predict))

# df2 %>% 
#   unnest(data, predicted) %>% 
#   ggplot(aes(x = depth, y = par)) +
#   geom_point() +
#   geom_line(aes(y = predicted), color = "red") +
#   scale_x_reverse() +
#   coord_flip() +
#   facet_wrap(~file)

# *************************************************************************
# Export results 
# *************************************************************************

df2 %>% unnest(mod_nls %>% purrr::map(broom::tidy)) %>% 
  write_csv("/home/pmassicotte/Desktop/nls_coeffs.csv")

df2 %>% unnest(mod_lm %>% purrr::map(broom::tidy)) %>% 
  write_csv("/home/pmassicotte/Desktop/lm_coeffs.csv")

# Plot the data -----------------------------------------------------------


plot_data <- function(file, data, predicted_nls) {
  
  predicted_nls <- data_frame(depth = data$depth, predicted_nls)
  
  p <- data %>%
    ggplot(aes(x = depth, y = par)) +
    geom_point() +
    geom_line(data = predicted_nls, aes(y = predicted_nls), color = "red") +
    scale_x_reverse() +
    coord_flip() +
    ggtitle(file) +
    ylab("PAR") +
    xlab("Depth (m)")
  
  invisible(p)
  
}

p <- purrr::pmap(list(df2$file, df2$data, df2$predicted_nls), plot_data)

pdf("graphs/cops_par.pdf")

lapply(p, function(x) x)

dev.off()

embed_fonts("graphs/cops_par.pdf")
