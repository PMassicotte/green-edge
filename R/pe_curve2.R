rm(list = ls())

sal_to_dic <- function(salinity) {
  return(((salinity * 0.067) - 0.05) * 0.96 * 12000)
}

process_dpm <- function(df, volume_incubation, volume_pipette_tc) {
  
  ratio <- volume_incubation / volume_pipette_tc
  
  tc <- mean(df$dpm1[grepl("tc|t0", df$id, ignore.case = TRUE)], na.rm = TRUE)
  black <- mean(df$dpm1[grepl("black|dark", df$id, ignore.case = TRUE)], na.rm = TRUE)
  
  dic <- sal_to_dic(unique(df$salinity))
  
  data <- filter(df, grepl("^p", id, ignore.case = TRUE)) %>% 
    mutate(dpm1 = dpm1 - black) %>% 
    mutate(p_manip = dic * dpm1 / (tc * incub_time / 60) / ratio)
  
  return(data)
  
}

file <- "data/pe-curves/GE2015-PvsE-Takuvik_14C_dpm_20170302.xlsx"
# file <- "data/pe-curves/GE2016-PvsE_DPM.xlsx"

df <- read_excel(
  file,
  skip = 5,
  sheet = 2
) %>%
  janitor::clean_names() %>%
  janitor::remove_empty_cols() %>%
  janitor::remove_empty_rows() %>%
  mutate(date = as.Date(as.character(julianday), format = "%Y%j")) %>%
  dplyr::select(-julianday) %>%
  dplyr::select(id,
                date,
                depth_m,
                time,
                incub_time = incu_time_min,
                dpm1,
                light,
                salinity) %>%
  mutate(time = parse_number(time)) %>%
  drop_na(dpm1)

# %>% 
#   mutate(depth_m = parse_number(depth_m))

volume_incubation <- 50 # 50 ml
volume_pipette_tc <- 0.02 # 20 ul

df <- df %>% 
  group_by(date, depth_m) %>% 
  nest() %>% 
  mutate(
    dpm = purrr::map(
      data,
      process_dpm,
      volume_incubation = volume_incubation,
      volume_pipette_tc = volume_pipette_tc
    )
  ) %>% 
  unnest(dpm)

# ****************************************************************************
# Find out if we should use model type 1 or 2
# ****************************************************************************

df <- df %>% 
  group_by(date, depth_m) %>% 
  mutate(model_type = if_else(p_manip[which.max(light)] < max(p_manip), "model1", "model2"))


# ****************************************************************************
# Fit the data
# ****************************************************************************

fit_pe <- function(df) {
  
  mod <- NA
  
  opt <- nls.control(maxiter = 400, minFactor = 1e-10, tol = 1e-10)
  
  mynls <- safely(minpack.lm::nlsLM)
  
  if (unique(df$model_type) == "model1") {
    
    mod <- mynls(
      p_manip ~
        ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
      data = df,
      start = list(
        ps = max(df$p_manip, na.rm = TRUE),
        alpha = 0,
        beta = 0,
        p0 = 0.1
      ),
      lower = c(
        ps =  0,
        alpha = 0,
        beta = 0,
        p0 = -Inf
      ),
      control = opt
    )
    
  } else if (unique(df$model_type) == "model2") {
    
    mod <- mynls(
      p_manip ~
        ps * (1 - exp(-alpha * light / ps)) + p0,
      data = df,
      start = list(
        ps = 3,
        alpha = 0.01,
        p0 = 0.01
      ),
      lower = c(0, 1e-7, -Inf),
      control = opt
    )
    
  }
  
  return(mod$result)
  
}

df <- df %>% 
  group_by(date, depth_m) %>% 
  nest() %>% 
  mutate(model = purrr::map(data, fit_pe)) 

# pred <- df %>% 
#   mutate(is_fitted = purrr::map(model, function(x) {!is.null(x)} )) %>% 
#   unnest(is_fitted) %>% 
#   filter(is_fitted) %>% 
#   mutate(pred = purrr::map2(data, model, modelr::add_predictions))

# ****************************************************************************
# Plot the data
# ****************************************************************************
plot_curve <- function(df, model, date, depth) {
  
  p <- df %>% 
    ggplot(aes(x = light, y = p_manip)) +
    geom_point() +
    ggtitle(paste("Date:", format(as.Date(date, origin = "1970-01-01"), "%Y-%j"), "Depth:", depth)) +
    xlab("Light intensity") +
    ylab("Primary production") +
    ggrepel::geom_text_repel(aes(label = id), size = 2)
  
  
  if (!is.null(model)) {
    
    df <- modelr::add_predictions(df, model)
    
    p <- p + 
      geom_line(data = df, aes(x = light, y = pred, color = model_type)) +
      theme(legend.position = c(1, 0), legend.justification = c(1.1, -0.5))
  }
  
  return(p)
}

pdf("graphs/pe-curves/ge2016.pdf", width = 5, height = 4)

pmap(list(df$data, df$model, df$date, df$depth_m) , plot_curve)

dev.off()


df %>% 
  mutate(coef = purrr::map(model, broom::tidy)) %>% 
  unnest(coef)
