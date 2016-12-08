rm(list = ls())

# df <- readxl::read_excel("data/pe-curves/GE2016-Box2A_Takuvik_14C_dpm.xlsx", skip = 3) %>% 
#   dplyr::select(dpm1 = DPM1)


#' Convert salinity to dissolved inorganic carbon (DIC)
#'
#' @param salinity Salinity measurement
#'
#' @return DIC value.
#' @export
#'
#' @examples
sal_to_dic <- function(salinity) {
  return(((salinity * 0.067) - 0.05) * 0.96 * 12000)
}

#' Fit PE curve to the data
#'
#' @param df A data frame containing data.
#'
#' @return
#' @export
#'
#' @examples
fit_pe <- function(df) {
  
  my_nlsLM <- safely(minpack.lm::nlsLM)
  
  mod <- my_nlsLM(
    p_manip ~ ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
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
    control = nls.control(maxiter = 200)
  )
  
  # mod
  
  return(mod$result)
}

#' Extract metrics from a fitted PE curve model
#'
#' @param mod A PE curve model fitted with nls.
#'
#' @return
#' @export
#'
#' @examples
calculate_metrics <- function(mod) {

  if (is.null(mod)) {
    return(data.frame(
      ps = NA,
      alpha = NA,
      beta = NA,
      p0 = NA,
      pm = NA,
      ek = NA,
      rss = NA,
      r2 = NA
    ))
  }
    
  ps <- coef(mod)["ps"]
  alpha <- coef(mod)["alpha"]
  beta <- coef(mod)["beta"]
  p0 <- coef(mod)["p0"]
  
  rss <- sum(residuals(mod) ^ 2)
  
  ## Calculate the pseudo-R2
  y <- mod$m$lhs()
  yy <- fitted(mod)
  r2 <- 1 - sum((y - yy) ^ 2) / (length(y) * var(y))
  
  ## Extra parameters (form the Excel sheet I was given)
  pm <- ps * (alpha / (alpha + beta)) * (beta / (alpha + beta)) ^ (beta / alpha)
  ek <- pm / alpha
  
  ## Return the data
  df <- data.frame(ps, alpha, beta, p0, pm, ek, rss, r2)
  
  return(df)
}

plot_curve <- function(res) {
  
  df <- res$fitted
  df2 <- res$corrected_data
  # metrics <- unlist(res$metrics)
  # 
  # s <- substitute(
  #   atop(
  #     "ps" == ps ~
  #       alpha == a ~
  #       beta == b ~
  #       "p0" == p0 ~
  #       "pm" == pm,
  #     "ek" == ek ~
  #       "rss" == rss ~
  #       R ^ 2 == r2
  #   ),
  #   list(
  #     ps =  round(metrics["ps"], digits = 2),
  #     a = round(metrics["alpha"], digits = 2),
  #     b = round(metrics["beta"], digits = 2),
  #     p0 = round(metrics["p0"], digits = 2),
  #     pm = round(metrics["pm"], digits = 2),
  #     ek = round(metrics["ek"], digits = 2),
  #     rss = round(metrics["rss"], digits = 2),
  #     r2 = round(metrics["r2"], digits = 4)
  #   )
  # )
  # 
  # s <-  as.character(as.expression(s))    
  
  p <- df2 %>%
    ggplot(aes(x = light, y = p_manip)) +
    geom_point() +
    # annotate("text", x = 700, y = 25, label = s, parse = TRUE, size = 3) +
    ggtitle(paste("Date:", format(as.Date(res$date, origin = "1970-01-01"), "%Y-%j"), "Depth:", res$depth)) +
    xlab("Light intensity") +
    ylab("Primary production") +
    ggrepel::geom_text_repel(aes(label = id), size = 2)
  
  if (is.null(df)) {
    return(p)
  } else {
    p <- p + geom_line(data = df, aes(y = .fitted), col = "red")
    return(p)
  }
  

  invisible(p)
}

check_data <- function(df) {
  
  p <- any(grepl("^p", df$id, ignore.case = T))
  t <- any(grepl("^t", df$id, ignore.case = T))
  b <- any(grepl("^b", df$id, ignore.case = T))
  
  all(p, t, b)
  
}

process_dpm <- function(df, volume_incubation, volume_pipette_tc) {
 
  ratio <- volume_incubation / volume_pipette_tc
  
  tc <- mean(df$dpm1[grepl("tc", df$id, ignore.case = TRUE)], na.rm = TRUE)
  black <- mean(df$dpm1[grepl("black", df$id, ignore.case = TRUE)], na.rm = TRUE)
  
  dic <- sal_to_dic(unique(df$salinity))
  
  data <- filter(df, grepl("^p", id, ignore.case = TRUE)) %>% 
    mutate(dpm1 = dpm1 - black) %>% 
    mutate(p_manip = dic * dpm1 / (tc * incub_time / 60) / ratio)
  
  return(data)
   
}

# *************************************************************************
# CTD 2015
# 
# 1. We need to estimate salinity at 1.5 m because it is not provided in the
# CTD file.
# 
# 2. We averaged salinity when multiple values provided for a same depth.
# *************************************************************************
ctd_2015 <-
  data.table::fread("data/pe-curves/Takuvik_GreenEdge2015.txt", sep = ";") %>%
  as_tibble() %>%
  dplyr::select(date = DateUTC,
                depth = `PRESS [db]`,
                salinity = `PSAL [psu]`) %>%
  mutate(date = as.Date(date, "%d-%m-%Y")) %>%
  group_by(date, depth) %>%
  summarise(salinity = mean(salinity, na.rm = TRUE)) %>% # mean of up and downcast
  nest() %>%
  mutate(estimated_salinity =
           map(data, ~ data.frame(approx(
             .$depth, .$salinity, xout = 1.5) # estimate salinity at 1.5m
           ))) %>% 
  unnest(estimated_salinity) %>% 
  rename(depth = x, salinity = y) %>% 
  ungroup() %>% 
  bind_rows(., unnest(., data)) %>% 
  dplyr::select(date, depth, salinity) %>% 
  arrange(date, depth) %>% 
  mutate(depth = as.character(depth))

# *************************************************************************
# test on real data
# *************************************************************************

volume_incubation <- 50 # 50 ml
volume_pipette_tc <- 0.02 # 20 ul

## Read the dpm file provided by Joannie
dpm_2015 <- readxl::read_excel(
  "data/pe-curves/GE2015-PvsE-Takuvik_14C_dpm.xlsx",
  skip = 3,
  col_types = c(rep("text", 17)),
  sheet = 2,
) %>%
  janitor::remove_empty_rows() %>%
  dplyr::select(
    dpm1 = DPM1,
    date = JulianDay,
    depth = `Depth (m)`,
    id = ID,
    incub_time = `Incu Time (min)`,
    light = Light,
    salinity = Salinity_ice
  ) %>%
  mutate(
    dpm1 = parse_number(dpm1),
    date = as.Date(date, "%Y%j"),
    incub_time = parse_number(incub_time),
    light = parse_number(light),
    salinity = parse_number(salinity)
  ) %>% 
  left_join(ctd_2015, by = c("date", "depth")) %>%
  replace_na(list(salinity.x = 0, salinity.y = 0)) %>% 
  mutate(salinity = salinity.x + salinity.y) %>% 
  dplyr::select(-salinity.y, -salinity.x) %>% 
  mutate(salinity = ifelse(salinity == 0, 31, salinity)) %>% # set sal to 31 if any missing values
  group_by(date, depth) %>% 
  nest() %>% 
  mutate(data_ok = map(data, check_data)) %>% # remove data that are missing information (tc, dark,)
  unnest(data_ok) %>% 
  filter(data_ok == TRUE) %>% 
  mutate(
    corrected_data = map(
      data,
      process_dpm,
      volume_incubation = volume_incubation,
      volume_pipette_tc = volume_pipette_tc
    )
  ) %>% 
  mutate(model = map(corrected_data, fit_pe)) %>%
  mutate(fitted = map(model, broom::augment)) %>%
  mutate(coefs = map(model, broom::tidy)) %>%
  mutate(metrics = map(model, calculate_metrics))

# dpm_2015

p <- apply(dpm_2015, 1, plot_curve)

pdf("/home/pmassicotte/Desktop/graphs.pdf", width = 5, height = 4)

for (i in 1:length(p)) {
  print(p[[i]])
}

dev.off()

metrics <- unnest(dpm_2015, metrics) %>%
  setNames(iconv(names(.), to = "ASCII//TRANSLIT")) %>%
  setNames(iconv(names(.), "UTF-8", "UTF-8", sub = "")) %>%
  mutate(station_id = format(date, "%Y%j")) %>% 
  dplyr::select(
    station_id,
    date,
    depth,
    ps,
    p0,
    alpha,
    beta,
    pm,
    ek,
    r2,
    rss
  )

# *************************************************************************
# Normalize the data
# 
# Take the total chla from another file to normalize some of the fitted
# parameters.
# *************************************************************************

pigment1 <- readxl::read_excel("data/pe-curves/GreenEdge2015_pigments_280415.xlsx",
                               "data",
                               na = "LOD") %>%
  janitor::clean_names() %>%
  dplyr::select(
    date = sampling_date_utc,
    latitude,
    longitude,
    depth = depth_m,
    tchla = total_chlorophyll_a,
    tchla_qa
  ) %>% 
  mutate(depth = as.character(-depth)) %>% 
  mutate(date = as.Date(date))


pigment2 <-
  readxl::read_excel("data/pe-curves/Green Edge 2015_HPLC_Ice 0-1cm_VG(pr le web).xlsx",
                     sheet = "Used Data") %>%
  janitor::clean_names() %>%
  dplyr::select(
    depth = sample_identification_2,
    date = sampling_date_2,
    type = na,
    tchla) %>% 
  mutate(type = iconv(type, to = "ASCII//TRANSLIT")) %>% 
  dplyr::filter(grepl("corrected", type)) %>% 
  dplyr::select(-type) %>% 
  mutate(depth = substr(depth, 1, 2)) %>% 
  mutate(date = as.Date(lubridate::parse_date_time(paste(date, "2015"), orders = "%d %B %Y")))

pigment <- bind_rows(pigment1, pigment2)

metrics <- metrics %>% mutate(depth = ifelse(grepl("ice", depth), substr(depth, 1, 2), depth))

res <- left_join(metrics, pigment, by = c("depth", "date")) %>% 
  mutate(alpha_star = alpha / tchla) %>% 
  mutate(beta_star = beta / tchla) %>% 
  mutate(pm_star = pm / tchla) %>% 
  mutate_if(is.numeric, funs(round(., digits = 2)))

write_csv(res, "/home/pmassicotte/Desktop/test.csv")

prob <- anti_join(metrics, pigment, by = c("depth", "date"))

# station(jd), long, lat, date, depth, ...