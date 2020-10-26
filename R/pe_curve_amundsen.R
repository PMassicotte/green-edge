rm(list = ls())

calculate_metrics <- function(mod, data) {
  if (is.null(mod)) {
    return(
      data.frame(
        ps = NA,
        alpha = NA,
        beta = NA,
        p0 = NA,
        pm = NA,
        ek = NA,
        im = NA,
        ib = NA,
        alpha_b = NA,
        beta_b = NA,
        pb_max = NA,
        rss = NA,
        r2 = NA
      )
    )
  }

  chla <- unique(data$total_chlorophyll_a)

  ps <- coef(mod)["ps"]
  alpha <- coef(mod)["alpha"]
  beta <- coef(mod)["beta"]
  p0 <- coef(mod)["p0"]

  rss <- sum(residuals(mod)^2)

  ## Calculate the pseudo-R2
  y <- mod$m$lhs()
  yy <- fitted(mod)
  r2 <- 1 - sum((y - yy)^2) / (length(y) * var(y))

  ## Extra parameters (from the Excel sheet I was given)
  pm <- ps * (alpha / (alpha + beta)) * (beta / (alpha + beta))^(beta / alpha)
  ek <- pm / alpha

  im <- (ps / alpha) * log((alpha + beta) / beta)
  ib <- ps / beta

  ## Normalize
  alpha_b <- alpha / chla
  beta_b <- beta / chla
  pb_max <- pm / chla

  ## Return the data
  # df <- data.frame(ps, alpha, beta, p0, pm, ek, alpha_b, beta_b, pb_max, rss, r2)

  df <- data.frame(ps, alpha, beta, p0, pm, ek, im, ib, alpha_b, beta_b, pb_max, rss, r2)

  return(df)
}


sal_to_dic <- function(salinity) {

  # return(((salinity * 0.067) - 0.05) * 0.96 * 12000)

  # DIC in ug, validated by Marcel and Patrick R.
  dic <- 25900

  return(dic)
}

process_dpm <- function(df, volume_incubation, volume_pipette_tc) {
  ratio <- volume_incubation / volume_pipette_tc
  incub_time <- 120

  tc <- mean(df$dpm[grepl("tc", df$type, ignore.case = TRUE)], na.rm = TRUE)
  black <- mean(df$dpm[grepl("^t0$|^s1$|^s2$", df$type, ignore.case = TRUE)], na.rm = TRUE)

  dic <- sal_to_dic(unique(df$salinity))

  data <- filter(df, grepl("^s([3-9]|1[0-9]|20)", type, ignore.case = TRUE)) %>%
    mutate(dpm = dpm - black) %>%
    mutate(p_manip = dic * dpm / (tc * incub_time / 60) / ratio)

  return(data)
}

## Read all the data

file <- "data/pe-curves/Input_fileGE_Amundsen_PvsE_noOutliers_01022018.xlsx"

sheets <- readxl::excel_sheets(file)
sheets <- sheets[str_detect(sheets, "^G")]

df <- map2(file, sheets, read_excel, col_types = "text", na = c(" ", "NA", "na")) %>%
  set_names(sheets) %>%
  bind_rows(.id = "station") %>%
  janitor::clean_names() %>%
  drop_na(program) %>%
  mutate_at(.vars = vars(program, pid, sample, dpm, light, cast, bottle), parse_number)

## Fill missing values (bottle)

df <- df %>%
  group_by(station, curve_id) %>%
  fill(cast, bottle, .direction = "down") %>%
  fill(cast, bottle, .direction = "up") %>%
  ungroup()

## There are no cast and bottle associated with ice station. When this happends,
## fill the bottle with the highest bottle value from that cast.

df <- df %>%
  group_by(station) %>%
  fill(cast, .direction = "down") %>%
  fill(cast, .direction = "up") %>%
  mutate(bottle = ifelse(is.na(bottle), max(bottle, na.rm = TRUE), bottle)) %>%
  ungroup()

## Match with bottle data

bottle <- read_csv("data/clean/bottle_list.csv") %>%
  select(date, cast, bottle, depth = depsm, salinity = sal00)

df <- df %>%
  left_join(bottle, by = c("cast", "bottle")) %>%
  arrange(station, cast, curve_id)

## Check if some PE curve are not associated to a bottle
res <- df %>%
  anti_join(bottle, by = c("cast", "bottle"))

stopifnot(nrow(res) == 0)

# write_csv(df, "~/Desktop/pe.csv")

df <- df %>%
  filter(!str_detect(curve_id, "ice"))

## Read chla

chla <- read_excel("data/pe-curves/GreenEdge-Amundsen-pigments-180131.xlsx") %>%
  janitor::clean_names() %>%
  select(station, bottle, total_chlorophyll_a)

anti_join(df, chla) %>% distinct(station, bottle, depth)

df <- df %>%
  inner_join(chla, by = c("station", "bottle"))

## Calculate p_manip

volume_incubation <- 1.5 # 1.5 ml
volume_pipette_tc <- 0.02 # 20 ul

df <- df %>%
  group_by(station, curve_id, depth) %>%
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
  group_by(station, curve_id, depth) %>%
  mutate(model_type = if_else(p_manip[which.max(light)] < max(p_manip, na.rm = TRUE), "model1", "model2"))


# ****************************************************************************
# Fit the data
# ****************************************************************************

fit_pe <- function(df, model_type) {

  # print(model_type)

  mod <- NA

  if (is.na(model_type)) {
    return(NULL)
  }

  opt <- nls.control(maxiter = 400, minFactor = 1e-10, tol = 1e-10)

  safe_nls <- safely(minpack.lm::nlsLM)

  if (model_type == "model1") {
    mod <- safe_nls(
      p_manip ~
      ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
      data = df,
      start = list(
        ps = max(df$p_manip, na.rm = TRUE),
        alpha = 0.1,
        beta = 0.000001,
        p0 = 0
      ),
      lower = c(
        ps = min(df$p_manip, na.rm = TRUE),
        alpha = 0,
        beta = 0,
        p0 = -Inf
      ),
      upper = c(
        ps = Inf,
        alpha = Inf,
        beta = Inf,
        p0 = Inf
      ),
      control = opt
    )
  } else if (model_type == "model2") {
    mod <- safe_nls(
      p_manip ~
      ps * (1 - exp(-alpha * light / ps)) + p0,
      data = df,
      start = list(
        ps = 2,
        alpha = 0.01,
        p0 = 0.01
      ),
      lower = c(
        ps = min(df$p_manip, na.rm = TRUE),
        alpha = 0,
        p0 = -Inf
      ),
      upper = c(
        ps = Inf,
        alpha = Inf,
        p0 = Inf
      ),
      control = opt
    )
  }

  return(mod$result)
}

plot_raw <- function(df, station, curve_id, date, depth) {
  df %>%
    ggplot(aes(x = light, y = p_manip)) +
    geom_point() +
    labs(title = paste(station, curve_id, date, depth, sep = "--")) +
    ggrepel::geom_text_repel(aes(label = type))
}

df <- df %>%
  group_by(station, curve_id, date, depth, model_type) %>%
  nest() %>%
  # slice(1) %>%
  mutate(p = pmap(list(data, station, curve_id, as.character(date), depth), plot_raw))

# pdf("graphs/pe-curves/ge2016-amundsen.pdf", height = 6, width = 8)
# df$p
# dev.off()


##########################################################################

## TODO
df <- df %>%
  mutate(model = purrr::map2(data, model_type, fit_pe))

pred <- df %>%
  mutate(is_fitted = purrr::map(model, function(x) {
    !is.null(x)
  })) %>%
  unnest(is_fitted) %>%
  filter(is_fitted) %>%
  mutate(pred = purrr::map2(data, model, modelr::add_predictions))

# ****************************************************************************
# Plot the data
# ****************************************************************************
plot_curve <- function(df, station, model, date, depth, model_type) {
  df <- df %>%
    mutate(model_type = model_type)

  # mod2 <- phytotools::fitPGH(df$light, df$p_manip)
  # print(mod2)

  alpha <- coef(model)["alpha"]
  p0 <- coef(model)["p0"]
  ps <- coef(model)["ps"]
  beta <- coef(model)["beta"]

  pm <- ps * (alpha / (alpha + beta)) * (beta / (alpha + beta))^(beta / alpha)
  ek <- pm / alpha

  p <- df %>%
    ggplot(aes(x = light, y = p_manip)) +
    geom_point() +
    ggtitle(paste("Station: ", station, "Date:", format(as.Date(date, origin = "1970-01-01"), "%Y-%j"), "Depth:", depth)) +
    xlab("Light intensity") +
    ylab("Primary production") +
    ggrepel::geom_text_repel(aes(label = type), size = 2) +
    theme(
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 6),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    guides(
      color =
        guide_legend(
          label.position = "top",
          keywidth = unit(2, "cm"),
          keyheight = unit(0.1, "cm")
        )
    )

  if (!is.null(model)) {
    pred <- df %>%
      modelr::data_grid(light = modelr::seq_range(light, 100)) %>%
      modelr::add_predictions(., model)

    lab <- glue::glue(
      'alpha = {signif(coef(model)["alpha"], 3)},
      beta = {signif(coef(model)["beta"], 3)},
      p0 = {signif(coef(model)["p0"], 3)},
      ps = {signif(coef(model)["ps"], 3)},
      pm = {signif(pm, 3)},
      ek = {signif(ek, 3)}'
    )

    p <- p +
      geom_line(data = pred, aes(x = light, y = pred), color = "#3c3c3c") +
      labs(subtitle = lab) +
      geom_abline(
        slope = alpha,
        intercept = p0,
        color = "blue",
        lwd = 0.25,
        lty = 2,
        key_glyph = "rect"
      ) +
      geom_hline(
        aes(yintercept = p0, color = "p0"),
        lwd = 0.25,
        lty = 2,
        key_glyph = "rect"
      ) +
      geom_hline(aes(yintercept = pm + p0, color = "pm + p0"),
        lwd = 0.25,
        lty = 2,
        key_glyph = "rect"
      ) +
      geom_hline(aes(yintercept = ps + p0, color = "ps + p0"),
        lwd = 0.25,
        lty = 2,
        key_glyph = "rect"
      ) +
      geom_vline(aes(xintercept = ek, color = "ek"),
        lwd = 0.25,
        lty = 2,
        key_glyph = "rect"
      )
  }

  return(p)
}

# pred <- pred %>%
#   ungroup() %>%
#   slice(1)

pdf("graphs/pe-curves/ge2016-amundsen_fitted.pdf", width = 5, height = 4.5)

pmap(
  list(
    pred$data,
    pred$station,
    pred$model,
    pred$date,
    pred$depth,
    pred$model_type
  ),
  plot_curve
)

dev.off()

params <- pred %>%
  mutate(params = purrr::map2(model, data, calculate_metrics)) %>%
  unnest(params) %>%
  dplyr::select(-data, -model, -pred, -p)

write_csv(params, "data/pe-curves/photosynthetic_parameters_amundsen_2016.csv")
