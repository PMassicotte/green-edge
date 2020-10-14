library(plotly)
rm(list = ls())

# c("G300", "G415", "G719") Stations where DOW have been manually set

ic <- read_csv("data/clean/ice_concentration_history_amundsen.csv") 

# %>% 
#   filter(station %in% c("G510", "G415", "G300", "G415", "G719"))

## When was the sampling?
sampling <- ic %>% 
  group_by(station) %>% 
  dplyr::filter(date_utc == ice_date)

ice3 <- ic %>% 
  filter(lubridate::month(ice_date) >= 5) %>% ## start looking after May
  crossing(sic_target = c(0.1, 0.5, 0.8)) %>% 
  group_by(station, sic_target) %>% 
  nest() %>% 
  mutate(dow = pmap(list(data, sic_target, station), function(df, sic_target, station) {
    
    # print(station)
    
    ## La stratégie ici est qu'il faut au moins 3 jours consécutifs avec une SIC en bas de la "target"
    res <- df %>%
      mutate(sic_difference = abs(sic - sic_target)) %>% 
      mutate(next_3_are_bellow_sic_target = rollapplyr(
        sic,
        width = 4,
        function(x) all(x[2:4] < sic_target), # Next three, not including self value
        coredata = TRUE,
        fill = NA,
        align = "left"
      )) %>% 
      filter(next_3_are_bellow_sic_target == TRUE) %>% 
      filter(sic_difference == min(sic_difference)) %>% 
      slice(1) %>% 
      mutate(dow = parse_number(date_utc - ice_date)) %>% 
      select(sampling_date = date_utc, dow_date = ice_date, dow_length = dow, dow_sic = sic)
    
    return(res)
    
  })) %>% 
  unnest(dow, .drop = FALSE)

ice3 %>% 
  count(station) %>% 
  arrange(desc(n))


# Manual classification ---------------------------------------------------

## This was done by visual inspection (Joannie)

ice3$dow_sic[ice3$station == "G209" & ice3$sic_target == 0.1] <- ic$sic[ic$station == "G209" & ic$ice_date == "2016-06-08"]
ice3$dow_date[ice3$station == "G209" & ice3$sic_target == 0.1] <- "2016-06-08"
ice3$dow_sic[ice3$station == "G209" & ice3$sic_target == 0.5] <- ic$sic[ic$station == "G209" & ic$ice_date == "2016-06-05"]
ice3$dow_date[ice3$station == "G209" & ice3$sic_target == 0.5] <- "2016-06-05"

ice3$dow_sic[ice3$station == "G300" & ice3$sic_target == 0.1] <- ic$sic[ic$station == "G300" & ic$ice_date == "2016-05-16"]
ice3$dow_date[ice3$station == "G300" & ice3$sic_target == 0.1] <- "2016-05-16"
ice3$dow_sic[ice3$station == "G300" & ice3$sic_target == 0.5] <- ic$sic[ic$station == "G300" & ic$ice_date == "2016-05-22"]
ice3$dow_date[ice3$station == "G300" & ice3$sic_target == 0.5] <- "2016-05-22"
ice3$dow_sic[ice3$station == "G300" & ice3$sic_target == 0.8] <- ic$sic[ic$station == "G300" & ic$ice_date == "2016-05-29"]
ice3$dow_date[ice3$station == "G300" & ice3$sic_target == 0.8] <- "2016-05-29"

ice3$dow_sic[ice3$station == "G414" & ice3$sic_target == 0.5] <- ic$sic[ic$station == "G414" & ic$ice_date == "2016-06-27"]
ice3$dow_date[ice3$station == "G414" & ice3$sic_target == 0.5] <- "2016-06-27"
ice3$dow_sic[ice3$station == "G414" & ice3$sic_target == 0.8] <- ic$sic[ic$station == "G414" & ic$ice_date == "2016-06-11"]
ice3$dow_date[ice3$station == "G414" & ice3$sic_target == 0.8] <- "2016-06-11"

ice3$dow_sic[ice3$station == "G415" & ice3$sic_target == 0.1] <- ic$sic[ic$station == "G415" & ic$ice_date == "2016-06-29"]
ice3$dow_date[ice3$station == "G415" & ice3$sic_target == 0.1] <- "2016-06-29"
ice3$dow_sic[ice3$station == "G415" & ice3$sic_target == 0.5] <- ic$sic[ic$station == "G415" & ic$ice_date == "2016-06-11"]
ice3$dow_date[ice3$station == "G415" & ice3$sic_target == 0.5] <- "2016-06-11"
ice3$dow_sic[ice3$station == "G415" & ice3$sic_target == 0.8] <- ic$sic[ic$station == "G415" & ic$ice_date == "2016-06-10"]
ice3$dow_date[ice3$station == "G415" & ice3$sic_target == 0.8] <- "2016-06-10"

ice3$dow_sic[ice3$station == "G416" & ice3$sic_target == 0.1] <- ic$sic[ic$station == "G416" & ic$ice_date == "2016-06-29"]
ice3$dow_date[ice3$station == "G416" & ice3$sic_target == 0.1] <- "2016-06-29"
ice3$dow_sic[ice3$station == "G416" & ice3$sic_target == 0.5] <- ic$sic[ic$station == "G416" & ic$ice_date == "2016-06-10"]
ice3$dow_date[ice3$station == "G416" & ice3$sic_target == 0.5] <- "2016-06-10"
ice3$dow_sic[ice3$station == "G416" & ice3$sic_target == 0.8] <- ic$sic[ic$station == "G416" & ic$ice_date == "2016-06-09"]
ice3$dow_date[ice3$station == "G416" & ice3$sic_target == 0.8] <- "2016-06-09"

ice3$dow_sic[ice3$station == "G510" & ice3$sic_target == 0.1] <- ic$sic[ic$station == "G510" & ic$ice_date == "2016-06-30"]
ice3$dow_date[ice3$station == "G510" & ice3$sic_target == 0.1] <- "2016-06-30"
ice3$dow_sic[ice3$station == "G510" & ice3$sic_target == 0.5] <- ic$sic[ic$station == "G510" & ic$ice_date == "2016-06-29"]
ice3$dow_date[ice3$station == "G510" & ice3$sic_target == 0.5] <- "2016-06-29"
ice3$dow_sic[ice3$station == "G510" & ice3$sic_target == 0.8] <- ic$sic[ic$station == "G510" & ic$ice_date == "2016-06-26"]
ice3$dow_date[ice3$station == "G510" & ice3$sic_target == 0.8] <- "2016-06-26"

ice3$dow_sic[ice3$station == "G719" & ice3$sic_target == 0.1] <- ic$sic[ic$station == "G719" & ic$ice_date == "2016-07-16"]
ice3$dow_date[ice3$station == "G719" & ice3$sic_target == 0.1] <- "2016-07-16"
ice3$dow_sic[ice3$station == "G719" & ice3$sic_target == 0.5] <- ic$sic[ic$station == "G719" & ic$ice_date == "2016-07-11"]
ice3$dow_date[ice3$station == "G719" & ice3$sic_target == 0.5] <- "2016-07-11"
ice3$dow_sic[ice3$station == "G719" & ice3$sic_target == 0.8] <- ic$sic[ic$station == "G719" & ic$ice_date == "2016-07-10"]
ice3$dow_date[ice3$station == "G719" & ice3$sic_target == 0.8] <- "2016-07-10"

ice4 <- ice3 %>% 
  mutate(dow_length = as.vector(sampling_date - dow_date))

anti_join(ice4 %>% select(-data), ice3 %>% select(-data))

ice3 <- ice4

# Save --------------------------------------------------------------------

ice3 %>% 
  select(-data) %>% 
  write_csv("data/clean/dow_amundsen_2016.csv")


# Visualize ---------------------------------------------------------------

ice3 %>%
  unnest() %>% 
  filter(sic_target == 0.1) %>% 
  filter(station == "G209") %>% 
  ggplot(aes(x = ice_date, y = sic)) +
  geom_line() +
  geom_point() +
  geom_point(data = sampling %>% filter(station == "G209"), aes(x = date_utc, y = sic, color = "Sampling date"), size = 3) +
  geom_point(data = ice3 %>%  filter(station == "G209"), aes(x = dow_date, y = dow_sic, color = factor(sic_target)), size = 3)

# +
#   ggforce::facet_wrap_paginate(~station, scales = "free", nrow = 6, ncol = 6, page = NULL)
  # facet_wrap(~station, scales = "free")

p <- plotly::ggplotly()

# https://plot.ly/r/getting-started/#initialization-for-online-plotting
# Sys.setenv("plotly_username"="PMassicotte")
# Sys.setenv("plotly_api_key"="F2BGTMvn3x9iG4MIBi8h")
# api_create(p, filename = "r-docs-midwest-boxplots")

# Manual correction -------------------------------------------------------

# The algo to determine the DOW 50 is not perfect. Joannie will tweak the file a
# bit and I will use this file to produce the final data.

