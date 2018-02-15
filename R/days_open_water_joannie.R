ic <- read_csv("data/clean/ice_history.csv")

## When was the sampling?
ice2 <- ic %>% 
  group_by(station) %>% 
  dplyr::filter(date_utc == ice_date)

ic %>% 
  ggplot(aes(x = ice_date, y = sic)) +
  geom_line() +
  geom_point(data = ice2, aes(x = date_utc, y = sic, color = "Sampling date")) +
  facet_wrap(~station)

plotly::ggplotly()

## Ice break up?

ice3 <- ic %>% 
  filter(lubridate::month(ice_date) >= 5) %>% 
  group_by(station) %>% 
  slice(which.min(abs(sic - 0.50)))

ic %>% 
  ggplot(aes(x = ice_date, y = sic)) +
  geom_line() +
  geom_point(data = ice2, aes(x = date_utc, y = sic, color = "Sampling date")) +
  geom_point(data = ice3, aes(x = ice_date, y = sic, color = "Ice 50%")) +
  facet_wrap(~station)

plotly::ggplotly()

res <- ice3 %>% 
  rename(dow_50_ice_date = ice_date) %>% 
  mutate(dow_50 = date_utc - dow_50_ice_date) %>% 
  mutate(method = NA)

write_csv(res, "~/Desktop/dow50.csv")


# Manual correction -------------------------------------------------------

# The algo to determine the DOW 50 is not perfect. Joannie will tweak the file a
# bit and I will use this file to produce the final data.

