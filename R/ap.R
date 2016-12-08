rm(list = ls())

df <-
  read_delim(
    "/home/pmassicotte/Desktop/GE2015_all_ap_spec_20151024.out",
    delim = " ",
    col_names = TRUE
  )

meta <- df[1:5, ] %>% 
  gather(sample_id, value, matches("\\d{3}")) %>% 
  spread(ID, value)

dat <- df[7:nrow(df), ] %>% 
  gather(sample_id, absorbance, matches("\\d{3}")) %>% 
  rename(wavelength = ID) %>% 
  mutate(wavelength = parse_number(wavelength)) %>% 
  mutate(absorbance = parse_number(absorbance))

df <- inner_join(meta, dat, by = "sample_id") %>% 
  janitor::clean_names()

df %>%
  filter(wavelength <= 600) %>% 
  ggplot(aes(
    x = wavelength,
    y = absorbance,
    group = sample_id)
  ) +
  geom_line() +
  facet_wrap(~type, scales = "free")
