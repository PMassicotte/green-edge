cdom_corrected <- read_csv("data/clean/cdom_ic.csv")

p1 <- cdom_corrected %>% 
  filter(wavelength == 254) %>% 
  ggplot(aes(x = jday, y = absorption, group = interaction(from_depth, to_depth))) +
  geom_line(aes(color = to_depth)) +
  geom_point(aes(color = to_depth)) +
  facet_wrap(~snow_type, scales = "free_x") +
  labs(title = "Corrected spectra") +
  ylab(bquote(a[CDOM(254)]~(m^{-1})))

cdom_uncorrected <- read_csv("~/Desktop/uncorrected_spectra_IC2016.csv")

p2 <- cdom_uncorrected %>% 
  filter(wavelength == 254) %>% 
  ggplot(aes(x = jday, y = absorption, group = interaction(from_depth, to_depth))) +
  geom_line(aes(color = to_depth)) +
  geom_point(aes(color = to_depth)) +
  facet_wrap(~snow_type, scales = "free_x") +
  labs(title = "Uncorrected spectra") +
  ylab(bquote(a[CDOM(254)]~(m^{-1})))


p <- cowplot::plot_grid(p1, p2, ncol = 1)
p

ggsave("graphs/cdom/corrected_vs_uncorrected.pdf", width = 10, height = 10)


df <- bind_rows(cdom_corrected, cdom_uncorrected, .id = "correction")

df %>% 
  filter(wavelength %in% c(254, 350, 440) & water_type == "ice") %>% 
  ggplot(aes(x = jday, y = absorption)) +
  geom_line(aes(color = to_depth, linetype = correction)) +
  geom_point(aes(color = to_depth)) +
  facet_grid(wavelength~snow_type, scales = "free") +
  ylab(bquote(a[CDOM(lambda)]~(m^{-1}))) +
  scale_linetype_discrete(breaks = c(1, 2), labels = c("Corrected", "Uncorrected"))

ggsave("graphs/cdom/corrected_vs_uncorrected2.pdf", height = 8, width = 10)
