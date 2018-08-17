
rm(list = ls())

## CTD data for the water density and salinity

ctd <- data.table::fread("data/1508_ODV.txt", sep = "\t", skip = 7) %>% 
  repair_names() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(cruise, yyyy_mm_ddthh_mm_ss_sss, depth = pres_decibars, contains("sigt"), contains("psal")) %>% 
  mutate_at(vars(contains("sigt")), parse_number) %>% 
  mutate(date = anytime::anydate(yyyy_mm_ddthh_mm_ss_sss)) %>% 
  arrange(date, depth) %>% 
  group_by(date, depth) %>% ## between 1-3 CTD's per day, average data
  summarise(
    sigt_kg_m_3 = mean(sigt_kg_m_3, na.rm = TRUE),
    psal_psu = mean(psal_psu, na.rm = TRUE),
    n = n()
  ) %>% 
  mutate(water_density = sigt_kg_m_3 + 1000) %>% 
  mutate(doy = parse_number(format(date, "%j"))) %>% 
  ungroup()

ctd[is.na(ctd)] <- NA ## Replace NaN with NA

## Some CTD depth are missing, just complete it (mostly 0.5 and 1.5 m)

ctd <- ctd %>%  
  group_by(date) %>% 
  tidyr::complete(depth = c(0.5, 1.5, unique(depth)), date) %>% 
  fill(sigt_kg_m_3, psal_psu, water_density, doy, .direction = "up")

# ctd %>% 
#   filter(date == "2015-05-02") 

dic <- read_excel("data/DIC.xlsx", "All Depths") %>% 
  gather(depth, dic, -1) %>% 
  set_names(c("date", "depth", "dic")) %>% 
  drop_na(dic) %>% 
  mutate(depth = parse_number(depth)) %>% 
  mutate(depth = ifelse(depth == 0, 0.5, depth)) %>% 
  mutate(date = as.Date(date)) 

dic[is.na(dic)] <- NA

df <- dic %>% 
  left_join(ctd) %>% 
  group_by(date) %>% 
  arrange(date, depth) %>% 
  fill(water_density, .direction = "up") %>% 
  mutate(dic = dic * water_density / 1e6 * 12 * 1000)


# Model DIC from salinity -------------------------------------------------

p1 <- df %>% 
  ggplot(aes(x = psal_psu, y = dic)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "In-situ DIC vs CTD slainity")

p2 <- p1 +
  scale_x_continuous(limits = c(32.3, NA)) +
  scale_y_continuous(limits = c(25600, NA)) 

p <- cowplot::plot_grid(p1, p2, ncol = 1, labels = "AUTO")
ggsave("graphs/dic_vs_salinity.pdf", width = 5, height = 7)

## Model with all data

mod1 <- lm(dic ~ psal_psu, data = df)
plot(df$psal_psu, df$dic)
abline(mod1)
summary(mod1)

## Model with only high salinity data

df2 <- df %>% 
  filter(dic >= 25600 & psal_psu >= 32.3) 

mod2 <- lm(dic ~ psal_psu, data = df2)
summary(mod2)

plot(df2$psal_psu, df2$dic)
abline(mod2)

res <- ctd %>% 
  modelr::add_predictions(mod1, var = "dic_from_salinity_all_salinity") %>% 
  modelr::add_predictions(mod2, var = "dic_from_salinity_high_salinity") %>% 
  left_join(df) %>% 
  rename(dic_insitu = dic) %>% 
  mutate(dic_patrick = ifelse(psal_psu >= 30, 25900, 24000))

## Residuals

res <- res %>% 
  drop_na(dic_insitu) %>% 
  mutate(res_dic_from_salinity_all_salinity = abs(dic_insitu - dic_from_salinity_all_salinity)) %>% 
  mutate(res_dic_from_salinity_high_salinity = abs(dic_insitu - dic_from_salinity_high_salinity)) %>% 
  mutate(res_dic_patrick = abs(dic_insitu - dic_patrick)) %>% 
  select(date, dic_insitu, contains("res")) %>% 
  gather(method, residuals, contains("res"), -dic_insitu)

res %>% 
  ggplot(aes(x = method, y = residuals)) +
  geom_boxplot() +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  ylab("Residuals (ug/L)")

ggsave("graphs/histo_residuals_prediction_dic.pdf")


