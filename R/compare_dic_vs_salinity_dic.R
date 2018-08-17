dic <- read_excel("data/DIC.xlsx", "All Depths") %>% 
  gather(depth, dic, -1) %>% 
  set_names(c("date", "depth", "dic")) %>% 
  mutate(depth = parse_number(depth)) %>% 
  mutate(depth = ifelse(depth == 0, 0.5, depth)) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(dic = dic * 1027 / 1e6 * 12 * 1000)

salinity <- read_excel("data/pe-curves/GE2015-PvsE_DPM_20170412.xlsx", skip = 5, sheet = 2) %>% 
  janitor::clean_names() %>% 
  distinct(julianday, depth_m, salinity) %>% 
  filter(!grepl("ice", depth_m)) %>% 
  mutate(depth = parse_number(depth_m)) %>% 
  mutate(date = as.Date(as.character(julianday), "%Y%j"))

df <- inner_join(dic, salinity) %>% 
  mutate(dic_from_salinity = ((salinity * 0.067) - 0.05) * 0.96 * 12000)


p1 <- df %>%
  ggplot(aes(x = dic, y = dic_from_salinity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ylab("DIC from salinity (Parsons et al. 1984)")

p2 <- p1 + 
  xlim(c(25000, 26500)) +
  ylim(c(24000, 26500)) 

p <- cowplot::plot_grid(p1, p2, ncol = 1)

ggsave("graphs/dic_vs_salinity_dic.pdf", width = 5, height = 7)
