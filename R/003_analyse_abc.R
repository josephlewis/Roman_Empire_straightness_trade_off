set.seed(NULL)
set.seed(1)

RE_roads <- sf::st_read("./Data/Roman_roads/Roman_roads2.gpkg")
sf::st_geometry(RE_roads) <- "geometry"

RE_roads %>%
  sf::st_drop_geometry() %>%
  group_by(name) %>%
  count() %>%
  View()

sim_list <- list.files("./Output/RE_roads_simulated", full.names = TRUE, recursive = FALSE, pattern = ".rds")
sim_list <- gtools::mixedsort(sim_list)

road_sims <- lapply(sim_list, readRDS)

no_post_rows <- 100

tol <- no_post_rows/nrow(road_sims[[1]])

road_sims <- lapply(road_sims, function(x) { x[order(x$max_dist),]})
road_sims_posterior <- lapply(road_sims, function(x) { x[1:(nrow(road_sims[[1]])*tol),]})

road_sims_posterior <- do.call(rbind, road_sims_posterior)
road_sims_posterior <- road_sims_posterior %>%
  left_join(sf::st_drop_geometry(RE_roads[c("name", "RE_road_indx")]))

#road_sims_posterior$road_indx <- rep(1:length(road_sims), each = no_post_rows)
#road_sims_posterior$road_indx <- factor(road_sims_posterior$road_indx)

library(dplyr)

road_sims_posterior %>%
  summarise(mean_conf.low = quantile(p.b_mean_re, 0.025),
            mean_median = median(p.b_mean_re),
            mean_conf.high = quantile(p.b_mean_re, 0.975),
            sd_conf.low = quantile(p.b_sd_re, 0.025),
            sd_median = median(p.b_sd_re),
            sd_conf.high = quantile(p.b_sd_re, 0.975))

road_sims_posterior %>%
  group_by(name) %>%
  summarise(mean_conf.low = quantile(p.b_mean_province, 0.025),
            mean_median = median(p.b_mean_province),
            mean_conf.high = quantile(p.b_mean_province, 0.975),
            sd_conf.low = quantile(p.b_sd_province, 0.025),
            sd_median = median(p.b_sd_province),
            sd_conf.high = quantile(p.b_sd_province, 0.975),
            n = n()/100)

ggplot(road_sims_posterior) + 
  geom_density(aes(x = p.b_mean_province, colour = name)) + 
  labs(x = "parameter b", colour = "Province") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

ggplot(road_sims_posterior) + 
  geom_histogram(aes(x = p.b_mean_province)) + 
  facet_wrap(~name, ncol = 1) + 
  labs(x = "parameter b", colour = "Province") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

road_sims_posterior %>%
  summarise(conf.low = quantile(p.b, 0.025),
            median = median(p.b),
            conf.high = quantile(p.b, 0.975),
            n = n()/100)

# road_sims_posterior <- road_sims_posterior %>%
#   left_join(data.frame(RE_road_indx = 0:170, group = cut(0:170, breaks = 5, include.lowest = FALSE, right = TRUE)))

library(ggdist)
library(ggplot2)
library(ggthemes)

road_sims_posterior$RE_road_indx <- factor(road_sims_posterior$RE_road_indx, levels = (road_sims_posterior %>%
  group_by(RE_road_indx) %>%
  summarise(median = quantile(p.b, 0.5)) %>%
  arrange(desc(median)) %>%
  pull(RE_road_indx)))

road_sims_posterior <- road_sims_posterior %>%
  mutate(facet_group_ID = ntile(as.numeric(RE_road_indx), 6))

ggplot(road_sims_posterior) +
  stat_interval(aes(x = RE_road_indx, y = p.b)) +
  geom_hline(yintercept = 3.6, linetype = "dashed", colour = "black") +
  facet_wrap(~facet_group_ID, scales = "free_x", ncol = 2) + 
  scale_colour_brewer(palette = "Reds") +
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  labs(x = "Road ID", y = "parameter b", colour = "Credible Interval") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

ggplot(road_sims_posterior) +
  stat_pointinterval(aes(x = RE_road_indx, y = max_dist), size = 0.5) +
  facet_wrap(~facet_group_ID, scales = "free_x", ncol = 2) + 
  scale_colour_brewer(palette = "Reds") +
  #scale_y_continuous(breaks = seq(0, 4000, 500)) +
  labs(x = "Road ID", y = "parameter b") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right")

quantile(road_sims_posterior$max_dist, c(0.025, 0.5, 0.975), na.rm = TRUE)

# round(c(median = median(road_sims_posterior$p.b_mean_re), 
#         quantile(road_sims_posterior$p.b_mean_re, 0.025),
#         quantile(road_sims_posterior$p.b_mean_re, 0.975)), 2)
# 
# round(c(median = median(road_sims_posterior$p.b_mean_province), 
#         quantile(road_sims_posterior$p.b_mean_province, 0.025),
#         quantile(road_sims_posterior$p.b_mean_province, 0.975)), 2)
# 
# round(c(median = median(road_sims_posterior$p.b), 
#         quantile(road_sims_posterior$p.b, 0.025),
#         quantile(road_sims_posterior$p.b, 0.975)), 2)

quantile(truncnorm::rtruncnorm(n = 1e4, mean = sample(road_sims_posterior$p.b_mean_re, size = 1e4,replace = TRUE), sd = sample(road_sims_posterior$p.b_sd_re, size = 1e4, replace = TRUE), a = 0))

quantile(truncnorm::rtruncnorm(n = 1e4, mean = sample(road_sims_posterior$p.b_mean_province, size = 1e4,replace = TRUE), sd = sample(road_sims_posterior$p.b_sd_province, size = 1e4, replace = TRUE), a = 0))

quantile(truncnorm::rtruncnorm(n = 1e4, mean = sample(road_sims_posterior[road_sims_posterior$name == "Italy",]$p.b_mean_province, size = 1e4,replace = TRUE), sd = sample(road_sims_posterior[road_sims_posterior$name == "Italy",]$p.b_sd_province, size = 1e4, replace = TRUE), a = 0))
