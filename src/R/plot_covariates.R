


ggplot(data = dat, aes(x = distance_km_per100, y = mob_per_pop_between)) + 
  geom_point(alpha = 0.1, size = 1) +
  scale_y_continuous(trans = "log", breaks = c(0.0001, 0.001, 0.01, 0.1)) +
  scale_x_continuous(trans = "log", breaks = c(0.1, 1))



dat$log_prop_diff = log(dat$prop_diff)

ggplot(data = dat, aes(x = log_prop_diff)) + 
  geom_histogram(alpha = 0.5)





ggplot(data = dat, aes(x = prop_diff, y = mob_per_pop_within_diff)) + 
  geom_point(alpha = 0.1, size = 1) +
  scale_y_continuous(trans = "log", breaks = c(0.0001, 0.001, 0.01, 0.1)) +
  scale_x_continuous(trans = "log", breaks = c(0.1, 1))




ggplot(data = dat, aes(x = distance_km_per100, y = mob_per_pop_within_diff)) + 
  geom_point(alpha = 0.1, size = 1) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log")


cor.test(dat$distance_km_per100, dat$mob_per_pop_between, method = "pearson")



cor.test(dat$distance_km_per100, dat$mob_per_pop_between, method = "spearman")
