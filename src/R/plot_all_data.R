library(dplyr)
library(ggplot2)
library(cowplot)
library(tmap) # to plot the map
library(sf)
library(scales) # for log y scale

# Define the breaks manually starting from October 2020
date_breaks <- seq(as.Date("2020-10-01"), as.Date("2022-03-30"), by = "2 months")

# National ONS prevalence data
prev <- readRDS("data/processed/ons_prev_england.rds")

# Number of clusters each month
n_clusters <- read.csv("data/processed/n_clusters.csv")

# LTLA specific prevalences
dat_og <- read.csv("data/raw/ltla_in_region_debiased_prev.csv")

coef = 100
p1 <- ggplot() +
  geom_line(data = dat_og %>% 
              filter(week_date>= "2020-10-01",
                     week_date<= "2022-03-31"),
            aes(x = as.Date(week_date), y = mean_prev, group = location_fine), 
            color = "grey", alpha = 0.3)+
  geom_line(data = prev %>% 
              filter(Date_og>= "2020-10-01",
                     Date_og<= "2022-03-31"),
            aes(x = Date_og, y = mean_prev), color = "#0d3b66")+
  geom_ribbon(data = prev %>% 
                filter(Date_og>= "2020-10-01",
                       Date_og<= "2022-03-31"),
              aes(x = Date_og, ymin = lower, ymax = upper), fill = "#0d3b66", alpha = 0.6) +
  scale_x_date(date_labels = "%d %b %y", breaks = date_breaks) +
  labs(x = "Date", y = "Prevalence") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 14),        # X axis text size
        axis.text.y = element_text(size = 14))
p1


# Mobility
mob <- read.csv("data/raw/covariates/mobility_lad_weekly.csv")

p2 <- mob %>%
  filter(week_date>= "2020-10-01") %>%
  group_by(week_date, type) %>%
  summarise(trips = sum(trips)) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(week_date), y = trips, group = type)) +
  geom_vline(xintercept = as.Date("2020-11-5")) +
  geom_vline(xintercept = as.Date("2020-12-2"), linetype="dotted") +
  geom_vline(xintercept = as.Date("2021-01-6")) +
  geom_vline(xintercept = as.Date("2021-03-08"), linetype="dashed") +  
  geom_vline(xintercept = as.Date("2021-12-8")) +
  geom_line(aes(color = type)) +
  scale_x_date(date_labels = "%d %b %y", breaks = date_breaks) +
  # scale_x_date(date_labels = "%b %y", date_breaks = "2 months", limits = c(as.Date("2020-10-01"), NA)) +
  labs(x = "Date", y = "Number of trips in a week") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 14),        # X axis text size
        axis.text.y = element_text(size = 14)) +
  scale_color_manual(values = c("within" = "#606c38", 
                                "between" = "#bb8588"),
                     labels = c("within" = "Within LTLA", 
                                "between" = "Between LTLAs"))

p2


# Monthly mobility per capita, all data
mob_between <- readRDS("data/processed/covariates/ltla_monthly_mobility_processed_between.rds") %>%
  mutate(month_year = as.Date(paste("01", mmyy, sep = "-"), format = "%d-%m-%Y"))
mob_within <- readRDS("data/processed/covariates/ltla_monthly_mobility_processed_within.rds") %>%
  mutate(month_year = as.Date(paste("01", mmyy, sep = "-"), format = "%d-%m-%Y"))

p <- ggplot() +
  geom_jitter(data = mob_between, aes(x = month_year, y = mob_per_pop, 
                                      group = month_year, color = "between"), size = 0.5, alpha = 0.2) +
  geom_jitter(data = mob_within, aes(x = month_year, y = mob_per_pop, 
                                     group = month_year, color = "within"), size = 0.5, alpha = 0.2) +
  geom_violin(data = mob_between, aes(x = month_year, y = mob_per_pop, 
                                      group = month_year, color = "between"), alpha = 0.5) +
  geom_violin(data = mob_within, aes(x = month_year, y = mob_per_pop, 
                                     group = month_year, color = "within"), alpha = 0.5) +
  labs(x = "Date", y = "Number of trips in a month per capita") +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  scale_y_continuous(trans = 'log10') +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 14),        # X axis text size
        axis.text.y = element_text(size = 14)) +
  scale_color_manual(values = c("within" = "#606c38", 
                                "between" = "#bb8588"),
                     labels = c("within" = "Within LTLA", 
                                "between" = "Between LTLAs"))
p
ggsave(p, file = "outputs/epi/mob_per_pop_alldata.png", width = 8, height = 6)


# Monthly trips per capita summaries of data
mob_between <- readRDS("data/processed/covariates/ltla_monthly_mobility_processed_between.rds") 
summary(mob_between$mob_per_pop)
mob_between <- mob_between %>%
  group_by(mmyy) %>%
  summarise(mean_mob_per_pop = mean(mob_per_pop, na.rm = TRUE),
            sd_mob_per_pop = sd(mob_per_pop, na.rm = TRUE),
            mob_per_pop_50 = quantile(mob_per_pop, na.rm = TRUE, 0.5),
            mob_per_pop_lower = quantile(mob_per_pop, na.rm = TRUE, 0.025),
            mob_per_pop_upper = quantile(mob_per_pop, na.rm = TRUE, 0.975)) %>%
  mutate(month_year = as.Date(paste("01", mmyy, sep = "-"), format = "%d-%m-%Y")) %>%
  ungroup()

mob_within <- readRDS("data/processed/covariates/ltla_monthly_mobility_processed_within.rds") 
summary(mob_within$mob_per_pop)
mob_within <- mob_within %>%
  group_by(mmyy) %>%
  summarise(mean_mob_per_pop = mean(mob_per_pop, na.rm = TRUE),
            sd_mob_per_pop = sd(mob_per_pop, na.rm = TRUE),
            mob_per_pop_50 = quantile(mob_per_pop, na.rm = TRUE, 0.5),
            mob_per_pop_lower = quantile(mob_per_pop, na.rm = TRUE, 0.025),
            mob_per_pop_upper = quantile(mob_per_pop, na.rm = TRUE, 0.975)) %>%
  mutate(month_year = as.Date(paste("01", mmyy, sep = "-"), format = "%d-%m-%Y")) %>%
  ungroup()

date_breaks <- seq(as.Date("2020-10-01"), as.Date("2022-03-30"), by = "2 months")
ggplot() +
  # geom_line(aes(y = y = mean_mob_per_pop, color = "between")) +
  # geom_ribbon(aes(ymin = mean_mob_per_pop - 1.96*sd_mob_per_pop, 
  #                 ymax = mean_mob_per_pop + 1.96*sd_mob_per_pop,
  #                 fill = "between"),
  #             alpha = 0.7, show.legend = FALSE)+
  geom_vline(xintercept = as.Date("2020-11-5")) +
  geom_vline(xintercept = as.Date("2021-01-6")) +
  geom_vline(xintercept = as.Date("2021-12-8")) +
  geom_line(data = mob_between, aes(x = month_year, y = mob_per_pop_50, color = "between")) +
  geom_ribbon(data = mob_between, aes(x = month_year,
                                      ymin = mob_per_pop_lower,
                                      ymax = mob_per_pop_upper,
                                      fill = "between"),
                                  alpha = 0.7, show.legend = FALSE)+
  geom_line(data = mob_within, aes(x = month_year, y = mob_per_pop_50, color = "within")) +
  geom_ribbon(data = mob_within, aes(x = month_year,
                                      ymin = mob_per_pop_lower,
                                      ymax = mob_per_pop_upper,
                                      fill = "within"),
              alpha = 0.7, show.legend = FALSE)+
  labs(x = "Date", y = "Number of trips in a week") +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  scale_y_continuous(trans = 'log10') +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 14),        # X axis text size
        axis.text.y = element_text(size = 14)) +
  scale_color_manual(values = c("within" = "#606c38", 
                                "between" = "#bb8588"),
                     labels = c("within" = "Within LTLA", 
                                "between" = "Between LTLAs")) +
  scale_fill_manual(values = c("within" = "#606c38", 
                                "between" = "#bb8588"),
                     labels = c("within" = "Within LTLA", 
                                "between" = "Between LTLAs"))

# Extract the mobility legend
# legends <- cowplot::get_legend(p2)

# Display the mobility plot without the legend
p2_no_legend <- p2 + theme(legend.position = "none")


# IMD map
Eng_map_old <- st_read(dsn = "data/raw/Local_Authority_(Lower_Tier)_IMD_2019_(OSGB1936)-shp",
                       layer = "3393c1ec-7625-46ee-b5a4-370783173f4a202044-1-htedjg.l276p")

ltla_imd <- readRDS("data/processed/covariates/ltla_imd_processed.rds") %>%
  rename(lad19cd = location_fine)

map_and_data <- sp::merge(Eng_map_old, ltla_imd, by = "lad19cd")

# m1 <- tm_shape(map_and_data) +
#   tm_borders(alpha = 0.1) +
#   tm_fill("imd", border.alpha = 0,
#           style = "cont",
#           palette = "YlOrBr") +
#   tm_layout(legend.outside = TRUE, frame = FALSE)

m1 <- tm_shape(map_and_data) +
  tm_borders(fill_alpha = 0.1) +
  tm_fill("imd",
          col_alpha = 0,
          fill.scale = tm_scale_continuous(
            values = "brewer.yl_or_br",    # colour palette
            value.na = "transparent", # NA areas invisible on map
            label.na = NA          # removes "Missing" from legend
          ),
          fill.legend = tm_legend(title = "IMD",
                                  title.size = 1.1,   # increase legend title size
                                  text.size = 0.9,     # increase legend numbers size
                                  frame = FALSE)
  ) +
  tm_layout(legend.outside = TRUE, frame = FALSE, meta.margins=c(0, 0, 0, 0.25))
m1 <- tmap_grob(m1)

# Age proportion map
Eng_map <- st_read(dsn = "data/raw/Local_Authority_Districts_(December_2022)_Boundaries_UK_BFC/",
                   layer = "LAD_DEC_2022_UK_BFC")
Eng_map <- subset(Eng_map, startsWith(LAD22CD, "E"))

pop_agegroups <- read.csv("data/processed/covariates/pop_agegroups64.csv")

a_temp <- pop_agegroups %>%
  rename(LAD22CD = location_fine)
map_and_data_temp <- sp::merge(Eng_map, a_temp, by = "LAD22CD")
# m2 <- tm_shape(map_and_data_temp) +
#   tm_fill("prop", border.alpha = 0,
#           style = "cont", palette = "RdPu") +
#   tm_layout(legend.outside = TRUE, frame = FALSE)

m2 <- tm_shape(map_and_data_temp) +
  tm_fill(
    "prop",
    col_alpha = 0,   # replaces border.alpha
    fill.scale = tm_scale_continuous(
      values = "RdPu",         # palette
      value.na = "transparent",# transparent for missing areas
      label.na = NA             # remove "Missing" from legend
    ),
    fill.legend = tm_legend(title = "Prop > 64 years",
                            title.size = 1.1,   # increase legend title size
                            text.size = 0.9,     # increase legend numbers size
                            frame = FALSE) 
  ) +
  tm_layout(legend.outside = TRUE, frame = FALSE, meta.margins=c(0, 0, 0, 0.25)) +
  tmap_options(component.autoscale = FALSE)
# m2
m2 <- tmap_grob(m2)


# Population density map
pop_density <- read.csv("data/processed/covariates/pop_density.csv")

b_temp <- pop_density %>%
  rename(LAD22CD = location_fine)
map_and_data_temp1 <- sp::merge(Eng_map, b_temp, by = "LAD22CD")
# m3 <- tm_shape(map_and_data_temp1) +
#   tm_fill("pop_density", border.alpha = 0,
#           style = "cont", palette = "-RdYlGn") +
#   tm_layout(legend.outside = TRUE, frame = FALSE)
m3 <- tm_shape(map_and_data_temp1) +
  tm_fill(
    "pop_density",
    col_alpha = 0,   # replaces border.alpha
    fill.scale = tm_scale_continuous(
      values = "-RdYlGn",      # reversed palette
      value.na = "transparent",# NA areas invisible
      label.na = NA,            # removes "Missing" from legend
      limits = c(0, max(map_and_data_temp1[["pop_density"]]))
    ),
    fill.legend = tm_legend(title = "Population density",
                            title.size = 1.1,   # increase legend title size
                            text.size = 0.9,     # increase legend numbers size
                            frame = FALSE,
                            position = tm_pos_out("right", "center")) 
  ) +
  tm_layout(legend.outside = TRUE, frame = FALSE, meta.margins=c(0, 0, 0, 0.25))

# m3
m3 <- tmap_grob(m3)


p_temp1 <- plot_grid(p1, p2_no_legend,
               nrow = 2,
               labels = c("A", "B"),
               align = "v")
p_temp2 <- plot_grid(m1, m2, m3,
                     labels = c("C", "D", "E"),
                     nrow = 1)
p <- plot_grid(p_temp1, p_temp2,
               ncol = 1,
               rel_heights = c(2, 1))
p
ggsave(p, file = "outputs/data_AC.png", width = 15, height = 12)
ggsave(p, file = "outputs/data_AC.pdf", width = 15, height = 12)
