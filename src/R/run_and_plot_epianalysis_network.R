# Run the monthly epi analysis
# Plot the coefficients overtime
library(lubridate)
library(tidyverse)
library(scales)
library(ggbreak)
library(gamlss)
library(gamlss.dist)
library(pscl)
source("src/R/do_epianalysis_network.R")
dat_og <- read.csv("data/raw/ltla_in_region_debiased_prev.csv")

# Code to select months for which we want to do epi analysis 
weeks_monthyear <- tibble(weeks = unique(dat_og$week_date) ) %>%
  mutate(monthyear = format(as.Date(weeks), "%Y-%m"),
         mmyy = paste0(month(weeks), "-", year(weeks))) 

mmyy_vector <- weeks_monthyear %>%
  distinct(mmyy) %>%
  head(-1) %>% # remove last month (no clustering done)
  tail(-1) # remove first month (no clustering done)
mmyy_vector <- mmyy_vector[1:nrow(mmyy_vector)-1, ] # remove 3-2022

# List to store regression outputs and number of clusters each month for figure 1
results_list <- vector(mode='list')
n_clusters_list <- vector(mode = 'list')
i=1
for(temp_mmyy in mmyy_vector$mmyy){
  print(paste0("Month: ", temp_mmyy))
  
  # Run for a given month
  if (temp_mmyy == "10-2021") {
    maxIters = 100000
  } else {
    maxIters = 10000
  }
  results_list[[i]] <- do_epianalysis_network(mmyy = temp_mmyy, maxIters = maxIters) 
  
  # Extract number of clusters a given month
  n_cl_temp = readRDS(paste0("data/processed/final_clustertrend_assignment_",
                             temp_mmyy, ".rds")) %>%
    dplyr::pull(final_clusters) %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  n_cl_temp_row <- data.frame("month_year" = temp_mmyy, "n_clusters" = n_cl_temp)
  
  n_clusters_list[[i]] = n_cl_temp_row
  
  i = i + 1
}

results_df <- do.call("rbind", results_list) %>%
  mutate(estimate = as.numeric(estimate),
         pvalue = as.numeric(pvalue),
         month_year = as.Date(paste("01", month_year, sep = "-"), format = "%d-%m-%Y")) %>%
  na.omit()

# Divide log prop diff by 10, so that the coefficient corresponds to a ten percent increase
results_df <- results_df %>%
  mutate(estimate = ifelse(covariate == "prop_diff", estimate/10, estimate))

# Save the number of clusters each month
n_clusters_df <- do.call("rbind", n_clusters_list) %>%
  mutate(month_year = as.Date(paste("01", month_year, sep = "-"), format = "%d-%m-%Y"))
write.csv(n_clusters_df, file = "data/processed/n_clusters.csv")


# Plot the results
# date_breaks <- seq(as.Date("2020-10-01"), as.Date("2022-02-28"), by = "2 months")
date_breaks <- c(as.Date("2020-10-01"), as.Date("2020-11-01"), as.Date("2020-12-01"),
                 as.Date("2021-01-01"), as.Date("2021-12-01"),
                 as.Date("2022-01-01"), as.Date("2022-02-01"))

# Changing label of variables for facet plot
results_df$covariate <- factor(results_df$covariate, 
                               labels = c("Intercept",
                                          "Absolute difference in monthly within-LTLA mobility per capita (monthly trips
per capita)", 
                                          "Average mobility per capita between the LTLAs (monthly trips per capita)",
                                          "Absolute difference in IMD levels (scaled by 10)",
                                          "Absolute difference in population density (1000 people per square kilometer)",
                                          "Logged absolute difference in the proportion of people over 64 years (10% change)",
                                          "Distance between the centroids of the LTLAs (100 km)",
                                          "Shared boundary indicator (1:Yes)"),
                               levels = c("(Intercept)",
                                          "mob_per_pop_within_diff", 
                                          "mob_per_pop_between",
                                          "imd_per10_diff",
                                          "pop_density_per1000_diff",
                                          "prop_diff",
                                          "distance_km_per100",
                                          "is_neighbourTRUE"))
# Convert month_year to a factor with levels corresponding to your breaks
results_df$month_year <- factor(results_df$month_year, levels = date_breaks)

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(data = results_df %>%
               filter(!(covariate %in% "Intercept")), 
             aes(x = month_year, y = estimate, group = month_year, color = covariate), 
             alpha = 0.7,
             size = 3.5) +
  geom_text(data = results_df %>%
              filter(!(covariate %in% "Intercept")), 
            aes(x = month_year, y = estimate, label = ifelse(pvalue < 0.05, "*", "")),
            nudge_x = 0.18,
            size = 12,
            vjust = 0.5) +
  scale_x_discrete(labels = format(date_breaks, "%b %y")) +  # Use labels to format dates
  # scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) + 
  labs(x = "Date", y = "Estimate") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 14),        # X axis text size
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text.x = element_text(size = 13),
        strip.background = element_rect(fill="#f5ebe0")) +
  scale_color_manual(values = c("Absolute difference in monthly within-LTLA mobility per capita (monthly trips
per capita)" = "#028090", 
                                "Average mobility per capita between the LTLAs (monthly trips per capita)" = "#0d3b66",
                                "Absolute difference in IMD levels (scaled by 10)" = "#dc0073",
                                "Absolute difference in population density (1000 people per square kilometer)" = "black",
                                "Difference between proportion of people older than 64 years (10% change)" = "#d1ac00",
                                "Distance between the centroids of the LTLAs (100 km)" = "#d45113",
                                "Shared boundary indicator (1:Yes)" = "#813405")) +
  guides(color = guide_legend(nrow = 4)) +
  labs(x = "Date", y = "Estimated increase in probability of co-clustering of two LTLAs") +
  facet_wrap(~covariate, scales = "free", ncol = 2, labeller = label_wrap_gen(width = 60))
p



# Position of the break on the discrete axis
break_pos <- which(levels(results_df$month_year) == "2021-01-01") + 0.5
p <- p +
  annotate("text",
           x = break_pos,
           y = -Inf,
           label = "//",
           vjust = 0.4,
           size = 6,
           colour = "black") +
  coord_cartesian(clip = "off")


break_pos <- which(levels(results_df$month_year) == "2021-10-01") + 0.5
p <- p +
  annotate("text",
           x = break_pos,
           y = -Inf,
           label = "//",
           vjust = 0.4,
           size = 6,
           colour = "black") +
  coord_cartesian(clip = "off")




p
ggsave(p, file = "outputs/epi/epi_facet_logpropdiff.png", width = 13, height = 10)
ggsave(p, file = "outputs/epi/epi_facet_logpropdiff.pdf", width = 13, height = 10)

