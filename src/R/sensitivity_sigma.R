library(dplyr)
library(ggplot2)
library(cowplot)
library(tmap)
source("src/R/create_clustertrends_sens_sigma.R")
dat_og <- read.csv("data/raw/ltla_in_region_debiased_prev.csv")

# To select the same number of initial clusters when running multiple chains
set.seed(12)

# Month
monthyear_fixed <- "2021-01"
# Code to select weeks for which you want to do clustering etc
weeks_monthyear <- tibble(weeks = unique(dat_og$week_date) ) %>%
  mutate(monthyear = format(as.Date(weeks), "%Y-%m"))

sigmas <- c(1/100000, 1/10000, 1/1000000)

# Run clustering and plotting for each month
for(sigma_temp in sigmas){
  print(paste0("Month: ", monthyear_fixed))
  temp_weeks <- weeks_monthyear %>% 
    filter(monthyear == monthyear_fixed) %>%
    select(weeks) %>%
    pull() %>%
    as.Date()

  # Clustering
  create_clustertrends_sens_sigma(dat_og = dat_og, weeks = temp_weeks, n_more_chains = 2,
                         alpha = 2, sigma_mult_factor = sigma_temp, maxIters = 10000)
  
  
  # Plotting
  mmyy <- paste0(month(temp_weeks[1]), "-", year(temp_weeks[1]))
  data <- dat_og %>% 
    filter(week_date %in% temp_weeks) %>% 
    dplyr::select(location_fine, week_date, mean_prev)
  
  # Remove LTLAs with missing prevalence information for one or more weeks
  data <- reshape(data, idvar = "location_fine", timevar = "week_date", direction = "wide") %>%
    na.omit(is.na())
  data_location_fine <- data[1]
  # weekly prevalences for this month
  data <- data[,-1] %>% as.matrix()
  
  results_final = readRDS(paste0("data/processed/all_clustertrend_assignment_",
                                 mmyy,"_sigma_", sigma_temp, ".rds"))
  
  print("Making the trend plot and map")
  plot_and_save_last_iter_sens_sigma(data = data, data_location_fine = data_location_fine,
                          results_final = results_final, mmyy = mmyy, weeks = temp_weeks,
                          sigma = sigma_temp)
}

# Make final plot
# final lists with all plots
maps <- vector(mode='list', 3)
trends <- vector(mode='list', 3)

maps[[1]] <- tmap_grob(readRDS("outputs/maps/rdata/cluster_1-2021_sigma_1e-04.rds"))
maps[[2]] <- tmap_grob(readRDS("outputs/maps/rdata/cluster_1-2021.rds"))
maps[[3]] <- tmap_grob(readRDS("outputs/maps/rdata/cluster_1-2021_sigma_1e-06.rds"))

trends[[1]] <- readRDS("outputs/trends/rdata/cluster_1-2021_sigma_1e-04.rds") +  
  theme(axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 16),        # X axis text size
        axis.text.y = element_text(size = 16),
        strip.text.x = element_text(size = 12.5))
trends[[2]] <- readRDS("outputs/trends/rdata/cluster_1-2021.rds") +  
  theme(axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 16),        # X axis text size
        axis.text.y = element_text(size = 16),
        strip.text.x = element_text(size = 12.5))
trends[[3]] <- readRDS("outputs/trends/rdata/cluster_1-2021_sigma_1e-06.rds") +  
  theme(axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 16),        # X axis text size
        axis.text.y = element_text(size = 16),
        strip.text.x = element_text(size = 12.5))

p <- plot_grid(maps[[1]], trends[[1]], maps[[2]], trends[[2]], maps[[3]], trends[[3]],
               ncol = 2)
ggsave(p, file = "outputs/maps_trends_sens_sigma.png", width = 14, height = 15)
ggsave(p, file = "outputs/maps_trends_sens_sigma.pdf", width = 14, height = 15)



