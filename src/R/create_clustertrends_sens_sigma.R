library(dplyr)
library(ggplot2)
library(kernlab)
library(rstan)
library(RColorBrewer)
library(tmap) # to plot the map
library(sf)
library(lubridate)
library(tidyverse)
source("src/R/functions/function_dp_gibbs_multivariate.R")
source("src/R/functions/function_utility.R")
source("src/R/functions/function_summarise_clusterassignment.R")

create_clustertrends_sens_sigma <- function(dat_og, weeks, n_more_chains, 
                                 alpha, sigma_mult_factor, maxIters){
  
  print("Doing inference")
  # ----------------------------------------------------------------------------
  # GETTING PREVALENCE DATA READY FOR CLUSTERING
  # ----------------------------------------------------------------------------
  mmyy <- paste0(month(weeks[1]), "-", year(weeks[1]))
  
  data <- dat_og %>% 
    filter(week_date %in% weeks) %>% 
    dplyr::select(location_fine, week_date, mean_prev)
  
  # Remove LTLAs with missing prevalence information for one or more weeks
  data <- reshape(data, idvar = "location_fine", timevar = "week_date", direction = "wide") %>%
    na.omit(is.na())
  data_location_fine <- data[1]
  # weekly prevalences for this month
  data <- data[,-1] %>% as.matrix()

  # ----------------------------------------------------------------------------
  # PARAMETERS AND INITIAL VALUES FOR DIRICHLET PROCESS GIBBS SAMPLER
  # ----------------------------------------------------------------------------
  # the base distribution here is Normal
  # set the parameter values and initial values to be used in the DP Gibbs sampler
  alpha <- alpha # concentration parameter >0 (smaller = few new clusters)
  mu0 <- matrix(rep(0, length(weeks)), ncol = length(weeks), byrow = TRUE) # mean of base distribution
  sigma0 <- diag(length(weeks)) * 1 # sd of base distribution
  sigma <- diag(length(weeks)) * sigma_mult_factor # variance of likelihood
  maxIters <- maxIters

  test_clusters <- alpha*log(1+(nrow(data)/alpha))

  # ----------------------------------------------------------------------------
  # INFERENCE (RUNNING THE GIBBS SAMPLER)
  # ----------------------------------------------------------------------------
  # Chain 1 - initially all points belong to different clusters
  results_c1 <- dp_gibbs(data = data, alpha = alpha, mu0 = mu0,
                         sigma0 = sigma0, sigma = sigma, c_init = seq(1:nrow(data)),
                         maxIters = maxIters)
  
  # Chain 2 and more - number of clusters fixed and points assigned accordingly
  # final list with all chains including the two above
  results_cn <- vector(mode='list', n_more_chains + 1)
  results_cn <- run_multiple_chains(n_more_chains,
                                    data = data, alpha = alpha, mu0 = mu0,
                                    sigma0 = sigma0, sigma = sigma, c_init = c_init_r,
                                    maxIters = maxIters,
                                    results_list = results_cn)
  
  # add the results from chain 1 towards the end of the list
  results_cn[[length(results_cn)]] <- results_c1
  
  # Calculate how many clusters is each chain create at the last iteration
  mode_nclust_vec <- sapply(results_cn, mode_unique_per_column)
  print(table(mode_nclust_vec))
  
  # Apply spectral clustering across all MCMC chains and get one cluster assignment for LTLAs
  results_final <- consensus_clustering(mmyy, maxIters, results_list = results_cn)

  # Save LTLA and final cluster assignment
  saveRDS(cbind(data_location_fine, results_final),
          file = paste0("data/processed/all_clustertrend_assignment_",
                        mmyy, "_sigma_", sigma_mult_factor, ".rds"))
  # Save all chains to supplementary figure
  saveRDS(results_cn,
          file = paste0("data/processed/all_chains_all_clustertrend_assignment_",
                        mmyy, "_sigma_", sigma_mult_factor, ".rds"))

}


plot_and_save_last_iter_sens_sigma <- function(data, data_location_fine, results_final, mmyy, weeks, sigma){
  
  # Add more colors if >28 clusters are found
  my_color <- c("1" = "#94d2bd",
                "2" = "#ee9b00",
                "3" = "#669bbc",
                "4" = "#4f772d",
                "5" = "#005f73",
                "6" = "#936639",
                "7" = "#007f5f",
                "8" = "#c9184a",
                "9" = "#f05a29",
                "10" = "#72369d",
                "11" = "#f3b816",
                "12" = "#774936",
                "13" = "#80ffdb",
                "14" = "#b5179e",
                "15" = "#ffff3f",
                "16" = "#e5383b",
                "17" = "#ff7b00",
                "18" = "#f1c0e8",
                "19" = "#312244",
                "20" = "#d9ae94",
                "21" = "#ff4800",
                "22" = "#b76935",
                "23" = "#03045e",
                "24" = "#ccd5ae",
                "25" = "#001219",
                "26" = "#99582a",
                "27" = "#3a86ff",
                "28" = "#8338ec")
  
  # Keeping prevalence data and final cluster assignment
  results_plot <- data %>%
    as.data.frame() %>%
    cbind(results_final) %>%
    select(head(names(.), length(weeks)), tail(names(.), 1)) %>% # keep prev and last col
    rename(final_clusters = names(.)[ncol(.)]) %>% # rename last col
    pivot_longer(!final_clusters, names_to = "week_date", values_to = "mean_prev") %>%
    mutate(final_clusters = as.factor(final_clusters)) %>%
    mutate(week_date = sub("^[^.]*\\.", "", week_date))
  
  results_plot_sum <- results_plot %>%
    group_by(week_date, final_clusters) %>%
    summarise(mean_prev = mean(mean_prev))
  
  p_cl_obs <- ggplot(data = results_plot, aes(x = as.Date(week_date), y = mean_prev, group = final_clusters)) +
    geom_jitter(aes(color = final_clusters), 
                show.legend = FALSE, alpha = 0.7, size = 2, stroke = 0) +
    geom_line(data = results_plot_sum, aes(color = final_clusters), show.legend = FALSE) +
    theme_bw() +
    scale_y_continuous(limits = c(0, 0.07)) +
    scale_x_date(date_labels = "%d %b %y", breaks = unique(as.Date(results_plot$week_date))) + 
    # scale_color_brewer(palette = "Set2") +
    scale_color_manual(values = my_color, aesthetics = c("color", "fill")) +
    labs(x = "Week", y = "Prevalence") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  print(p_cl_obs)
  
  # assign final cluster assignments to prevalence data
  dat_map <- cbind(data_location_fine, data) %>%
    cbind(results_final) %>%
    as.data.frame() %>%
    select(head(names(.), length(weeks) + 1), tail(names(.), 1)) %>%
    rename(final_clusters = names(.)[ncol(.)])
  
  # England map
  Eng_map <- st_read(dsn = "data/raw/Local_Authority_Districts_(December_2022)_Boundaries_UK_BFC/",
                     layer = "LAD_DEC_2022_UK_BFC")
  
  Eng_map <- subset(Eng_map, startsWith(LAD22CD, "E"))
  
  # final cluster assignment
  ltla_cluster <- dat_map
  
  a_temp <- ltla_cluster %>%
    rename(LAD22CD = location_fine)
  map_and_data_temp <- sp::merge(Eng_map, a_temp, by = "LAD22CD")
  a_temp_plot <- tm_shape(map_and_data_temp) +
    tm_fill("final_clusters", border.alpha = 0,
            title = mmyy, style = "cat", palette = my_color) +
    tm_layout(legend.outside = TRUE, frame = FALSE)
  a_temp_plot
  
  print(a_temp_plot)
  
  tmap_save(a_temp_plot, paste0("outputs/maps/cluster_", mmyy, "_sigma_", sigma, ".png"))
  # save(a_temp_plot, file = paste0("data/processed/cluster_map_", mmyy,".RData"))
  ggsave(p_cl_obs, file = paste0("outputs/trends/cluster_", mmyy, "_sigma_", sigma, ".png"), width = 5, height = 3)
  
  saveRDS(dat_map, file = paste0("data/processed/final_clustertrend_assignment_",
                                 mmyy, "_sigma_", sigma, ".rds"))
  
  saveRDS(a_temp_plot, file = paste0("outputs/maps/rdata/cluster_",
                                     mmyy, "_sigma_", sigma, ".rds"))
  saveRDS(p_cl_obs, file = paste0("outputs/trends/rdata/cluster_",
                                  mmyy, "_sigma_", sigma, ".rds"))
}

