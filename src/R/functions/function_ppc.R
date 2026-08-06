do_ppc <- function(weeks, mmyy, mu0, sigma0, sigma){
  dat_obs_path <- paste0("data/processed/final_clustertrend_assignment_", mmyy, ".rds")
  dat_obs <- readRDS(dat_obs_path) %>%
    # rename(final_clusters = results_final) %>%
    arrange(final_clusters) # all cluster assignments together
  
  # Calculate observed empirical cdf
  # Long version of dat
  dat_obs_long <- dat_obs %>%
    select(head(names(.), length(weeks) + 1), tail(names(.), 1)) %>% # keep prev and last col
    rename(final_clusters = names(.)[ncol(.)]) %>% # rename last col
    pivot_longer(!c(final_clusters, location_fine), names_to = "week_date", values_to = "obs_mean_prev") %>%
    mutate(final_clusters = as.factor(final_clusters))
  
  reps <- 100
  # Matrix storing posterior draws of mean_prev
  dat_sim <- matrix(0, 
                    nrow = length(unique(dat_obs$location_fine))*length(weeks), 
                    ncol = reps) %>% as.data.frame()
  for(rep in 1:reps){
    tau0 <- solve(sigma0)
    tau <- solve(sigma)
    
    # Store all draws of prevalences in a alist corresponding to cluster number
    pred_list <- vector(mode='list', length(unique(dat_obs$final_cluster)))
    nums_list = vector(mode='list', length(pred_list))
    
    for(i in 1:length(unique(dat_obs$final_cluster))){
      temp_cluster = unique(dat_obs$final_cluster)[i]
      # For each cluster, draw prevalences from the poserior distributionof the cluster mean
      dat_obs_temp <- dat_obs %>%
        filter(final_clusters == temp_cluster) %>%
        select(-final_clusters, -location_fine)
      
      n_cl = nrow(dat_obs_temp)
      sum_data = colSums(dat_obs_temp[ , ])
      mu2_temp = solve(n_cl*tau + tau0) %*% (tau0 %*% t(mu0) + tau %*% sum_data)
      sigma2_temp = solve(n_cl*tau + tau0)
        
      # Posterior predictive draws for this rep
      # Initialize an empty matrix to store pred_temp values
      pred_temp_values <- matrix(0, nrow = nrow(dat_obs_temp), ncol = length(weeks))
      for(j in 1:nrow(dat_obs_temp)){
        # draw a mu from posterior of mu
        mu_post_temp = rmvnorm(n = 1, mean = mu2_temp, sigma = sigma2_temp)
        # draw a prev from mu drawn above and likelihood
        pred_temp <- rmvnorm(n = 1, mean = mu_post_temp, sigma = sigma)
        # Store the pred_temp value in the vector
        pred_temp_values[j, ] <- pred_temp
        
      }
      pred_list[[i]] = pred_temp_values
      nums_list[[i]] = n_cl
    }
    
    # data frame with posterior predicted values of prevalences for a given rep
    dat_sim_rep <- do.call("rbind", pred_list) %>% as.data.frame()
    colnames(dat_sim_rep) <- colnames(dat_obs %>% 
                                        select(-location_fine, -final_clusters) %>% 
                                        select(head(names(.), length(weeks)), tail(names(.), 1)))
    dat_sim_rep <- dat_sim_rep %>%
      cbind(final_clusters = dat_obs$final_clusters) %>%
      cbind(location_fine = dat_obs$location_fine)
    
    # long version of dat_sim_rep
    dat_sim_rep_long <- dat_sim_rep %>%
      pivot_longer(!c(final_clusters, location_fine), names_to = "week_date", values_to = "sim_mean_prev") %>%
      mutate(final_clusters = as.factor(final_clusters))
    
    # save each rep of simulated empirical cdf by week and cluster
    dat_sim[ ,rep] = dat_sim_rep_long$sim_mean_prev
    colnames(dat_sim)[rep] <- paste0("sim_mean_prev_rep", rep)
  }
  
  # add columns for location fine, week_date, final_clusters, and observed prevalences
  dat_sim <- dat_sim %>%
    mutate(obs_mean_prev = dat_obs_long$obs_mean_prev,
           location_fine = dat_sim_rep_long$location_fine,
           week_date = dat_sim_rep_long$week_date,
           final_clusters = dat_sim_rep_long$final_clusters)
  
  return_ecdf_values <- function(x, max_min_prev, min_max_prev, n_values){
    ecdf_f <- ecdf(x) # vector specific ecdf
    prev_values <- seq(max_min_prev, min_max_prev, length.out = n_values) # fixed numbers
    out_values <- ecdf_f(prev_values)
    return(out_values)
  }
  
  ecdf_list <- vector(mode = "list", 
                      length = length(unique(dat_sim$final_clusters))*length(unique(dat_sim$week_date)))
  # How many values for calculating ecdf for each simulation and observed mean_prev
  n_values = 50
  cw_tick = 0
  for(c in unique(dat_sim$final_clusters)){
    for(w in unique(dat_sim$week_date)){
      # Matrix for storing ecdf values for all simulation and observed mean_prev
      # last three rows are not prevalence columns
      dat_ecdf_c_w <- matrix(0, 
                             nrow = n_values, 
                             ncol = reps + 1) %>% as.data.frame()
      for(i in 1:(ncol(dat_sim) - 3)){
        dat_sim_c_w <- dat_sim %>%
          filter(final_clusters == c,
                 week_date == w)
        max_prev <- max(apply(dat_sim_c_w[, 1:11], 2, max)) # max of observed or simulated prev
        min_prev <- min(apply(dat_sim_c_w[, 1:11], 2, min)) # min of observed or simulated prev
        # temp_ecdf_c_w <- return_ecdf_values(dat_sim_c_w[, i], 0.005, 0.02, n_values)
        temp_ecdf_c_w <- return_ecdf_values(dat_sim_c_w[, i], min_prev, max_prev, n_values)
        dat_ecdf_c_w[, i] <- temp_ecdf_c_w
      }
      cw_tick = cw_tick + 1
      # Add the final_cluster number and week_date 
      dat_ecdf_c_w <- cbind(dat_ecdf_c_w, week_date = w)
      dat_ecdf_c_w <- cbind(dat_ecdf_c_w, final_clusters = c)
      # Save this dataframe in the list
      ecdf_list[[cw_tick]] <- dat_ecdf_c_w
    }
  }
  
  ecdf_df <- do.call("rbind", ecdf_list)
  # last two cols are already named
  # NOTE - these are NOT prevalences!!
  colnames(ecdf_df)[1:(ncol(ecdf_df) - 2)] <- colnames(dat_sim %>% 
                                                         select(head(names(.), (ncol(ecdf_df) - 2))))
  
  ecdf_df_long <- ecdf_df %>%
    pivot_longer(!c(final_clusters, week_date, obs_mean_prev), names_to = "Rep", values_to = "sim_ecdf") %>%
    mutate(week_date = gsub("mean_prev\\.", "", week_date),
           week_date = as.Date(week_date, format = "%Y-%m-%d"))
  
  # Custom labeller function to format dates in facet wrap
  date_labeller <- function(value) {
    return(format(as.Date(value), "%d %b %y"))
  }
  
  cluster_ids = as.numeric(ecdf_df_long$final_clusters)
  x = list()
  for (i in seq_along(cluster_ids)) {
    x[[i]] = nums_list[[ cluster_ids[[i]] ]]
  }
  
  ecdf_df_long_2 = ecdf_df_long %>% filter(x >= 10)
  
  p_ppc_ecdf <- ggplot(data = ecdf_df_long_2, aes(x = obs_mean_prev, y = sim_ecdf), group = final_clusters) +
    geom_point(aes(color = final_clusters), alpha = 0.3, size = 1) + 
    geom_abline() + 
    facet_wrap(~week_date, nrow = 1, labeller = as_labeller(date_labeller)) +
    theme_bw() +
    labs(x = "Observed empirical CDF",
         y = "Simulated empirical CDF") +
    scale_color_brewer(palette = "Set2") +
    # scale_x_continuous(labels = scales::number_format(accuracy = 0.001))+
    theme(strip.background = element_rect(fill = "white", color = "grey"),
          strip.text = element_text(color = "black"),
          legend.position = "bottom", legend.box = "horizontal")+
    guides(color="none")
  p_ppc_ecdf
  ggsave(p_ppc_ecdf, file = paste0("outputs/ppc/ppc_ecdf_", mmyy,".png"), width = 8, height = 5)
  saveRDS(p_ppc_ecdf, file = paste0("outputs/ppc/rdata/ppc_ecdf_", mmyy,".rds"))
  
  
  # Compare the observed and posterior predicted values of prevalence for 
  # a given month and a given final cluster assignment
  dat_sim_plot <- dat_sim %>%
    select(1:100, final_clusters, week_date, location_fine) %>%
    pivot_longer(cols = 1:100, names_to = "rep", values_to = "sim_mean_prev") %>%
    mutate(week_date = gsub("mean_prev\\.", "", week_date),
           week_date = as.Date(week_date, format = "%Y-%m-%d")) %>%
    group_by(final_clusters, week_date) %>%
    slice_sample(n = 100) %>%
    ungroup()
  
  
  dat_obs_points <- dat_sim %>%
    select(final_clusters, week_date, location_fine, obs_mean_prev) %>%
    mutate(week_date = gsub("mean_prev\\.", "", week_date),
           week_date = as.Date(week_date, format = "%Y-%m-%d"))

  p_ppc_prev <- ggplot() +
    geom_jitter(data = dat_sim_plot,
                aes(x = week_date, y = sim_mean_prev, group = final_clusters),
                show.legend = FALSE, alpha = 0.5, color = "#588157") +
    geom_jitter(data = dat_obs_points,
                aes(x = week_date, y = obs_mean_prev, group = final_clusters),
                show.legend = FALSE, alpha = 0.75, color = "#ffba49") +
    theme_bw() +
    scale_color_brewer(palette = "Set2") +
    facet_wrap(~final_clusters, nrow = 6) +
    labs(x = "", y = "Prevalence") +
    scale_x_date(date_labels = "%d %b %y", breaks = unique(as.Date(dat_sim_plot$week_date))) +
    theme(strip.background = element_rect(fill = "white", color = "grey"),
          strip.text = element_text(color = "black"))

  # dat_sim_plot <- dat_sim %>%
  #   select(1, final_clusters, week_date, location_fine, obs_mean_prev) %>%
  #   rename("sim_mean_prev" = 1) %>%
  #   mutate(week_date = gsub("mean_prev\\.", "", week_date),
  #          week_date = as.Date(week_date, format = "%Y-%m-%d"))
  # 
  # p_ppc_prev <- dat_sim_plot %>%
  #   ggplot(aes(x = week_date, y = obs_mean_prev, group = final_clusters)) +
  #   geom_jitter(show.legend = FALSE, alpha = 0.5, color = "#ffba49") +
  #   geom_jitter(aes(y = sim_mean_prev), show.legend = FALSE, alpha = 0.5, color = "#588157") +
  #   theme_bw() +
  #   scale_color_brewer(palette = "Set2") +
  #   facet_wrap(~final_clusters, nrow = 6) +
  #   labs(x = "", y = "Prevalence") +
  #   scale_x_date(date_labels = "%d %b %y", breaks = unique(as.Date(dat_sim_plot$week_date))) +
  #   # scale_x_date(date_labels = "%d %b %y") +
  #   theme(strip.background = element_rect(fill = "white", color = "grey"),
  #         strip.text = element_text(color = "black"))
  p_ppc_prev
  ggsave(p_ppc_prev, file = paste0("outputs/ppc/ppc_prev_", mmyy,".png"), width = 5, height = 5)
  saveRDS(p_ppc_prev, file = paste0("outputs/ppc/rdata/ppc_prev_", mmyy,".rds"))
}
