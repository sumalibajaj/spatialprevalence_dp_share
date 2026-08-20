plot_and_save_last_iter <- function(data, data_location_fine, results_final, mmyy, weeks){
  
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
                "11" = "#f3b816")
  
  # To make sure light blue color corresponds to biggest cluster always in the plots..
  # Reorder cluster labels by cluster size
  cluster_sizes <- sort(table(results_final$results_final), decreasing = TRUE)
  
  # Create mapping:
  # largest cluster -> "1"
  # second largest -> "2"
  # etc.
  cluster_map <- setNames(
    as.character(seq_along(cluster_sizes)),
    names(cluster_sizes)
  )
  
  # Apply remapping
  results_final$results_final <- factor(
    cluster_map[as.character(results_final$results_final)],
    levels = as.character(seq_along(cluster_sizes))
  )
  
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
                show.legend = FALSE, alpha = 0.7, size = 2, stroke = 0, width = 2.0, height=0) +
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
  map_and_data_temp$final_clusters = factor(map_and_data_temp$final_clusters)
  
  levels_from_df = levels(map_and_data_temp$final_clusters)
  
  a_temp_plot <- tm_shape(map_and_data_temp) +
    tm_fill("final_clusters", border.alpha = 0,
            title = mmyy, style = "cat",
            palette = my_color[1:length(unique(map_and_data_temp$final_clusters))],
            fill.legend = tm_legend_hide(),
            fill.scale = tm_scale_categorical(values=my_color[1:length(unique(map_and_data_temp$final_clusters))])) +
    tm_layout(legend.outside = TRUE, frame = FALSE)
    tm_add_legend(type="fill", labels=levels_from_df, fill=my_color[1:length(unique(map_and_data_temp$final_clusters))], title="Cluster")
  a_temp_plot

  print(a_temp_plot)

  tmap_save(a_temp_plot, paste0("outputs/maps/cluster_", mmyy,".png"))
  # save(a_temp_plot, file = paste0("data/processed/cluster_map_", mmyy,".RData"))
  ggsave(p_cl_obs, file = paste0("outputs/trends/cluster_", mmyy,".png"), width = 5, height = 3)
  
  saveRDS(dat_map, file = paste0("data/processed/final_clustertrend_assignment_",
                                 mmyy, ".rds"))
  
  saveRDS(a_temp_plot, file = paste0("outputs/maps/rdata/cluster_",
                                 mmyy, ".rds"))
  saveRDS(p_cl_obs, file = paste0("outputs/trends/rdata/cluster_",
                                     mmyy, ".rds"))
}
