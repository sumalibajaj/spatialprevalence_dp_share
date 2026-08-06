# To see if all chains for all months converge when alpha = 2 (default case) ONLY
library(dplyr)
# Comparing clustering
mmyy_df <- read.csv("data/processed/mmyy_df.csv")

# final list with all months and chains
results <- vector(mode='list', nrow(mmyy_df))
n_chains = 4

for(i in seq_along(mmyy_df$mmyy)){
  mmyy_temp <- mmyy_df$mmyy[i]
  print(mmyy_temp)
  
  # read output file with number clusters for each iteration in a given month
  df_temp <- readRDS(paste0("data/processed/all_chains_all_clustertrend_assignment_",
                            mmyy_temp, ".rds"))
  length(df_temp)
  
  # mmyy_temp = "3-2021"
  # df_temp <- `all_chains_all_clustertrend_assignment_3-2021`
  
  # Store results from all chain in a month specific list
  chains_clusters_list <- vector(mode='list', n_chains)
  j = 1
  # Calculate the maximum (or no. of clusters) of each iteration and each chain
  for(chain in 1:n_chains){
    # Calculate the maximum (or no. of clusters) of each iteration
    n_clusters <- apply(df_temp[[chain]], 2, max)
    n_clusters <- data.frame(n_clusters) %>% 
      cbind(chain_number = chain, mmyy = mmyy_temp) %>%
      mutate(iter = 1:n())
    
    chains_clusters_list[[j]] = n_clusters
    j = j + 1
  }
  
  # Save month specific in the main list
  chains_clusters_df <- do.call("rbind", chains_clusters_list)
  
  results[[i]] = chains_clusters_df
}


date_labeller <- function(value) {
  return(format(as.Date(value), "%b %Y"))
}

results_df <- do.call("rbind", results) %>%
  mutate(month_year = as.Date(paste("01", mmyy, sep = "-"), format = "%d-%m-%Y"),
         chain_number = as.factor(chain_number)) %>%
  filter(month_year >= "2020-10-01",
         month_year <= "2022-03-01")
  
# Plot convergence for all months
p <- ggplot(data = results_df %>% filter(n_clusters < 30) %>%
              # filter(iter > max(iter)/2),
              filter(iter > 0),
# p <- ggplot(data = results_df,          
            aes(x = iter, y = n_clusters, group = chain_number)) +
  geom_line(aes(color = chain_number)) +
  scale_y_continuous(trans = 'log10') +
  facet_wrap(~month_year, scales = "free", ncol = 4, labeller = as_labeller(date_labeller)) +
  theme_bw() +  
  labs(x = "Iteration", y = "Number of clusters", color = "Chains") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 13),        # X axis text size
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 14),
        strip.text.x = element_text(size = 13),
        strip.background = element_rect(fill="#f5ebe0"),
        plot.margin = margin(r=25)) +
  scale_color_manual(values = c("1" = "#274c77",
                               "2" = "#6096ba",
                               "3" = "#a3cef1",
                               "4" = "#a9a29c",
                               "5" = "#cdb4db"))
p
ggsave(p, file = "outputs/sensitivity/chain_convergence.png", width = 12, height = 10)
