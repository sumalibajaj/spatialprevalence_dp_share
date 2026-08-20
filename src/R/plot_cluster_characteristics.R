library(dplyr)
library(ggplot2)
library(cowplot)

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

################################################################################
# Number of LTLAs in clusters each month
################################################################################
# Number of clusters each month
n_clusters <- read.csv("data/processed/n_clusters.csv")
mmyy_df <- read.csv("data/processed/mmyy_df.csv")# %>% filter(!(mmyy %in% c("3-2022")))

# final list with all months and chains
results_list <- vector(mode='list', nrow(mmyy_df))

i=1
for(temp_mmyy in mmyy_df$mmyy){
  print(paste0("Month: ", temp_mmyy))

  # Extract number of clusters a given month
  n_cl_temp = readRDS(paste0("data/processed/final_clustertrend_assignment_",
                             temp_mmyy, ".rds")) %>%
    group_by(final_clusters) %>%
    summarise(n_ltla = n()) %>%
    cbind("month_year" = temp_mmyy) %>%
    mutate(month_year = as.Date(paste("01", month_year, sep = "-"), format = "%d-%m-%Y"))
  
  results_list[[i]] = n_cl_temp
  
  i = i + 1
}
results_df <- do.call("rbind", results_list) 
# results_df <- results_df %>%
#   group_by(month_year) %>%
#   mutate(n_clusters = max(final_clusters),
#          final_clusters = as.factor(final_clusters))

results_df <- results_df %>%
  group_by(month_year) %>%
  mutate(n_clusters = max(as.numeric(as.character(final_clusters))),
         final_clusters = as.factor(final_clusters)) %>%
arrange(desc(n_ltla), .by_group = TRUE) %>%  
  mutate(order_in_month = as.factor(row_number()))

# Define the breaks manually starting from October 2020
date_breaks <- seq(as.Date("2020-10-01"), as.Date("2022-03-30"), by = "2 months")

ggplot(data = results_df %>% filter(month_year>="2020-10-01")) +
  geom_jitter(aes(x = month_year, y = n_clusters, 
                 color = final_clusters, size = n_ltla),
              height = 0.5, width = 15, alpha = 0.7, show.legend = c(color = FALSE)) +
  scale_color_manual(values = my_color, aesthetics = c("color", "fill")) +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  scale_y_continuous(breaks = seq(min(results_df$n_clusters), max(results_df$n_clusters), by = 2)) +
  labs(x = "Date", y = "Total number of clusters") +
  geom_text(aes(x = month_year, y = n_clusters, label = n_clusters), 
            vjust = -0.5,  # Vertical adjustment
            hjust = 1.1,   # Horizontal adjustment
            size = 4,      # Text size
            color = "black") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 14),        # X axis text size
        axis.text.y = element_text(size = 14))


q1 <- ggplot(data = results_df %>% filter(month_year>="2020-10-01")) +
  geom_bar(aes(x = month_year, y = n_ltla, fill = order_in_month),
           position="stack", stat="identity", show.legend = FALSE) +
  scale_color_manual(values = my_color, aesthetics = c("color", "fill")) +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  labs(x = "Date", y = "Number of LTLAs") +
  geom_text(aes(x = month_year, label = n_clusters), 
            y = max(results_df[["n_ltla"]]),
            hjust = 0.5,   # Horizontal adjustment
            size = 4,      # Text size
            color = "black") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 14),        # X axis text size
        axis.text.y = element_text(size = 14))
q1
################################################################################
# Distance between co-clustered LTLAs
################################################################################
mmyy_df <- read.csv("data/processed/mmyy_df.csv") %>%
  #filter(!(mmyy %in% c("8-2020", "9-2020", "3-2022")))
  filter(!(mmyy %in% c("8-2020", "9-2020")))

co_clustered_pairs_list <- vector(mode='list', nrow(mmyy_df))

i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)
  
  ltla_clusters <- readRDS(paste0("data/processed/final_clustertrend_assignment_",
                                  mmyy_temp, ".rds")) %>% 
    select(location_fine, final_clusters) %>%
    mutate(mmyy = mmyy_temp)
  
  ltla_distances <- readRDS("data/processed/distance_between_ltlas.rds") 
  
  # Merge with cluster information
  merged_df <- ltla_distances %>%
    left_join(ltla_clusters, by = c("location_fine1" = "location_fine")) %>%
    rename(final_clusters1 = final_clusters) %>%
    left_join(ltla_clusters, by = c("location_fine2" = "location_fine")) %>%
    rename(final_clusters2 = final_clusters)
  
  # Filter to keep only co-clustered pairs
  co_clustered_pairs <- merged_df %>%
    filter(final_clusters1 == final_clusters2) %>%
    select(location_fine1, location_fine2, distance_m, final_clusters1, mmyy.x)
  
  
  co_clustered_pairs_list[[i]] = co_clustered_pairs
  i = i + 1
}

co_clustered_pairs_df <- do.call("rbind", co_clustered_pairs_list) %>%
  mutate(month_year = as.Date(paste("01", mmyy.x, sep = "-"), format = "%d-%m-%Y"),
         final_clusters = as.factor(final_clusters1),
         distance_km = distance_m/1000)

# Summary of distances
summary_co_clustered_pairs <- co_clustered_pairs_df %>%
  group_by(month_year, final_clusters1) %>%
  summarise(av_distance_km = mean(distance_km),
            l_distance_km = quantile(distance_km, 0.25),
            u_distance_km = quantile(distance_km, 0.75)) %>%
  mutate(final_clusters = as.factor(final_clusters1))

summary_co_clustered_pairs <- left_join(summary_co_clustered_pairs, results_df,
                                        by = c("month_year", "final_clusters"))

# Custom labeller function to format dates in facet wrap
date_labeller <- function(value) {
  return(format(as.Date(value), "%b %Y"))
}

p <- ggplot(data = co_clustered_pairs_df) +
  geom_jitter(aes(x = final_clusters, y = distance_km, color = final_clusters),
              size = 1, alpha = 0.2, show.legend = FALSE)+
  geom_violin(aes(x = final_clusters, y = distance_km, group = final_clusters1),
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap(~month_year, scales = "free", labeller = as_labeller(date_labeller),
             ncol = 3)+
  scale_color_manual(values = my_color, aesthetics = c("color", "fill")) +
  labs(x = "Cluster number", y = "Distance (km)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", color = "grey"),
        strip.text = element_text(color = "black", size = 16),
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 16),        # X axis text size
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16))

ggsave(p, file = "outputs/cluster_char_dist.png", width = 18, height = 22)
ggsave(p, file = "outputs/cluster_char_dist.pdf", width = 18, height = 22)

q2 <- ggplot(data = summary_co_clustered_pairs) +
  geom_point(aes(x = month_year, y = av_distance_km, color = order_in_month, 
                 size = n_ltla), 
             alpha = 0.7, show.legend = c(size = FALSE, color = FALSE))+
# geom_errorbar(aes(x = month_year, ymin = l_distance_km, ymax = u_distance_km, 
#                   color = final_clusters)) +
  scale_color_manual(values = my_color, aesthetics = c("color", "fill")) +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  labs(x = "Date", y = "Distance (km)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", color = "grey"),
        strip.text = element_text(color = "black", size = 16),
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 16),        # X axis text size
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "bottom")
q2

################################################################################
# Entropy
################################################################################
results_df1 <- results_df %>%
  group_by(month_year) %>%
  mutate(total_ltla = sum(n_ltla)) %>%
  group_by(month_year, final_clusters) %>%
  mutate(p_i = n_ltla/total_ltla) %>%
  ungroup() 

summary_df1 <- results_df1 %>%
  group_by(month_year) %>%
  summarise(entropy = -sum(p_i*log(p_i)))

# Now entropy with population size 
mmyy_df <- read.csv("data/processed/mmyy_df.csv") %>%
  # filter(!(mmyy %in% c("8-2020", "9-2020", "3-2022")))
  filter(!(mmyy %in% c("8-2020", "9-2020")))

summary_df2 <- data.frame(matrix(ncol = 2, nrow = nrow(mmyy_df)))
x <- c("mmyy", "entropy_pop")
colnames(summary_df2) <- x

i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)
  
  ltla_clusters <- readRDS(paste0("data/processed/final_clustertrend_assignment_",
                                  mmyy_temp, ".rds")) %>% 
    select(location_fine, final_clusters) %>%
    mutate(mmyy = mmyy_temp)
  
  ltla_pop <- readRDS("data/processed/covariates/ltla_pop_agegroups_processed.rds") %>%
    group_by(location_fine) %>%
    summarize(pop = sum(pop, na.rm = TRUE))
  
  # Merge with cluster information
  merged_df <- ltla_pop %>%
    left_join(ltla_clusters, by = c("location_fine" = "location_fine")) 
  
  # Calculate entropy for this month
  entropy_temp <- merged_df %>%
    group_by(final_clusters) %>%
    summarise(pop_cluster = sum(pop, na.rm = TRUE)) %>%
    mutate(total_pop = sum(pop_cluster, na.rm = TRUE),
           p_i = pop_cluster/total_pop) %>%
    summarise(entropy_pop = -sum(p_i*log(p_i))) %>%
    pull()
  
  summary_df2[i,1] = mmyy_temp
  summary_df2[i,2] = entropy_temp
  i = i + 1
}  
summary_df2 <- summary_df2 %>%
  mutate(month_year = as.Date(paste("01", mmyy, sep = "-"), format = "%d-%m-%Y"))

summary_entropy <- merge(summary_df1, summary_df2, by = "month_year")

q3 <- ggplot(data = summary_entropy, aes(x = month_year)) +
  geom_line(aes(y = entropy), color ="#145277") +
  geom_line(aes(y = entropy_pop), color ="#f75c03") +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  labs(x = "Date", y = "Entropy") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", color = "grey"),
        strip.text = element_text(color = "black", size = 16),
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 16),        # X axis text size
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "bottom")
q3

q <- plot_grid(q1, q2, q3, labels = c("A", "B", "C"), ncol = 1, align = "v")
q
ggsave(q, file = "outputs/cluster_char_AC.png", width = 10, height = 10)
ggsave(q, file = "outputs/cluster_char_AC.pdf", width = 10, height = 10)


