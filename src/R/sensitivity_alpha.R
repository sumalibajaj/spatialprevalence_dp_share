# To see if different values of alpha have an effect on clustering
# First do clustering for different values of alpha
# Then compare the no. of clusters each month for different values of alpha
source("src/R/create_clustertrends.R")
dat_og <- read.csv("data/raw/ltla_in_region_debiased_prev.csv")

i <- seq(1:length(unique(dat_og$location_fine))) 
df <- as.data.frame(i) %>%
  mutate(alpha_1 = 1/(i-1+1),
         alpha_2 = 2/(i-1+2),
         alpha_5 = 5/(i-1+5))
sum(df$alpha_1)
sum(df$alpha_2)
sum(df$alpha_5)

# To select the same number of initial clusters when running multiple chains
set.seed(12)

# Code to select weeks for which you want to do clustering etc
weeks_monthyear <- tibble(weeks = unique(dat_og$week_date) ) %>%
  mutate(monthyear = format(as.Date(weeks), "%Y-%m"))

# Run clustering and plotting for each month
for(temp_monthyear in unique(weeks_monthyear$monthyear)){
  print(paste0("Month: ", temp_monthyear))
  temp_weeks <- weeks_monthyear %>% 
    filter(monthyear == temp_monthyear) %>%
    select(weeks) %>%
    pull() %>%
    as.Date()
  
  # # Clustering
  if(length(temp_weeks) <= 1)
    print("only one week in this month, so moving on to next month")
  else
    create_clustertrends(dat_og = dat_og, weeks = temp_weeks, n_more_chains = 3,
                         alpha = 1, sigma_mult_factor = 1/100000, maxIters = 10000)
  
}

# Run clustering and plotting for each month
for(temp_monthyear in unique(weeks_monthyear$monthyear)){
  print(paste0("Month: ", temp_monthyear))
  temp_weeks <- weeks_monthyear %>% 
    filter(monthyear == temp_monthyear) %>%
    select(weeks) %>%
    pull() %>%
    as.Date()
  
  # # Clustering
  if(length(temp_weeks) <= 1)
    print("only one week in this month, so moving on to next month")
  else
    create_clustertrends(dat_og = dat_og, weeks = temp_weeks, n_more_chains = 3,
                         alpha = 5, sigma_mult_factor = 1/100000, maxIters = 10000)
  
}

# Comparing clustering
mmyy_df <- read.csv("data/processed/mmyy_df.csv")

# final list with all months and alpha
alpha = 2
results_alpha2 <- vector(mode='list', nrow(mmyy_df))

# Create a data frame with month and number of clusters, alpha
# ALPHA = 2
i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)

  # read output file with number clusters for each iteration in a given month
  df_temp <- readRDS(paste0("data/processed/all_clustertrend_assignment_",
                 mmyy_temp, ".rds"))
  
  # Calculate the maximum (or no. of clusters) of each iteration
  n_clusters <- apply(df_temp %>% select(-location_fine), 2, max)
  
  out_temp <- data.frame(n_clusters) %>% cbind(mmyy_temp, alpha) %>%
    mutate(iter = 1:n())
  
  results_alpha2[[i]] = out_temp
  i = i + 1
}

results_alpha2_df <- do.call("rbind", results_alpha2)


# ALPHA = 5
alpha = 5
results_alpha5 <- vector(mode='list', nrow(mmyy_df))

i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)
  
  # read output file with number clusters for each iteration in a given month
  df_temp <- readRDS(paste0("data/processed/all_clustertrend_assignment_",
                            mmyy_temp,"_alpha5.rds"))
  
  # Calculate the maximum (or no. of clusters) of each iteration
  n_clusters <- apply(df_temp %>% select(-location_fine), 2, max)
  
  out_temp <- data.frame(n_clusters) %>% cbind(mmyy_temp, alpha) %>%
    mutate(iter = 1:n())
  
  results_alpha5[[i]] = out_temp
  i = i + 1
}

results_alpha5_df <- do.call("rbind", results_alpha5)


# ALPHA = 1
alpha = 1
results_alpha1 <- vector(mode='list', nrow(mmyy_df))

i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)
  
  # read output file with number clusters for each iteration in a given month
  df_temp <- readRDS(paste0("data/processed/all_clustertrend_assignment_",
                            mmyy_temp,"_alpha1.rds"))
  
  # Calculate the maximum (or no. of clusters) of each iteration
  n_clusters <- apply(df_temp %>% select(-location_fine), 2, max)
  
  out_temp <- data.frame(n_clusters) %>% cbind(mmyy_temp, alpha) %>%
    mutate(iter = 1:n())
  
  results_alpha1[[i]] = out_temp
  i = i + 1
}

results_alpha1_df <- do.call("rbind", results_alpha1)

# Combine all alpha data
results <- rbind(results_alpha2_df, results_alpha5_df)
results <- rbind(results, results_alpha1_df)
results <- results %>%
  mutate(month_year = as.Date(paste("01", mmyy_temp, sep = "-"), format = "%d-%m-%Y"))

results = results %>% filter(mmyy_temp != "10-2021")

# How many clusters do we get at then end for different values of alpha
results_lastiter <- results %>%
  mutate(max_iter = max(iter),
         alpha = as.factor(alpha)) %>%
  filter(iter == max_iter)

date_breaks <- seq(as.Date("2020-10-01"), as.Date("2022-03-30"), by = "1 month")
date_breaks = date_breaks[-13]

results_lastiter = results_lastiter %>% filter(month_year >= "2020-10-01")


p <- ggplot(data = results_lastiter, 
            aes(x = month_year, y = n_clusters, fill = alpha)) +
  geom_bar(position="dodge", stat="identity", alpha = 0.9) +
  scale_x_date(date_labels = "%b %y", breaks = date_breaks) +
  # scale_x_discrete(labels = format(date_breaks, "%b %y")) +
  scale_y_continuous(breaks = seq(0,14,2)) +
  theme_bw() +
  labs(x = "Date", y = "Number of clusters") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 13),        # X axis text size
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 14)) +
  scale_fill_manual(values = c("1" = "#00a9a5",
                                "2" = "#0b5351",
                                "5" = "#092327"))

p
ggsave(p, file = "outputs/sensitivity/alpha.png", width = 15, height = 7)
ggsave(p, file = "outputs/sensitivity/alpha.pdf", width = 15, height = 7)


