# Run the clustering with outputs:
# cluster trend lines with observed data
# maps of clusters
# prob. of being in the same cluster
source("src/R/create_clustertrends.R")
dat_og <- read.csv("data/raw/ltla_in_region_debiased_prev.csv")

# To select the same number of initial clusters when running multiple chains
set.seed(12)

# Code to select weeks for which you want to do clustering etc
weeks_monthyear <- tibble(weeks = unique(dat_og$week_date) ) %>%
  mutate(monthyear = format(as.Date(weeks), "%Y-%m"))

temp_monthyear <- "2020-08"

# Run clustering and plotting for each month

for(temp_monthyear in unique(weeks_monthyear$monthyear)){
# for(temp_monthyear in c("2021-10")){
  print(paste0("Month: ", temp_monthyear))
  temp_weeks <- weeks_monthyear %>% 
    filter(monthyear == temp_monthyear) %>%
    select(weeks) %>%
    pull() %>%
    as.Date()
  
  # Clustering
  if(length(temp_weeks) <= 1){
    print("only one week in this month, so moving on to next month")
  } else{
    create_clustertrends(dat_og = dat_og, weeks = temp_weeks, n_more_chains = 3,
                         alpha = 2, sigma_mult_factor = 1/100000, maxIters = 10000) 
    
    # For sensitivity of Alpha, uncomment the following lines:
    # create_clustertrends(dat_og = dat_og, weeks = temp_weeks, n_more_chains = 3,
    #                      alpha = 1, sigma_mult_factor = 1/100000, maxIters = 2000) 
    # create_clustertrends(dat_og = dat_og, weeks = temp_weeks, n_more_chains = 3,
    #                      alpha = 5, sigma_mult_factor = 1/100000, maxIters = 2000) 
  }

  
  # Plotting
  if(length(temp_weeks) <= 1){
    print("only one week in this month, so moving on to next month")
  } else{
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
                                   mmyy, ".rds"))

    print("Making the trend plot and map")
    plot_and_save_last_iter(data = data, data_location_fine = data_location_fine,
                            results_final = results_final, mmyy = mmyy, weeks = temp_weeks)
  }
  
}


################ Plotting only, from saved runs ################################

weeks_monthyear <- tibble(weeks = unique(dat_og$week_date) ) %>%
  mutate(monthyear = format(as.Date(weeks), "%Y-%m"))

temp_monthyear <- "2020-08"

for(temp_monthyear in unique(weeks_monthyear$monthyear)){
  print(paste0("Month: ", temp_monthyear))
  temp_weeks <- weeks_monthyear %>% 
    filter(monthyear == temp_monthyear) %>%
    select(weeks) %>%
    pull() %>%
    as.Date()
  
  # Plotting
  if(length(temp_weeks) <= 1){
    print("only one week in this month, so moving on to next month")
  } else{
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
                                   mmyy, ".rds"))
    
    print("Making the trend plot and map")
    plot_and_save_last_iter(data = data, data_location_fine = data_location_fine,
                            results_final = results_final, mmyy = mmyy, weeks = temp_weeks)
  }
  
}