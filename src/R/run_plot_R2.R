# Run the monthly epi analysis
# Plot the coefficients overtime
library(lubridate)
library(tidyverse)
library(scales)
library(ggbreak)
dat_og <- read.csv("data/raw/ltla_in_region_debiased_prev.csv")

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)
library(tidyverse)
source("src/R/functions/function_create_pop_ltla_agegroups.R")
source("src/R/functions/function_create_pop_density_ltla.R")
source("src/R/functions/function_create_imd_ltla.R")
source("src/R/functions/function_utility.R")

# Monthly regression between prob. of being in the same cluster and covariates
do_epianalysis_R2 <- function(mmyy, maxIters){
  
  mmyy_input <- mmyy
  
  # OUTCOME
  # Get prob. of being in the same cluster ready
  prob <- readRDS(paste0("data/processed/prob_clustertrend_assignment_", mmyy, ".rds")) %>%
    mutate(pair_lad = paste(pmin(location_fine1, location_fine2), pmax(location_fine1, location_fine2), sep = "_"))
  
  # RUN ONLY FOR MONTHS WHERE WE HAVE ATLEAST THREE CLUSTERS
  
  # COVARIATES
  # Get LTLA age group specific population size ready
  # Keep proportion of individuals aged 0-64 in each LTLA
  pop_agegroups <- create_pop_ltla_agegroups("data/raw/covariates/ukpopestimatesmid2020on2021geography.xls") %>%
    mutate(age_group_new = ifelse(age_group %in% c("65-74", "75+"), "65+", "0-64")) %>%
    group_by(location_fine, age_group_new) %>%
    summarise(pop = sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(location_fine) %>%
    mutate(pop_ltla = sum(pop),
           prop = pop/pop_ltla) %>%
    ungroup() %>%
    filter(age_group_new == "65+") %>%
    select(location_fine, prop)

  
  # Get LTLA population density
  pop_density <- create_pop_density_ltla("data/raw/covariates/ukpopestimatesmid2020on2021geography.xls")
  
  # Get LTLA IMD ready
  imd <- create_imd_ltla("data/raw/covariates/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx")
  
  # Get mobility within and between LTLAs ready
  # DIRECTLY USING PROCESSED MOBILITY DATA
  # create_monthly_mobility("data/raw/covariates/mobility_lad_weekly.csv")
  mob_within <- readRDS("data/processed/covariates/ltla_monthly_mobility_processed_within.rds") %>% 
    filter(mmyy == mmyy_input) %>%
    mutate(location_fine = sub("\\_.*", "", pair_lad)) %>%
    select(location_fine, mob_per_pop)
  
  # If there is no mobility data available, exit
  if(nrow(mob_within) == 0){
    output_no <- data.frame("R2_adj" = NA, "R2" = NA, "month_year" = mmyy_input)
    return(output_no)
  } 
  # If the number of clusters <=3, exit
  clustertrend <- readRDS(paste0("data/processed/all_clustertrend_assignment_", mmyy, ".rds")) %>%
    select(last_col()) %>%
    pull()
  n_clusters <- max(clustertrend)
  
  if(n_clusters <= 3){
    output_no <- data.frame("R2_adj" = NA, "R2" = NA, "month_year" = mmyy_input)
    return(output_no)
  }else{
    mob_between <- readRDS("data/processed/covariates/ltla_monthly_mobility_processed_between.rds") %>%
      filter(mmyy == mmyy_input)%>%
      select(pair_lad, mob_per_pop)
    
    # Get distance between LTLAs
    # DIRECTLY USING PROCESSED DISTANCE DATA from function_create_distance_between_ltlas_and_neighbours.R
    dist <- readRDS("data/processed/distance_between_ltlas.rds") 
    
    # Get if LTLAs are neighbours
    # DIRECTLY USING PROCESSED NEIGHBOUR DATA from function_create_distance_between_ltlas_and_neighbours.R
    is_neighbour <- readRDS("data/processed/is_neighbours_ltlas.rds")
    
    # Merge all data for the regression
    # mobility within LTLA 1
    dat <- left_join(prob, mob_within, by = c("location_fine1" = "location_fine")) %>%
      rename(mob_per_pop_within1 = mob_per_pop)
    # mobility within LTLA 2
    dat <- left_join(dat, mob_within, by = c("location_fine2" = "location_fine")) %>%
      rename(mob_per_pop_within2 = mob_per_pop)
    # mobility between LTLA 1 and 2
    dat <- left_join(dat, mob_between, by = c("pair_lad" = "pair_lad")) %>%
      rename(mob_per_pop_between = mob_per_pop)
    
    # IMD of LTLA 1 
    dat <- left_join(dat, imd, by = c("location_fine1" = "location_fine")) %>%
      mutate(imd_per10_1 = imd/10) %>%
      select(-imd)
    # IMD of LTLA 2
    dat <- left_join(dat, imd, by = c("location_fine2" = "location_fine")) %>%
      mutate(imd_per10_2 = imd/10) %>%
      select(-imd)
    
    # Pop density of LTLA 1 
    dat <- left_join(dat, pop_density, by = c("location_fine1" = "location_fine")) %>%
      mutate(pop_density_per1000_1 = pop_density/1000) %>%
      select(-pop_density)
    # Pop density of LTLA 2
    dat <- left_join(dat, pop_density, by = c("location_fine2" = "location_fine")) %>%
      mutate(pop_density_per1000_2 = pop_density/1000) %>%
      select(-pop_density)
    
    # Pop proportion aged 0-64 in LTLA 1
    dat <- left_join(dat, pop_agegroups, by = c("location_fine1" = "location_fine")) %>%
      rename(prop1 = prop)
    # Pop proportion aged 0-64 in LTLA 2
    dat <- left_join(dat, pop_agegroups, by = c("location_fine2" = "location_fine")) %>%
      rename(prop2 = prop) 
    
    # Distance between LTLAs
    dat <- merge(dat, dist, by = c("location_fine1" = "location_fine1", 
                                   "location_fine2" = "location_fine2")) %>%
      mutate(distance_km = distance_m/1000,
             distance_km_per100 = distance_km/100) %>%
      select(-distance_km, -distance_m)
    # Are LTLAs neighbours
    dat <- merge(dat, is_neighbour, by = c("location_fine1" = "location_fine1", 
                                           "location_fine2" = "location_fine2"))
    
    # Remove NA's and make probabilities -> counts
    dat <- dat %>%
      na.omit() %>%
      mutate(y = prob*(maxIters/2),
             n = (maxIters/2))
    
    # Create  new variables of difference between pairs for IMD, pop density and prop
    # logging prop difference
    dat <- dat %>%
      mutate(imd_per10_diff = abs(imd_per10_1 - imd_per10_2),
             pop_density_per1000_diff = abs(pop_density_per1000_1 - pop_density_per1000_2),
             # prop_diff = log(abs(prop1 - prop2)),
             prop_diff = abs(prop1 - prop2),
             mob_per_pop_within_diff = abs(mob_per_pop_within1 - mob_per_pop_within2))
    
    # Linear
    m2 <- lm(prob ~ mob_per_pop_within_diff + 
               mob_per_pop_between + 
               imd_per10_diff +
               pop_density_per1000_diff +
               prop_diff +
               distance_km_per100 +
               is_neighbour, data = dat)  
    summary(m2)$adj.r.squared
    summary(m2)$r.squared
    
    output <- cbind(summary(m2)$adj.r.squared, summary(m2)$r.squared, mmyy_input) %>%
      as.data.frame()
    
    colnames(output) <- c("R2_adj", "R2", "month_year")
    
    return(output)
  }
}


# Code to select months for which we want to do epi analysis 
weeks_monthyear <- tibble(weeks = unique(dat_og$week_date) ) %>%
  mutate(monthyear = format(as.Date(weeks), "%Y-%m"),
         mmyy = paste0(month(weeks), "-", year(weeks))) 

mmyy_vector <- weeks_monthyear %>%
  distinct(mmyy) %>%
  head(-1) %>% # remove last month (no clustering done)
  tail(-1) # remove first month (no clustering done)

# List to store regression outputs and number of clusters each month for figure 1
results_list <- vector(mode='list')
i=1
for(temp_mmyy in mmyy_vector$mmyy){
  print(paste0("Month: ", temp_mmyy))
  
  # Run for a given month
  results_list[[i]] <- do_epianalysis_R2(mmyy = temp_mmyy, maxIters = 1000) 
  i = i + 1
}

results_df <- do.call("rbind", results_list) %>%
  mutate(R2_adj = as.numeric(R2_adj),
         R2 = as.numeric(R2),
         month_year = as.Date(paste("01", month_year, sep = "-"), format = "%d-%m-%Y")) %>%
  na.omit()


date_breaks <- c(as.Date("2020-10-01"), as.Date("2020-11-01"), as.Date("2020-12-01"),
                 as.Date("2021-01-01"), as.Date("2021-12-01"),
                 as.Date("2022-01-01"), as.Date("2022-02-01"))

# Convert month_year to a factor with levels corresponding to your breaks
results_df$month_year <- factor(results_df$month_year, levels = date_breaks)

p <- ggplot(data = results_df, aes(x = month_year, y = R2_adj, group = 1)) +
  geom_line()+
  scale_x_discrete(labels = format(date_breaks, "%b %y")) +  # Use labels to format dates
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Date", y = "Adjusted R squared") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),       # X axis title size
        axis.title.y = element_text(size = 16),       # Y axis title size
        axis.text.x = element_text(size = 16),        # X axis text size
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_blank())
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
p
ggsave(p, file = "outputs/epi/epiR2.png", width = 8, height = 6)
ggsave(p, file = "outputs/epi/epi_R2.pdf", width = 8, height = 6)
