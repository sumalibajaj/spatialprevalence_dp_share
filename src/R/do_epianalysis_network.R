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

library(sna)


do_epianalysis_network = function(mmyy, maxIters){
  
  mmyy_input <- mmyy
  
  # OUTCOME
  # Get prob. of being in the same cluster ready
  prob <- readRDS(paste0("data/processed/prob_clustertrend_assignment_", mmyy, ".rds")) %>%
    mutate(pair_lad = paste(pmin(location_fine1, location_fine2), pmax(location_fine1, location_fine2), sep = "_"))
  
  
  # RUN ONLY FOR MONTHS WHERE WE HAVE ATLEAST THREE CLUSTERS???
  
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
  
  write.csv(pop_agegroups, file = "data/processed/covariates/pop_agegroups64.csv")
  
  
  # Get LTLA population density
  pop_density <- create_pop_density_ltla("data/raw/covariates/ukpopestimatesmid2020on2021geography.xls")
  write.csv(pop_density, file = "data/processed/covariates/pop_density.csv")
  
  
  # Get LTLA IMD ready
  imd <- create_imd_ltla("data/raw/covariates/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx")
  write.csv(imd, file = "data/processed/covariates/imd.csv")
  
  
  # Get mobility within and between LTLAs ready
  # DIRECTLY USING PROCESSED MOBILITY DATA
  mob_within <- readRDS("data/processed/covariates/ltla_monthly_mobility_processed_within.rds") %>% 
    filter(mmyy == mmyy_input) %>%
    mutate(location_fine = sub("\\_.*", "", pair_lad)) %>%
    select(location_fine, mob_per_pop)
  
  # If there is no mobility data available, exit
  if(nrow(mob_within) == 0){
    output_no <- data.frame("estimate" = NA, "pvalue" = NA, "month_year" = mmyy_input, "covariate" = NA)
    
    return(output_no)
  } 
  # If the number of clusters <=3, exit
  clustertrend <- readRDS(paste0("data/processed/all_clustertrend_assignment_", mmyy, ".rds")) %>%
    select(last_col()) %>%
    pull()
  n_clusters <- max(clustertrend)
  
  if(n_clusters <= 3){
    output_no <- data.frame("estimate" = NA, "pvalue" = NA, "month_year" = mmyy_input, "covariate" = NA)
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
    # # mobility within LTLA 1
    # dat <- left_join(prob, mob_within, by = c("location_fine1" = "location_fine")) %>%
    #   mutate(avg_trips_within1 = avg_trips,
    #          avg_trips = standardize(avg_trips)) %>%
    #   rename(mobility_within1 = avg_trips)
    # # mobility within LTLA 2
    # dat <- left_join(dat, mob_within, by = c("location_fine2" = "location_fine")) %>%
    #   mutate(avg_trips_within2 = avg_trips,
    #          avg_trips = standardize(avg_trips)) %>%
    #   rename(mobility_within2 = avg_trips)
    # # mobility between LTLA 1 and 2
    # dat <- left_join(dat, mob_between, by = c("pair_lad" = "pair_lad")) %>%
    #   mutate(avg_trips_between = avg_trips,
    #          avg_trips = standardize(avg_trips)) %>%
    #   rename(mobility_between = avg_trips)
    # mobility within LTLA 1
    dat <- left_join(prob, mob_within, by = c("location_fine1" = "location_fine")) %>%
      rename(mob_per_pop_within1 = mob_per_pop) %>% 
      replace_na(list(mob_per_pop_within1 = 0))
    
    
    # mobility within LTLA 2
    dat <- left_join(dat, mob_within, by = c("location_fine2" = "location_fine")) %>%
      rename(mob_per_pop_within2 = mob_per_pop) %>% 
      replace_na(list(mob_per_pop_within2 = 0))
    
  
    # mobility between LTLA 1 and 2
    dat <- left_join(dat, mob_between, by = c("pair_lad" = "pair_lad")) %>%
      rename(mob_per_pop_between = mob_per_pop) %>% 
      replace_na(list(mob_per_pop_between = 0))
    
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
    
    dat <- dat %>%
      mutate(imd_per10_diff = abs(imd_per10_1 - imd_per10_2),
             pop_density_per1000_diff = abs(pop_density_per1000_1 - pop_density_per1000_2),
             prop_diff = log(abs(prop1 - prop2)),
             mob_per_pop_within_diff = abs(mob_per_pop_within1 - mob_per_pop_within2))
  }
  
  convert_to_matrix = function(df) {
    all_ltlas = sort(unique(c(df[["location_fine1"]], df[["location_fine2"]])))
    n = length(all_ltlas)
    
    y = matrix(NA, n, n, dimnames=list(all_ltlas, all_ltlas))
    
    for (i in 1:nrow(df)) {
      ltla1 = df[i, "location_fine1"]
      ltla2 = df[i, "location_fine2"]
      y[[ltla1, ltla2]] = df[i, "prob"]
      y[[ltla2, ltla1]] = df[i, "prob"]
      
    }
    diag(y) = 1.0
    
    covariates = c("mob_per_pop_within_diff", "mob_per_pop_between", "imd_per10_diff", "pop_density_per1000_diff", "prop_diff", "distance_km_per100", "is_neighbour")
    x = array(NA, dim = c(length(covariates), n, n), dimnames = list(covariates, all_ltlas, all_ltlas))
    for (k in seq_along(covariates)) {
      M = matrix(NA, n, n, dimnames=list(all_ltlas, all_ltlas))
      for (i in 1:nrow(df)) {
       ltla1 = df[i, "location_fine1"]
       ltla2 = df[i, "location_fine2"]
       M[[ltla1, ltla2]] = df[i, covariates[k]]
       M[[ltla2, ltla1]] = df[i, covariates[k]]
      }
      diag(M) = 0.0
      x[k, , ] = M
    }
    list(y = y, x = x)
  }
  
  yx = convert_to_matrix(dat)
  
  nl = netlm(yx$y, yx$x, intercept=TRUE, mode="graph", nullhyp="qap", diag=FALSE, reps=1000, test.statistic="t-value")

  variables = c("(Intercept)", "mob_per_pop_within_diff", "mob_per_pop_between", "imd_per10_diff", "pop_density_per1000_diff", "prop_diff", "distance_km_per100", "is_neighbourTRUE")

  output_nl = summary(nl)$coef %>% 
    cbind(pvalue = nl$pgreqabs) %>% 
    cbind(month_year = mmyy_input) %>% 
    cbind(covariate = variables) %>%
    as.data.frame()
  
  colnames(output_nl) <- c("estimate", "pvalue", "month_year", "covariate")
  
  return(output_nl)
}

