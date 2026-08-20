library(dplyr)
library(ggplot2)
# library(kernlab)
library(rstan)
library(RColorBrewer)
library(tmap) # to plot the map
library(sf)
library(lubridate)
library(tidyverse)
source("src/R/functions/function_dp_gibbs_multivariate.R")
source("src/R/functions/function_utility.R")
source("src/R/functions/function_create_prob_clustering.R")
source("src/R/functions/function_plotting.R")
source("src/R/functions/function_ppc.R")
source("src/R/functions/function_summarise_clusterassignment.R")

create_clustertrends <- function(dat_og, weeks, n_more_chains, 
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

  # save all clustertrend assignment to calculate probability of being in the same cluster
  if (alpha == 2){
    print("Saving cluster assignments")
    # Save LTLA and final cluster assignment only
    saveRDS(cbind(data_location_fine, results_final),
            file = paste0("data/processed/all_clustertrend_assignment_",
                          mmyy, ".rds"))
    # Save all chains to supplementary figure
    saveRDS(results_cn,
            file = paste0("data/processed/all_chains_all_clustertrend_assignment_",
                          mmyy, ".rds"))
    
    # Save LTLA, prevalences and final cluster assignment
    saveRDS(cbind(data_location_fine, data, results_final),
            file = paste0("data/processed/final_clustertrend_assignment_",
                          mmyy, ".rds"))

    # ----------------------------------------------------------------------------
    # CALCULATE PROB. OF LTLAs BEING IN THE SAME CLUSTER
    # ----------------------------------------------------------------------------
    print("Calculating probability of being in the same cluster")
    create_prob_clustering(mmyy = mmyy, maxIters = maxIters, data_location_fine = data_location_fine)
    
    # ----------------------------------------------------------------------------
    # POSTERIOR PREDICTIVE CHECK USING ONE FINAL CHAIN
    # plot 1 - observed vs simulated prev
    # plot 2 - observed ecdf vs simulated ecdf
    # ----------------------------------------------------------------------------
    print("Doing posterior predictive checks")
    do_ppc(weeks = weeks, mmyy = mmyy, mu0 = mu0, sigma0 = sigma0, sigma = sigma)
    
  } else{ # Only save no. of clusters per iteration for alpha sensitivity
    saveRDS(cbind(data_location_fine, results_final), 
            file = paste0("data/processed/all_clustertrend_assignment_",
                          mmyy, "_alpha", alpha, ".rds"))
  }
}



rerun_ppc <- function(dat_og, weeks, n_more_chains, alpha, sigma_mult_factor, maxIters){
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
  if (alpha == 2){
    do_ppc(weeks = weeks, mmyy = mmyy, mu0 = mu0, sigma0 = sigma0, sigma = sigma)
  }
}
