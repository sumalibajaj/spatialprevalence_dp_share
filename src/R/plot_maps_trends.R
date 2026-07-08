# Plot all PPC plots for all months
library(dplyr)
library(cowplot)
library(ggplot2)
library(tmap)
library(svglite)

mmyy_df <- read.csv("data/processed/mmyy_df.csv") %>%
  filter(!(mmyy %in% c("8-2020", "9-2020", "3-2022")))

# final lists with all plots
maps <- vector(mode='list', nrow(mmyy_df))
trends <- vector(mode='list', nrow(mmyy_df))

i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)
  maps[[i]] = tmap_grob(readRDS(paste0("outputs/maps/rdata/cluster_", mmyy_temp, ".rds")))
  trends[[i]] = readRDS(paste0("outputs/trends/rdata/cluster_", mmyy_temp, ".rds")) +
    theme(axis.title.x = element_text(size = 16),       # X axis title size
          axis.title.y = element_text(size = 16),       # Y axis title size
          axis.text.x = element_text(size = 16),        # X axis text size
          axis.text.y = element_text(size = 16),
          strip.text.x = element_text(size = 12.5))
  
  i = i+1
}

# Plot invasion maps and trends together and remaining together
# Add labels to trend plots
trends[[3]]  <- trends[[3]]  #+ ylab("Prevalence")
trends[[10]] <- trends[[10]] #+ ylab("Prevalence")
trends[[15]] <- trends[[15]] #+ 
  # ylab("Prevalence") +
  # xlab("Date")

r1 <- plot_grid(maps[[3]], trends[[3]],
                    maps[[10]], trends[[10]], 
                    maps[[15]], trends[[15]],
                    ncol = 2)
ggsave(r1, file = "outputs/maps_trends_vocs.png", width = 18, height = 15)
ggsave(r1, file = "outputs/maps_trends_vocs.pdf", width = 18, height = 15)
ggsave(r1, file = "outputs/maps_trends_vocs.svg", width = 18, height = 15)


r_m1 <- plot_grid(maps[[1]], maps[[3]], maps[[5]], maps[[7]], maps[[9]],
                ncol = 1)
r_m2 <- plot_grid(maps[[2]], maps[[4]], maps[[6]], maps[[8]],
                  ncol = 1)
r_m3 <- plot_grid(maps[[10]], maps[[12]], maps[[14]], maps[[16]],
                  ncol = 1)
r_m4 <- plot_grid(maps[[11]], maps[[13]], maps[[15]],
                  ncol = 1)
r_t1 <- plot_grid(trends[[1]], trends[[3]], trends[[5]], trends[[7]], trends[[9]],
                  ncol = 1)
r_t2 <- plot_grid(trends[[2]], trends[[4]], trends[[6]], trends[[8]],
                  ncol = 1)
r_t3 <- plot_grid(trends[[10]], trends[[12]], trends[[14]], trends[[16]],
                  ncol = 1)
r_t4 <- plot_grid(trends[[11]], trends[[13]], trends[[15]],
                  ncol = 1)

ggsave(r_m1, file = "outputs/maps_1.pdf", width = 5, height = 15)
ggsave(r_m2, file = "outputs/maps_2.pdf", width = 5, height = 12)
ggsave(r_m3, file = "outputs/maps_3.pdf", width = 5, height = 15)
ggsave(r_m4, file = "outputs/maps_4.pdf", width = 5, height = 12)
ggsave(r_t1, file = "outputs/trends_1.pdf", width = 8, height = 15)
ggsave(r_t2, file = "outputs/trends_2.pdf", width = 8, height = 12)
ggsave(r_t3, file = "outputs/trends_3.pdf", width = 8, height = 15)
ggsave(r_t4, file = "outputs/trends_4.pdf", width = 8, height = 12)

