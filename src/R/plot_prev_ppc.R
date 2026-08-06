# Plot all PPC plots for all months
library(dplyr)
library(cowplot)

mmyy_df <- read.csv("data/processed/mmyy_df.csv") %>%
  filter(!(mmyy %in% c("8-2020", "9-2020")))

# final list with all plots
results <- vector(mode='list', nrow(mmyy_df))


# Custom formatting function
custom_format <- function(x) {
  formatted <- ifelse(x %in% c(0, 1), sprintf("%.0f", x), sprintf("%.1f", x))
  return(formatted)
}

i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)
  results[[i]] = readRDS(paste0("outputs/ppc/rdata/ppc_ecdf_", mmyy_temp, ".rds")) +
    labs(y = "", x = "") +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = custom_format) +
    theme(axis.title.x = element_text(size = 16),       # X axis title size
          axis.title.y = element_text(size = 16),       # Y axis title size
          axis.text.x = element_text(size = 16),        # X axis text size
          axis.text.y = element_text(size = 16),
          strip.text.x = element_text(size = 12.5))
  
  i = i+1
}

p <- plot_grid(results[[1]], results[[2]], results[[3]], 
               results[[4]], results[[5]], results[[6]], 
               results[[7]], results[[8]], results[[9]], 
               results[[10]], results[[11]], results[[12]], 
               results[[13]], results[[14]], results[[15]], 
               results[[16]], results[[17]], results[[18]],
               ncol = 3)
# p
ggsave(p, file = "outputs/ppc/ppc_ecdf_overall_AC.png", width = 15, height = 20)
ggsave(p, file = "outputs/ppc/ppc_ecdf_overall_AC.pdf", width = 15, height = 20)



# final list with all plots
results <- vector(mode='list', nrow(mmyy_df))

i=1
for(mmyy_temp in mmyy_df$mmyy){
  print(mmyy_temp)
  results[[i]] = readRDS(paste0("outputs/ppc/rdata/ppc_prev_", mmyy_temp, ".rds")) +
    labs(y = "Prevalence", x = "Week") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) + 
    theme(axis.title.x = element_text(size = 12),       # X axis title size
          axis.title.y = element_text(size = 12),       # Y axis title size
          axis.text.x = element_text(angle = 20, size = 11, hjust = 1),        # X axis text size
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 7))
  i = i+1
}

p <- plot_grid(results[[1]], results[[2]], results[[3]], 
               results[[4]], results[[5]], results[[6]], 
               results[[7]], results[[8]], results[[9]], 
               results[[10]], results[[11]], results[[12]], 
               results[[13]], results[[14]], results[[15]], 
               results[[16]], results[[17]], results[[18]],
               ncol = 3,
               align = 'v')
# p
ggsave(p, file = "outputs/ppc/ppc_prev_overall_AC.png", width = 20, height = 20)
ggsave(p, file = "outputs/ppc/ppc_prev_overall_AC.pdf", width = 20, height = 20)

p1 <- plot_grid(results[[1]], results[[2]], results[[3]], 
               ncol = 3)
p2 <- plot_grid(results[[4]], results[[5]], results[[6]], 
                ncol = 3)
p3 <- plot_grid(results[[7]], results[[8]], results[[9]], 
                ncol = 3)
p4 <- plot_grid(results[[10]], results[[11]], results[[12]], 
                ncol = 3)
p5 <- plot_grid(results[[13]], results[[14]], results[[15]], 
                ncol = 3)
p6 <- plot_grid(results[[16]], results[[17]], results[[18]], 
                ncol = 3)
p6

ggsave(p1, file = "outputs/ppc/ppc_prev_overall_AC_1.pdf", width = 20, height = 3)
ggsave(p2, file = "outputs/ppc/ppc_prev_overall_AC_2.pdf", width = 20, height = 3)
ggsave(p3, file = "outputs/ppc/ppc_prev_overall_AC_3.pdf", width = 20, height = 3)
ggsave(p4, file = "outputs/ppc/ppc_prev_overall_AC_4.pdf", width = 20, height = 3)
ggsave(p5, file = "outputs/ppc/ppc_prev_overall_AC_5.pdf", width = 20, height = 3)
ggsave(p6, file = "outputs/ppc/ppc_prev_overall_AC_6.pdf", width = 20, height = 3)






p <- plot_grid(results[[1]], results[[2]], results[[3]], 
               results[[4]], results[[5]], results[[6]], 
               results[[7]], results[[8]], results[[9]], 
               ncol = 3,
               align = 'v')
# p
ggsave(p, file = "outputs/ppc/ppc_prev_overall_AC_p1.png", width = 20, height = 20)


p <- plot_grid(results[[10]], results[[11]], results[[12]], 
               results[[13]], results[[14]], results[[15]], 
               results[[16]], results[[17]], results[[18]],
               ncol = 3,
               align = 'v')
# p
ggsave(p, file = "outputs/ppc/ppc_prev_overall_AC_p2.png", width = 20, height = 20)
