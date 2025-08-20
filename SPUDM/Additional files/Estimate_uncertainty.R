library(tidyverse)

dat_sum <- rio::import("summary.csv") %>% 
  filter(parameter == "theta")

dat_full <- rio::import("dat_long_res.csv")


dat_OUS <- dat_full %>% 
  filter(test_retest == "t1") %>% 
  group_by(J) %>% 
  summarise(StA_out = mean(sscore_standardized),
            sd = sd(sscore_standardized),
            theta_INS = mean(theta_INS)) %>% 
  ungroup() %>% 
  mutate(parameter = "S_Score")

dat_plot <- rbind(dat_OUS[,c("parameter", "sd")],
                  dat_sum[,c("parameter", "sd")])



theme_set(theme_classic(base_size = 16, 
                        base_family = 'serif'))

dat_plot$parameter <- factor(dat_plot$parameter, 
                             levels = c("theta", "S_Score"),
                             labels = c("\u03B8 SDs", "Standardized S-Scores SDs"))


SD_hist_freeX <- ggplot(dat_plot, aes(x = sd))+
  geom_histogram(color = "black",
                 linewidth = .7,
                 fill = "#1b305c") +
  xlab("Standard Deviations of Score Estimate Across 1194 Forecasters") +
   facet_wrap(~parameter, scale = "free_x") + 
  theme_classic(base_size = 16,
                base_family = 'serif')



SD_hist_fixX <- ggplot(dat_plot, aes(x = sd))+
  geom_histogram(color = "black",
                 linewidth = .7,
                 fill = "#1b305c") +
  xlab("Standard Deviations of Score Estimate Across 1194 Forecasters") +
  facet_wrap(~parameter) + 
  theme_classic(base_size = 16,
                base_family = 'serif')

ggsave(filename = "SD_hist_freeX.png", 
       plot = SD_hist_freeX, 
       bg = "transparent",
       width = 9, 
       height = 6, 
       dpi = 300)

ggsave(filename = "SD_hist_fixX.png", 
       plot = SD_hist_fixX, 
       bg = "transparent",
       width = 9, 
       height = 6, 
       dpi = 300)
