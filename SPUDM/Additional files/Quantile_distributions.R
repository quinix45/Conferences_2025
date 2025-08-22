
library(tidyverse)


theme_set(theme_classic(base_size = 15, 
                        base_family = 'serif'))


# set file location as working directory

dat <- rio::import("dat_long_res.csv")

dat_filt <- dat %>% 
              filter(test_retest == "t1" & item == "G2404") %>% 
                select(accuracy_hist_scaled, Q, J) %>% 
                  mutate(Q = factor(Q, 
                                    levels = c("-2", "-1", "0", "1", "2"),
                                    labels = c("q = 5%", " q = 25%", "q = 50%", "q = 75%", "q = 95%")))


# compute the mode of each density
modes <- dat_filt %>%
  group_by(Q) %>%
  summarise(
    mode = {
      dens <- density(accuracy_hist_scaled, adjust = 1.5)
      dens$x[which.max(dens$y)]
    },
    .groups = "drop"
  )


# adjust modes

modes[1,2] <- modes[1,2] -.028
modes[2,2] <- modes[2,2] -.028
modes[3,2] <- modes[3,2] - .07
modes[4,2] <- modes[4,2] - .06
modes[5,2] <- modes[5,2] - .15

plot <- dat_filt %>%
  ggplot(aes(x = accuracy_hist_scaled)) +
  # map both aesthetics to the same "type"
  geom_vline(aes(xintercept = 0, linetype = "Resolution", color = "Resolution")) +
  geom_vline(data = modes, aes(xintercept = mode, linetype = "Quantile Mode", color = "Quantile Mode")) +
  geom_density(adjust = 1.5, color = "#1b305c") +
  ggh4x::facet_manual(
    ~ Q,
    scales = "free",
    design = "AABBCC
              #DDEE#"
  ) +
  xlim(-3, 3) +
  xlab("Signed Error Distribution for Item G2404 Across all Forecasters") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  scale_color_manual(
    name   = NULL,
    values = c("Resolution" = "red", "Quantile Mode" = "black")
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = c("Resolution" = "dotted", "Quantile Mode" = "dashed")
  ) +
  guides(
    color = guide_legend(override.aes = list(linetype = c("dashed", "dotted")))
  )

plot

ggsave(filename = "item_G2404_plot.png", 
       plot = plot, 
       bg = "transparent",
       width = 9, 
       height = 6, 
       dpi = 300)




