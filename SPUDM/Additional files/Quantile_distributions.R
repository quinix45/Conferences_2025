
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


plot <- dat_filt %>%
ggplot(aes(x = accuracy_hist_scaled)) +
geom_vline(xintercept = 0, lty = 2)+
  ggh4x::facet_manual(~ Q, 
                      scales = "free", 
                      design = "AABBCC
                                  #DDEE#") +
  geom_density(adjust = 1.5, color = "#1b305c") +
  xlim(-3, 3) +
  xlab("Singed Error Distribution for Item G2404 Across all Forecasters") +
  theme(legend.position="none")+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank()
  )

ggsave(filename = "item_G2404_plot.png", 
       plot = plot, 
       bg = "transparent",
       width = 9, 
       height = 6, 
       dpi = 300)


