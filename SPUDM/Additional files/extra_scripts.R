
library(tidyverse)

######### add back original item label #########

summary <- rio::import("python_scripts/pymc results/summary.csv")


colnames(summary)[1] <- "parameter"

library(stringr)
summary$index <- as.numeric(str_extract(summary$parameter, "(?<=\\[)\\d+(?=\\])"))
summary$parameter <- str_remove(summary$parameter, "\\[.*\\]")
summary$par_type <- ifelse(str_detect(summary$parameter, "theta"), "person", "item")



full_dat <- rio::import("data/dat_long_res.csv")

# distinct() function kinda nice

item_lab <- full_dat %>% 
              select(I_py, 
                     item) %>% 
              distinct() %>% 
              rename(index = I_py)
  


summary_items <- left_join( summary, item_lab) %>% 
                    filter(parameter != "nu_log__") %>% 
                      mutate(item = case_when(par_type == "person" ~ NA,
                                              .default = item))


rio::export(summary_items, "FPT Presentations/IMPS/Additional files/summary.csv")



###### Create plot for responses based on theta values ######

theme_set(theme_classic(base_size = 16, 
                        base_family = 'serif'))


full_dat <- rio::import("data/dat_long_res.csv")


dat_plot <- full_dat %>% 
              filter(test_retest == "t1") %>% 
                select(b, d, theta, accuracy_hist_scaled, item, J, Q) %>% 
                  mutate(resp_exp  = b + Q*exp(d))

rm(full_dat)

thresholds <- seq(range(dat_plot$theta)[1],
                  range(dat_plot$theta)[2])
    

# sample random thetas within threshold

theta_sample <- function(x){
                            y <- c()
                            y[1] <- sample(unique(x)[unique(x) < -2], 1)
                            y[2] <- sample(unique(x)[unique(x) > -1.5 & unique(x) < -.7], 1)
                            y[3] <- sample(unique(x)[unique(x) > .4 & unique(x) < 1], 1)
                            y[4] <- sample(unique(x)[unique(x) > 1.5], 1)
                            return(y)
                            }


set.seed(3724)

theta_values <- theta_sample(dat_plot$theta)
items <- sample(unique(dat_plot$item), 5, replace = FALSE)


theta_labels <- setNames(
  paste0("\u03B8 = ", format(round(theta_values, 2), nsmall = 2)),
  as.character(theta_values)
)


 design <- c(
   "
 AABB
 DDEE
 "
 )
 
 
 long_dat_plot <- dat_plot %>% 
   filter(theta %in% theta_values,
          item %in% items) %>% 
   select(item, resp_exp, accuracy_hist_scaled, theta) %>% 
   reshape2::melt(id.vars = c("theta", "item"), value.name = "value")
 

 # labels for legend
 long_dat_plot$variable <- factor(long_dat_plot$variable, 
                                  labels = c("Item Expected Forecast", "Person Forecast"))
 
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
     color = guide_legend(override.aes = list(linetype = c("dotted", "dashed")))
   )
 
plot

ggsave(filename = "FPT Presentations/IMPS/Additional files/theta_plot.png", 
       plot = plot, 
       bg = "transparent",
       width = 9, 
       height = 6, 
       dpi = 300)
   




