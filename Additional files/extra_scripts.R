
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
 
 
 
 # source("http://www.openintro.org/stat/data/arbuthnot.R")
 # library(ggplot2)
 # library(reshape2)
 # 
 # names(arbuthnot) <- c("Year", "Men", "Women")
 # 
 # arbuthnot.melt <- melt(arbuthnot, id.vars = 'Year', variable.name = 'Sex', 
 #                        value.name = 'Rate')
 # 
 
 
 
 long_dat_plot <- dat_plot %>% 
   filter(theta %in% theta_values,
          item %in% items) %>% 
   select(item, resp_exp, accuracy_hist_scaled, theta) %>% 
   reshape2::melt(id.vars = c("theta", "item"), value.name = "value")
 

 # labels for legend
 long_dat_plot$variable <- factor(long_dat_plot$variable, 
                                  labels = c("Item Expected Forecast", "Person Forecast"))
 
 plot <- long_dat_plot %>% 
   ggplot(aes(x = value, y = item, shape = variable, color = variable)) +
   geom_point(size = 2.5) +
   ggh4x::facet_manual(
     ~ theta,
     design = design,
     labeller = labeller(theta = theta_labels)
   ) +
   xlim(-9, 9) +
   xlab("") +
 theme(legend.position="bottom",
       legend.title=element_blank()) +
   scale_color_manual(values = c(1,"red")) +
   scale_shape_manual(values = c(19, 17)) 
 
plot

ggsave(filename = "FPT Presentations/IMPS/Additional files/theta_plot.png", 
       plot = plot, 
       bg = "transparent",
       width = 9, 
       height = 6, 
       dpi = 300)
   




