


##########T dist ###########



Model_plot <- function(b = 0, d = 2, sd = 1, df = 8, theta = 0, a = 0, title = ""){
  
  require(tidyverse)
  require(extraDistr)
  
  
  Q <- c(-2, -1, 0, 1, 2)
  
  
  means <- c(b + Q[1]*d,
             b + Q[2]*d,
             b + Q[3]*d,
             b + Q[4]*d,
             b + Q[5]*d) 
  
  p <- ggplot() +
    geom_segment(aes(x = means[1], xend = means[1], y = 0, yend = dlst(means[1], mu = means[1], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[1], y = dlst(means[1], mu = means[1], sigma = sd/exp(theta), df =df) +.05, label="5%") +
    geom_segment(aes(x = means[2], xend = means[2], y = 0, yend = dlst(means[2], mu = means[2], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[2], y = dlst(means[2], mu = means[2], sigma = sd/exp(theta), df =df) +.05, label="25%") +
    geom_segment(aes(x = means[3], xend = means[3], y = 0, yend = dlst(means[3], mu = means[3], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[3], y = dlst(means[3], mu = means[3], sigma = sd/exp(theta), df =df) +.05, label="50%") +
    geom_segment(aes(x = means[4], xend = means[4], y = 0, yend = dlst(means[4], mu = means[4], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[4], y = dlst(means[4], mu = means[4], sigma = sd/exp(theta), df =df) +.05, label="75%") +
    geom_segment(aes(x = means[5], xend = means[5], y = 0, yend = dlst(means[5], mu = means[5], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[5], y = dlst(means[5], mu = means[5], sigma = sd/exp(theta), df =df) +.05, label="95%") +
    xlab("Expected Item Accuracy Distribution") +
    ylab("") +
    ggtitle(title) +
    xlim(-7, 7) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,dlst(means[3], mu = means[3], sigma = sd/exp(theta), df = df) +.1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  print(p)
}

# Model_plot(b = -2, title = "Hard to Predict Item")
# 
# 
# Model_plot(b = -2, title = "Easy to Predict Item")



Model_plot_full <- function(b = 0, d = 2, sd = 1, df = 8, theta = 0, a = 0, title = ""){
  
  require(tidyverse)
  require(extraDistr)
  
  Q <- c(-2, -1, 0, 1, 2)
  
  
  means <- c(b + Q[1]*d,
             b + Q[2]*d,
             b + Q[3]*d,
             b + Q[4]*d,
             b + Q[5]*d) 
  
  p <- ggplot() +
    geom_function(fun = dlst, args = list(mu = means[1], sigma = sd/exp(theta), df = df), color = "blue") +
    geom_function(fun = dlst, args = list(mu = means[2], sigma = sd/exp(theta), df = df), color = "blue") +
    geom_function(fun = dlst, args = list(mu = means[3], sigma = sd/exp(theta), df = df), color = "blue") +
    geom_function(fun = dlst, args = list(mu = means[4], sigma = sd/exp(theta), df = df), color = "blue") +
    geom_function(fun = dlst, args = list(mu = means[5], sigma = sd/exp(theta), df = df), color = "blue") +
    geom_segment(aes(x = means[1], xend = means[1], y = 0, yend = dlst(means[1], mu = means[1], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[1], y = dlst(means[1], mu = means[1], sigma = sd/exp(theta), df =df) +.05, label="5%") +
    geom_segment(aes(x = means[2], xend = means[2], y = 0, yend = dlst(means[2], mu = means[2], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[2], y = dlst(means[2], mu = means[2], sigma = sd/exp(theta), df =df) +.05, label="25%") +
    geom_segment(aes(x = means[3], xend = means[3], y = 0, yend = dlst(means[3], mu = means[3], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[3], y = dlst(means[3], mu = means[3], sigma = sd/exp(theta), df =df) +.05, label="50%") +
    geom_segment(aes(x = means[4], xend = means[4], y = 0, yend = dlst(means[4], mu = means[4], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[4], y = dlst(means[4], mu = means[4], sigma = sd/exp(theta), df =df) +.05, label="75%") +
    geom_segment(aes(x = means[5], xend = means[5], y = 0, yend = dlst(means[5], mu = means[5], sigma = sd/exp(theta), df =df)), linetype = 2) +
    annotate("text", x= means[5], y = dlst(means[5], mu = means[5], sigma = sd/exp(theta), df =df) +.05, label="95%") +
    xlab("Model Implied Accuarcy Distributions at 5 Quantiles") +
    xlim(-7, 7) +
    ggtitle(title) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,dlst(means[3], mu = means[3], sigma = sd/exp(theta), df = df) +.1))+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  print(p)
}


# Model_plot_full()
