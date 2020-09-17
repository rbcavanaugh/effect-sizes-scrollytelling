
library(readr)
library(here)
library(dplyr)
library(ggplot2)

# creates 100vh div
longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh;"
  )
}

months <- c("SMD", "NAP", "Tau-U", "PMG", "GLMM", "BMEM")
months.full <- c("Standardized Mean Difference",
                 "Non Overlap of All Pairs", 
                 "Tau-U",
                 "Proportion of Potential Maximal Gain",
                 "Generalized Linear mixed effects models",
                 "Bayesian mixed effects models")


es_all <- read_csv(here('data', "shiny_es.csv"))
df <- read_csv(here('data', 'shiny_data.csv'))  %>%
  mutate(sub_id = as.factor(sub_id),
         phase = as.factor(phase),
         new_id = rep(1:6, each = 15))

ids = seq(1:6)

####### Build list of data ######


p1 <- df %>%
    filter(new_id == 1) %>%
    ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id), ) +
    geom_point() +
    geom_line() + 
    geom_vline(aes(xintercept = 5.5), alpha = .5) +
    scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    scale_x_continuous(labels = NULL, breaks = NULL) +
    theme_grey(base_size = 12) +
    theme(legend.position = 'none') +
    ylab('Accuracy') +
    xlab(NULL)

p1

p2 <- df %>%
  filter(new_id == 2) %>%
  ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id)) +
  geom_point() +
  geom_line() + 
  geom_vline(aes(xintercept = 5.5), alpha = .5) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  theme_grey(base_size = 12) +
  theme(legend.position = 'none') +
  ylab('Accuracy') +
  xlab(NULL)

p2

p3 <- df %>%
  filter(new_id == 3) %>%
  ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id)) +
  geom_point() +
  geom_line() + 
  geom_vline(aes(xintercept = 5.5), alpha = .5) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  theme_grey(base_size = 12) +
  theme(legend.position = 'none') +
  ylab('Accuracy') +
  xlab(NULL)

p3

p4 <- df %>%
  filter(new_id == 4) %>%
  ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id)) +
  geom_point() +
  geom_line() + 
  geom_vline(aes(xintercept = 5.5), alpha = .5) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  theme_grey(base_size = 12) +
  theme(legend.position = 'none') +
  ylab('Accuracy') +
  xlab(NULL)

p4

p5 <- df %>%
  filter(new_id == 5) %>%
  ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id)) +
  geom_point() +
  geom_line() + 
  geom_vline(aes(xintercept = 5.5), alpha = .5) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  theme_grey(base_size = 12) +
  theme(legend.position = 'none') +
  ylab('Accuracy') +
  xlab(NULL)

p5

p6 <- df %>%
  filter(new_id == 6) %>%
  ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id)) +
  geom_point() +
  geom_line() + 
  geom_vline(aes(xintercept = 5.5), alpha = .5) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  theme_grey(base_size = 12) +
  theme(legend.position = 'none') +
  ylab('Accuracy') +
  xlab(NULL)

p6
  


render_month <- function(wp, cl = "dark"){
  tagList(
    h1(months[wp], class = paste(cl, "big")),
    render_count(wp)
  )
}


render_count <- function(wp, cl = "dark"){
  p(
    months[wp], "Twitter users have tweeted about #tidytuesday.",
    class = cl
  )
}


