
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(see)
library(RColorBrewer)


es_all <- read_csv(here('data', "shiny_es.csv"))
df <- read_csv(here('data', 'shiny_data.csv'))  %>%
  mutate(sub_id = as.factor(sub_id),
         phase = as.factor(phase),
         new_id = rep(1:6, each = 15))

colr <- brewer.pal(6, "Set2")