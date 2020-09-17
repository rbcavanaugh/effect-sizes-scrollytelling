library(readr)
library(here)

es_all <- read_csv(here('data', "effect_sizes_30_8-31.csv"))

df <- read.csv(here('data', 'data_sim_scd_8-11_unif_30.csv')) %>%
  select(sub_id, item_id_crossed, period, phase,
         slope_change, condition, gamma, alpha, p, response) %>%
  rename(session = period)