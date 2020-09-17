
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(see)
library(RColorBrewer)


es_all <- read_csv(here('data', "shiny_es.csv"))
df1 <- read_csv(here('data', 'shiny_data.csv'))  %>%
  mutate(sub_id = as.factor(sub_id),
         phase = as.factor(phase),
         new_id = rep(1:6, each = 15))

df2 <- df1 %>%
  mutate(new_id = 0)

df <- bind_rows(df1, df2)

colr <- scales::hue_pal()(6)

t = 0

library(showtext)
font_add_google(name = "Roboto", family = "roboto",
                regular.wt = 300, bold.wt = 500)
showtext_auto()
showtext_opts(dpi = 112)

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh; line-height:120vh; padding:20%"
    
  )
}