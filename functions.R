
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(see)
library(RColorBrewer)

df <- read.csv(here('data', 'session_summary.csv')) %>%
  filter(condition == 1) %>%
  mutate(sub_id = as.factor(sub_id),
         phase = as.factor(ifelse(phase == 0, 'baseline', 'treatment'))
  ) 

# df2 <- df1 %>%
#   mutate(new_id = 0)
# 
# df <- bind_rows(df1, df2)

colr <- scales::hue_pal()(6)

t = 1

library(showtext)
font_add_google(name = "Roboto", family = "roboto",
                regular.wt = 300, bold.wt = 500)
showtext_auto()
showtext_opts(dpi = 112)

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:90vh; line-height:120vh; padding:20%"
    
  )
}