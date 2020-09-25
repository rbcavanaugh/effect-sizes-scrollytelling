
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(see)
library(RColorBrewer)
library(ggrepel)
library(latex2exp)
library(grid)
library(gganimate)
library(gifski)


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
    style = "width = 100%; opacity:.8; background:#B0BEC5;height = 100vh" #padding-top:10%; padding-left:10%, padding-right:10%;"
    
  )
}

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

empty <- ""

