
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
library(ggpubr)


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


theme_scrolly <- function(){ 
  list(
    geom_point(size = 4, shape = 16),
    geom_line(size = 1.5),
    geom_vline(aes(xintercept = 5.5), alpha = .5),
    scale_y_continuous(limits = c(0,1), labels = scales::percent),
    scale_x_continuous(labels = seq(1,15,1), breaks = seq(1,15,1)),
    theme_modern(base_size = 12),
    theme(legend.position = 'none',
          panel.background = element_rect(fill = "transparent",colour = NA), 
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.title.x = element_text(family = 'roboto'),
          axis.title.y = element_text(family = 'roboto'),
          strip.text.x = element_text(family = 'roboto', face = "plain")),
      scale_colour_viridis_d(option = "plasma", begin = 0, end = .6),
      annotate(geom = "text", x = 5.4, y = 1, label = "baseline",
               size = 4, family = 'roboto', hjust = 'right', fontface = "italic"),
      annotate(geom = "text", x = 5.6, y = 1, label = "treatment",
               size = 4, family = 'roboto', hjust = 'left', fontface = "italic"),
    ylab(NULL),
    xlab(NULL) 
  )
}

theme_nap <- function() {
  list(
    geom_segment(aes(x = 3.05, y = .1666, xend = 6-.2, yend = .0666), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 7-.2, yend = .1666), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 8-.2, yend = .233), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 9-.2, yend = .166), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 10-.2, yend = .300), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 11-.2, yend = .500), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 12-.2, yend = .6), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 13-.2, yend = .5), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 14-.2, yend = .566), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    geom_segment(aes(x = 3.05, y = .1666, xend = 15-.2, yend = .6), color = 'black',size = .25,
                 arrow = arrow(type = 'closed',length = unit(0.25, "cm"))),
    annotate(geom = "text", x = 3, y = .28, label = TeX("$\\frac{1+.5+.5}{10}$",
                                                        output = 'character'),
             size = 4, family = 'roboto', parse = T),
    annotate(geom = "text", x = 2, y = .23, label = TeX("$\\frac{1}{10}$",
                                                        output = 'character'),
             size = 4, family = 'roboto', parse = T),
    annotate(geom = "text", x = 1, y = .18, label = TeX("$\\frac{1}{10}$",
                                                        output = 'character'),
             size = 4, family = 'roboto', parse = T),
    annotate(geom = "text", x = 3.5, y = 0.03, label = TeX("$\\frac{0}{10}$",
                                                           output = 'character'),
             size = 4, family = 'roboto', parse = T),
    annotate(geom = "text", x = 5.28, y = 0.03, label = TeX("$\\frac{0}{10}$",
                                                            output = 'character'),
             size = 4, family = 'roboto', parse = T),
    geom_label_repel(aes(label = ifelse(sub_id == 15 & phase == 'treatment' & mean_correct > 4/30 &
                                          phase == 'treatment' & mean_correct < 6/30, "T",
                                        ifelse(sub_id == 15 & phase == 'treatment' & mean_correct > 5/30, "N",
                                               ifelse(sub_id == 15 & phase == 'treatment' & mean_correct < 5/30, "O", "")))),
                     color = 'black',
                     size = 4,
                     family = 'roboto',
                     nudge_x = .25,
                     nudge_y = ifelse(df$session == 12, -.2, -.1),
                     xlim = c(6,NA),
                     fill = NA),
    annotate('text', x = 15, y = 1, label = "N = non-overlap\nO = overlap\nT=tie",
             size = 4, family = 'roboto',
             hjust = 'right',
             vjust = 'top')
  )
}


theme_smd2 <- function() {
list(
    annotate(
      geom = "curve", x = 2.5, y = .5, xend = 3, yend = .25, 
      curvature = -.6, arrow = arrow(length = unit(3, "mm"), type = 'closed')
    ),
    annotate(geom = "text", x = 3, y = .55,
             label = "low variability", size = 4, family = 'roboto'),
    annotate(geom = "text", x = 15, y = .85,
             label = "SMD = 16.8", size = 4, family = 'roboto',
             hjust = "right", color = "darkblue"),
    annotate(geom = "text", x = 15, y = .42,
             label = "SMD = 3.2", size = 4, family = 'roboto',
             hjust = "right", color = "darkmagenta")
)
}

theme_smd3 <- function() {
  list(
    annotate(geom = "curve", x = 8, y = .15, xend = 5.25, yend = 0, 
             curvature = -.4,
             arrow = arrow(length = unit(3, "mm"), type = 'closed')),
    annotate(geom = "text", x = 8.5, y = .2,
             label = "no variability, must pool", size = 4, family = 'roboto')
  )
}
