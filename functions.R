


library(showtext)
library(here)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

df99 <- read_csv(here('data', 'boot99.csv')) %>%
  mutate_at(2:4, .funs = logit2prob) %>%
  mutate(session = seq(1,15,1),
         sub_id = as.factor(99)) %>%
  select(-X1)

df84 <- read_csv(here('data', 'preds84.csv')) %>%
  mutate_at(2:4, .funs = logit2prob) %>%
  mutate(session = seq(1,15,1),
         sub_id = as.factor(84)) %>%
  select(-X1)

dfpreds <- bind_rows(df99, df84)

df <- read.csv(here('data', 'session_summary.csv')) %>%
  filter(condition == 1) %>%
  mutate(sub_id = as.factor(sub_id),
         phase = as.factor(ifelse(phase == 0, 'baseline', 'treatment'))) %>%
  left_join(dfpreds, by = c('session', 'sub_id'))

colr <- scales::hue_pal()(6)

t = 1

font_add_google(name = "Roboto", family = "roboto",
                regular.wt = 400, bold.wt = 500)
showtext_auto()
showtext_opts(dpi = 112)

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height = 80vh; width:90%; margin: auto; padding: 10%;" #padding-top:10%; padding-left:10%, padding-right:10%;"
    
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
    theme_modern(base_size = 16),
    theme(legend.position = 'none',
          panel.background = element_rect(fill = "transparent",colour = NA), 
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.title.x = element_text(family = 'roboto', size = 14),
          axis.title.y = element_text(family = 'roboto', size = 14),
          strip.text.x = element_text(family = 'roboto', size = 14)),
      scale_colour_viridis_d(option = "plasma", begin = 0, end = .6),
      annotate(geom = "text", x = 5.4, y = 1, label = "baseline",
               size = 5, family = 'roboto', hjust = 'right', fontface = "italic"),
      annotate(geom = "text", x = 5.6, y = 1, label = "treatment",
               size = 5, family = 'roboto', hjust = 'left', fontface = "italic"),
    ylab(NULL),
    xlab(NULL) 
  )
}


theme_smd1 <- function() {
  list(
    geom_hline(aes(yintercept = 0.21)),
    geom_hline(aes(yintercept = 0.53)),
    annotate(geom = "text", x = 1.5, y = 0.00,
             label = TeX("$\\bar{x}_{baseline} = \\frac{6.4}{30}$", output = 'character'),
             size = 6, family = 'roboto', hjust = "left",
             vjust = 'bottom', color = 'black', parse = T),
    annotate(geom = "text", x = 10, y = 0.75,
             label = TeX("$\\bar{x}_{treatment} = \\frac{16}{30}$", output = 'character'),
             size = 6, family = 'roboto', hjust = "left",
             vjust = 'bottom', color = 'black', parse = T),
    annotate(geom = "curve", x = 8, y = .10, xend = 4.6, yend = 0.18, 
             curvature = -.3,
             arrow = arrow(length = unit(3, "mm"), type = 'closed')),
    annotate(geom = "text", x = 8.5, y = .1,
             label = TeX("$\\sigma_{baseline} = 2.4$", output = 'character'), size = 6, family = 'roboto',
             hjust = "left", parse = T)
  )
}


theme_smd2 <- function() {
  list(
    annotate(
      geom = "curve", x = 2.5, y = .5, xend = 3, yend = .25, 
      curvature = -.6, arrow = arrow(length = unit(3, "mm"), type = 'closed')
    ),
    annotate(geom = "text", x = 1, y = .55,
             label = "low variability (sd = 0.5)", size = 6, family = 'roboto', hjust = "left"),
    annotate(geom = "text", x = 15, y = .85,
             label = "SMD = 21", size = 6, family = 'roboto',
             hjust = "right", color = "darkblue"),
    annotate(geom = "text", x = 15, y = .42,
             label = "SMD = 4.0", size = 6, family = 'roboto',
             hjust = "right", color = "darkmagenta")
  )
}

theme_smd3 <- function() {
  list(
    annotate(
      geom = "curve", x = 2.5, y = .5, xend = 3, yend = .25, 
      curvature = -.6, arrow = arrow(length = unit(3, "mm"), type = 'closed')),
    annotate(geom = "text", x = 1, y = .55,
             label = "low variability (sd = 0.5)", size = 6, family = 'roboto', hjust = "left"),
    annotate(geom = "text", x = 15, y = .85,
             label = "SMD pooled = 2.8", size = 6, family = 'roboto',
             hjust = "right", color = "darkblue"),
    annotate(geom = "text", x = 15, y = .42,
             label = "SMD pooled = 3.12", size = 6, family = 'roboto',
             hjust = "right", color = "darkmagenta")
  )
}

theme_smd4 <- function() {
  list(
    annotate(
      geom = "curve", x = 2.5, y = .5, xend = 3, yend = .25, 
      curvature = -.6, arrow = arrow(length = unit(3, "mm"), type = 'closed')
    ),
    annotate(geom = "text", x = 1, y = .55,
             label = "low variability (sd = 0.5)", size = 6, family = 'roboto', hjust = "left"),
    annotate(geom = "text", x = 15, y = .85,
             label = TeX("$\\SMD_{BR} = 29.9$", output = 'character'), size = 6, family = 'roboto',
             hjust = "right", color = "darkblue", parse = T),
    annotate(geom = "text", x = 15, y = .42,
             label = TeX("$\\SMD_{BR} = 4.6", output = 'character'), size = 6, family = 'roboto',
             hjust = "right", color = "darkmagenta", parse = T)
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
    annotate(geom = "text", x = 3, y = .28, label = TeX("$\\frac{1+ .5+ .5}{10}$",
                                                        output = 'character'),
             size = 6, family = 'roboto', parse = T),
    annotate(geom = "text", x = 2, y = .23, label = TeX("$\\frac{1}{10}$",
                                                        output = 'character'),
             size = 6, family = 'roboto', parse = T),
    annotate(geom = "text", x = 1, y = .18, label = TeX("$\\frac{1}{10}$",
                                                        output = 'character'),
             size = 6, family = 'roboto', parse = T),
    annotate(geom = "text", x = 3.5, y = 0.03, label = TeX("$\\frac{0}{10}$",
                                                           output = 'character'),
             size = 6, family = 'roboto', parse = T),
    annotate(geom = "text", x = 5.28, y = 0.03, label = TeX("$\\frac{0}{10}$",
                                                            output = 'character'),
             size = 6, family = 'roboto', parse = T),
    geom_label_repel(
      aes(label = ifelse(sub_id == 15 && phase == 'treatment' && mean_correct > 4/30 &&
                                          phase == 'treatment' && mean_correct < 6/30, "T",
                                        ifelse(sub_id == 15 && phase == 'treatment' && mean_correct > 5/30, "N",
                                               ifelse(sub_id == 15 && phase == 'treatment' && mean_correct < 5/30, "O", "")))),
                     color = 'black',
                     size = 6,
                     family = 'roboto',
                     nudge_x = .25,
                     nudge_y = ifelse(df$session == 12, -.2, -.1),
                     xlim = c(6,NA),
                     fill = NA),
    annotate('text', x = 15, y = 1, label = "N = non-overlap\nO = overlap\nT=tie",
             size = 6, family = 'roboto',
             hjust = 'right',
             vjust = 'top')
  )
}

theme_nap2 <- function(){
  list(
    annotate(geom = "text", x = 12, y = .85,
           label = "NAP = 1", size = 6, family = 'roboto', hjust = "left",
           color = "violetred3"),
    annotate(geom = "text", x = 12, y = .5,
             label = "NAP = 1", size = 6, family = 'roboto', hjust = "left",
             color = "purple4"),
    annotate(geom = "text", x = 12, y = .30,
             label = "NAP = 1", size = 6, family = 'roboto', hjust = "left",
             color = "brown3")
  )
}

theme_tau1 <- function(){
  list(
    annotate(geom = "text", x = 11, y = .9,
             label = "NAP = 1\nTau-U = 0.80", size = 6, family = 'roboto', hjust = "left",
             color = "violetred3")
  )
}

theme_tau2 <- function(){
  list(
    annotate(geom = "text", x = 14, y = .85,
             label = "Tau-U = 0.8", size = 6, family = 'roboto', hjust = "right",
             color = "violetred3"),
    annotate(geom = "text", x = 7, y = .2,
             label = "Tau-U = 1.06", size = 6, family = 'roboto', hjust = "left", vjust = 'top,',
             color = "darkmagenta"),
    annotate(geom = "text", x = 12, y = .50,
             label = "Tau-U = 0.94", size = 6, family = 'roboto', hjust = "left", vjust = 'top',
             color = "violetred4")
  )
}


theme_pmg1 <- function(){
  list(
    geom_hline(aes(yintercept = 0.15)),
    annotate("segment", x = 8, xend = 15, y = 1, yend = 1, color = 'black'),
    annotate("segment", x = 15, y = 0.15, xend = 15, yend = 1,
           color = 'darkblue', arrow = arrow(ends = 'both', type = 'closed',length = unit(0.25, "cm"))),
    annotate("segment", x = 14.5, y = 0.15, xend = 14.5, yend = 0.8,
           color = 'darkred', arrow = arrow(ends = 'both', type = 'closed',length = unit(0.25, "cm"))),
    annotate(geom = "text", x = 14.75, y = 0.97,
             label = "potential maximal gain", size = 6, family = 'roboto', hjust = "right", vjust = 'top', color = 'darkblue'),
    annotate(geom = "text", x = 14.25, y = .81,
             label = "actual gain", size = 6, family = 'roboto', hjust = "right", vjust = 'top', color = 'darkred'),
    annotate(geom = "text", x = 12, y = .05,
             label = "PMG = 0.76", size = 6, family = 'roboto', hjust = "left", vjust = 'bottom', color = 'black')
  )
}

theme_pmg2 <- function(){
  list(
    annotate(geom = "text", x = 15, y = 0.5,
             label = "PMG = 0.76", size = 6, family = 'roboto', hjust = "right", vjust = 'bottom', color = 'purple4'),
    annotate(geom = "text", x = 10, y = .95,
             label = "PMG = 1.0", size = 6, family = 'roboto', hjust = "left", vjust = 'top', color = 'darkslateblue')
  )
}



theme_glmm1 <- function() {
  list(
      geom_ribbon(data = df %>% filter(sub_id == 99),
                  aes(x = session, ymin = lci, ymax = uci),  alpha = .2, linetype = 0,), 
      geom_line(data = df %>% filter(sub_id == 99),
                aes(y = preds, x = session),size = .75)
  )
}

theme_glmm2 <- function() {
  list(
    geom_ribbon(data = df %>% filter(sub_id == 84 | sub_id == 99),
                aes(x = session, ymin = lci, ymax = uci),  alpha = .1, linetype = 0,), #
      geom_line(data = df %>% filter(sub_id == 84 | sub_id == 99),
                aes(y = preds, x = session), size = .75),
      annotate(geom = "text", x = 6, y = .85,
               label = "GLMM = 1.36x/session", size = 6, family = 'roboto', hjust = "left",
               color = "violetred3"),
      annotate(geom = "text", x = 9, y = .35,
               label = "GLMM = 1.41x/session", size = 6, family = 'roboto', hjust = "left",
               color = "orangered3")
  )
}

theme_glmm3 <- function() {
  list(
    geom_ribbon(data = df %>% filter(sub_id == 84 | sub_id == 99),
                aes(x = session, ymin = lci, ymax = uci),  alpha = .1, linetype = 0,), #
    geom_line(data = df %>% filter(sub_id == 84 | sub_id == 99),
              aes(y = preds, x = session), size = .75),
    annotate(geom = "text", x = 6, y = .85,
             label = "GLMM = 0.89x/session", size = 6, family = 'roboto', hjust = "left",
             color = "violetred3"),
    annotate(geom = "text", x = 9, y = .35,
             label = "GLMM = 1.36x/session", size = 6, family = 'roboto', hjust = "left",
             color = "orangered3")
  )
}

theme_bmem1 <- function(){
  list(
    geom_smooth(data = df %>% filter(sub_id == 12), method = "lm", se = FALSE, formula = y~x),
      annotate(geom = 'segment', x = 5, xend = 6.02, y = .23, yend = .39, size = .75, color = 'darkblue',
               linetype = 'dashed'),
      annotate(geom = "text", x = 1, y = .15,
               label = TeX('$\\beta_1: BaselineSlope$', output = 'character'),
               size = 6, family = 'roboto', hjust = "left",
               color = "darkblue", parse = T,fontface = "bold"),
      annotate("segment", x = 7.5, y = 0.2, xend = 5.7, yend = .32,
               color = 'darkblue',
               arrow = arrow(ends = 'last', type = 'closed',length = unit(0.25, "cm"))), 
      annotate(geom = "text", x = 7.7, y = .2,
               label = TeX('$\\beta_2: LevelChange$', output = 'character'),
               size = 6, family = 'roboto', hjust = "left",
               color = "darkblue", parse = T, fontface = "bold"),
      annotate("segment", x = 6, y = 0.39, xend = 14.5, yend = .42,
               color = 'darkblue'),
      annotate("segment", x = 11.5, y = 0.64, xend = 11.5, yend = .42,
               color = 'darkblue',
               arrow = arrow(ends = 'both', type = 'closed',length = unit(0.25, "cm"))),
      annotate(geom = "text", x = 11, y = .37,
               label = TeX('$\\beta_3: SlopeChange$', output = 'character'),
               size = 6, family = 'roboto', hjust = "left",
               color = "darkblue", parse = T, fontface = "bold"),
      annotate("segment", x = 5, y = 0.255, xend = 5, yend = .81,
               color = 'darkred', arrow = arrow(ends = 'both', type = 'closed',length = unit(0.25, "cm"))),
      annotate(geom = "text", x = 1, y = .85,
               label = "difference between\nposterior samples:\n+16 words",
               size = 6, family = 'roboto', hjust = "left",
               color = "darkred"),
      annotate(geom = 'segment', x = 5, xend = 15, y = .82, yend = .82, size = .75, color = 'darkblue',
               linetype = 'dotted')
  )
}

theme_bmem2 <- function(){
  list(
      annotate(geom = "text", x = 6, y = .85,
               label = "BMEM = +13.3 words", size = 6, family = 'roboto', hjust = "left",
               color = "violetred3"),
      annotate(geom = "text", x = 9, y = .35,
               label = "BMEM: +19.5 words", size = 6, family = 'roboto', hjust = "left",
               color = "orangered3")
  )
}

