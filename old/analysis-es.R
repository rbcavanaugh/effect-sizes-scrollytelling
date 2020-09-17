#############################################################################
# Effect Size Analysis
#############################################################################

library(GGally)
library(RColorBrewer)
library(tidyr)
library(cccrm)


my_custom_cor_color <- function(data, mapping, color = I("black"), sizeRange = c(1, 5), method="k", ...) {
  
  # get the x and y data to use the other code
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  xy <- tibble(
    x = x,
    y = y
  ) %>%
    pivot_longer(cols = 1:2, names_to = 'rater', values_to = 'rating')
  
  ct <- cccUst(xy, "rating", "rater") # Calculates concordance via U statistics
  
  r <- unname(ct)
  rt <- round(r, digits=2)[1]
  tt <- as.character(rt)
  
  # plot the cor value
  p <- ggally_text(
    label = tt, 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = 8,
    color=color,
    ...
  ) +
    
    theme(
      panel.background=element_rect(fill="white"),
      #panel.background = element_rect(color = "black", linetype = "dashed"),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank()
    ) 
  
  bluecols <- brewer.pal(7, 'Reds')
  colFn <- colorRampPalette(bluecols) #, interpolate ='spline')
  fill <- colFn(100)[findInterval(rt, seq(0, 1, length=100))]
  
  
  p <- p + theme(
    panel.background = element_rect(fill= fill)
  )
  
  p
}

my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(..., alpha = 0.7, fill = 'grey') 
}

my_scat <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_point(..., alpha = .5) +
    geom_abline(size = 1, alpha = .5) +
    #geom_smooth(method = "rlm", color = "red", se = F) +
    xlim(-4.25,4.25) +
    ylim(-4.25, 4.25)
}


  
  
