library(emojifont)   # deze en
library(fontawesome) # deze voor FontAwesome glyphs
library(extrafont)   # Deze voor custom fonts in ggplot, lees de documentatie
library(ggplot2)     # anders kun je niet plotten natuurlijk

my_waffle <- function(x, rows = 5, use_glyph = 'square', glyph_size = 6,
                      title = 'Waffle chart', subtitle = "subtitle") {
  
  len     <- sum(x)
  waffles <- seq(len) - 1
  nms     <- if(is.null(names(x))) seq_along(x) else names(x)
  df      <- data.frame(xvals = waffles %/% rows,
                        yvals = 1 - (waffles %% rows),
                        fill = factor(rep(nms, times = x)))
  
  ggplot(df, aes(xvals, yvals, color = fill)) +
    geom_text(label = fontawesome(paste('fa', use_glyph, sep = '-')), 
              family = 'fontawesome-webfont', size = glyph_size) +
    coord_equal(expand = TRUE) +
    lims(x  = c(min(df$xvals) - 1, max(df$xvals) + 1),
         y  = c(min(df$yvals) - 1, max(df$yvals) + 1)) + 
    theme_void(base_size = 16) +
      scale_color_manual(values =  c("#56C8BB", "#D8F85D"), name = "") +
      coord_fixed() + 
      labs(title = title, subtitle = subtitle) +
    theme(plot.margin      = margin(20, 20, 20, 20),
          legend.position  = "bottom",
          plot.background  = element_rect(fill = "#083F3B", color =  "#083F3B"),
          panel.background = element_rect(fill = "#083F3B", color =  "#083F3B"),
          plot.title       = element_text(family = "NeueMachina", colour = ddy_3, size = 20, hjust = 0.5),
          plot.subtitle    = element_text(family = "NeueMachina", colour = ddy_3, size = 12, hjust = 0.5),
          legend.text      = element_text(family = "BuenosAires", size = 8, colour = ddy_3)) 
} 

my_waffle(x = c(used      = 1, 
                unused    = 49), 
                rows      = 5, 
                use_glyph = "child",
                title     = "iets",
                subtitle  = "nog iets")