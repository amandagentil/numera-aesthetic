# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

numera_colors <- c(
  `cinza_escuro`  = "#4D4D4D",
  `cinza_claro`   = "#D9D9D9",
  `cinza_intermediario` = "grey",
  `azul_numera`   = "#5288DB")

#' Function to extract drsimonj colors as hex codes
#'
#' @param ... Character names of drsimonj_colors
#'
numera_cols <- function(...) {
  cols <- c(...) 

  if (is.null(cols))
    return (drsimonj_colors)

  numera_colors[cols]
}



numera_palettes <- list(
  `main`  = numera_cols("azul_numera", "cinza_escuro"),

  `light`  = numera_cols("azul_numera", "cinza_intermediario"),

  `mixed` = numera_cols("azul_numera", "cinza_escuro", "cinza_intermediario", "cinza_claro")
)



#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
numera_pal <- function(palette = palette, reverse = FALSE, ...) {
  pal <- numera_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}



#' Color scale constructor for numera colors
#'
#' @param palette Character name of palette in numera_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_numera <- function(palette = palette, discrete = TRUE, reverse = FALSE, ...) {
  pal <- numera_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("numera_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for numera colors
#'
#' @param palette Character name of palette in numera_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_numera <- function(palette = palette, discrete = TRUE, reverse = FALSE, ...) {
  pal <- numera_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("numera_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



#' Função para a criação de gráficos estilo "lollipop"

lollipop_graph <- function(data, x, y1, y2, legenda1, legenda2) { 
 
  
 lp <- ggplot(data = data) +
    geom_segment(aes_(x=as.name(x), xend= as.name(x), y= as.name(y1), yend= as.name(y2)), color="darkgrey", linetype = "dashed", size = 0.8) +
    geom_point(aes_(x= as.name(x), y= as.name(y2), colour =  legenda2), size=8) +
   geom_text(aes_(label=  as.name(y2), x =  as.name(x), y =  as.name(y2)), colour = "white", size = 4) + 
   geom_point(aes_(x= as.name(x), y= as.name(y1), colour =  legenda1), size=8) +
   geom_text(aes_(label=  as.name(y1), x =  as.name(x), y =  as.name(y1)), colour = "white", size = 4) +
   scale_colour_manual(values = c("#5288DB", "#4d4d4d")) + 
   coord_flip() +
   xlab("")+
   ylab("") + 
   theme(axis.text=element_text(size=20),
         panel.background = element_rect(fill = "white"),
         panel.grid.major.y = element_line(colour = "lightgrey"),
         legend.title = element_blank(),
         legend.text = element_text(size = 15, colour="#4D4D4D"),
         axis.text.x = element_blank(),
         axis.ticks = element_blank(),
         strip.text = element_text(size=14, colour = "white"),
         strip.background = element_rect(fill="grey"))
   
  
  return(lp)
}
 
#' Função para a criação de gráficos estilo "lollipop" facetado


fct_lollipop_graph <- function(data, x, y1, y2, legenda1, legenda2, fct) { 
  
  
  
  
  lp <- ggplot(data = data) +
    geom_segment(aes_(x=as.name(x), xend= as.name(x), y= as.name(y1), yend= as.name(y2)), color="darkgrey", linetype = "dashed", size = 0.8) +
    geom_point(aes_(x= as.name(x), y= as.name(y2), colour =  legenda2), size=8) +
    geom_text(aes_(label=  as.name(y2), x =  as.name(x), y =  as.name(y2)), colour = "white", size = 4) + 
    geom_point(aes_(x= as.name(x), y= as.name(y1), colour =  legenda1), size=8) +
    geom_text(aes_(label=  as.name(y1), x =  as.name(x), y =  as.name(y1)), colour = "white", size = 4) +
    scale_colour_manual(values = c("#5288DB", "#4d4d4d")) + 
    coord_flip() + 
    facet_wrap(.~data[[fct]]) + 
    #ylim(80,98) +
    xlab("") +
    ylab("") +
    coord_flip() + 
    theme(axis.text=element_text(size=20),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "lightgrey"),
          legend.title = element_blank(),
          legend.text = element_text(size = 15, colour="#4D4D4D"),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(colour = "#4d4d4d", fill = NA),
          strip.text = element_text(size=14, colour = "white"),
          strip.background = element_rect(fill="grey"))
  
  
  return(lp)
}


numera_theme <- function() {
  
  require(ggplot2)
  require(ggthemes)
  require(extrafont)
  
 
  
 p <-  theme_grey(grid_horizontal = TRUE, grid_vertical = TRUE, graph_contour = TRUE) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  title = element_text(family = "Roboto Light", colour = "#4D4D4D"),
                  axis.text = element_text(family = "Roboto Light", colour = "#4D4D4D", size = 8),
                  axis.text.x = element_text(vjust = 2),
                  legend.text = element_text(family = "Roboto Light", colour = "#4D4D4D"),
                  axis.ticks = element_line(colour = "#8F8F8F"),
                  panel.border = element_blank(),
                  axis.line = element_line(color = "#ebebeb", size = 0.25),
                  strip.background = element_rect(fill = '#ebebeb'),
                  strip.text = element_text(family = "Roboto Light", colour = "#474747", size = 9),
                  plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
                  complete = TRUE,
                  grid_horizontal = TRUE, 
                  grid_vertical = TRUE, 
                  graph_contour = TRUE
                  )
  
  return(p)
  
  }
