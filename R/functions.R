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
  `white`        = "#FFFFFF",
  `cinza_escuro`  = "#4D4D4D",
  `cinza_claro`   = "grey85",
  `cinza_intermediario` = "D9D9D9",
  `azul_numera`   = "#5288DB",
  `preto` = "#000000")

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
numera_pal <- function(palette, reverse = FALSE, ...) {
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
scale_color_numera <- function(palette, discrete = TRUE, reverse = FALSE, ...) {
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
scale_fill_numera <- function(palette, discrete = TRUE, reverse = FALSE, ...) {
  pal <- numera_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("numera_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
