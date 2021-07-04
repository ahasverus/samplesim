#' 
#' Create an Hexagonal Sticker for the Package
#' 

x <- samplesim::get_output("test_siar", here::here("vignettes", "docs"), 
                           change = TRUE)

x <- x[x$"type" == "Median of posterior distribution", ]


p <- ggplot(aes(x = size, y = value, group = source), data = medians) +
  geom_point(color = "#2B663D", size = 0.35) +
  geom_line(color = "#2B663D", size = 0.15) +
  ggplot2::theme_void() + 
  ggplot2::theme(legend.position = "none") +
  ggpubr::theme_transparent()


hexSticker::sticker(
  
  subplot   = p,
  package   = "samplesim",
  filename  = here::here("man", "figures", "hexsticker.png"),
  dpi       = 600,
  
  p_size    = 36.0,        # Title
  u_size    =  6.0,        # URL
  p_family  = "Aller_Rg",
  
  p_color   = "#163B2D",   # Title
  h_fill    = "#BA9F69",   # Background
  h_color   = "#23542E",   # Border
  u_color   = "#163B2D",   # URL
  
  p_x       = 1.00,        # Title
  p_y       = 0.60,        # Title
  s_x       = 0.95,        # Subplot
  s_y       = 1.12,        # Subplot
  
  s_width   = 1.60,        # Subplot
  s_height  = 0.90,        # Subplot
  
  url       = "https://ahasverus.github.io/samplesim",
  
  spotlight = TRUE,
  l_alpha   = 0.10,
  l_width   = 4,
  l_height  = 4
)
