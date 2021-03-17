

library(hexSticker)
library(here)

sticker(
  here("documentation", "_assets", "big-ben.png"),
  package="britpol",
  p_size = 30,
  p_x = 1,
  p_y = 0.625,
  p_color = "#C8102E",
  h_color = "#012169",
  h_fill = "#FFFFFF",
  s_x = 1,
  s_y = 1.285,
  s_width = 0.525,
  asp = 0.85,
  filename = here("documentation", "_assets", "hex.png"),
  dpi = 320
)
