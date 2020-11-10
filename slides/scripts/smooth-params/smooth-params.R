# load packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(palmerpenguins)

# define colors ----------------------------------------------------------------
beluga <- "#4A4843"
living_coral <- "#FA7268"
turkish_sea <- "#195190"
limpet_shell <- "#98DDDE"
turtle_green <- "#81894E"

# split data -------------------------------------------------------------------

penguins_nongentoo <- penguins %>%
  filter(species != "Gentoo")

# visualize the relationship ---------------------------------------------------

p_smooth <- ggplot(penguins_nongentoo) +
  geom_point(
    aes(x = flipper_length_mm, y = bill_depth_mm, color = species)
  ) +
  geom_smooth(
    aes(x = flipper_length_mm, y = bill_depth_mm),
    method = "loess", span = 0.375, color = beluga
  ) +
  labs(
    x = "Flipper length (mm)",
    y = "Bill length (mm)",
    color = "Species"
  ) +
  scale_color_manual(
    values = c("Chinstrap" = turkish_sea, "Adelie" = limpet_shell)
  )

ggsave(p_smooth,
       filename = here::here("slides/scripts/smooth-params/", "penguins-smooth.png"),
       width = 7, height = 4,
       dpi = "retina", bg = "transparent")
