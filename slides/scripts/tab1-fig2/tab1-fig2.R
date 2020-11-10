# load packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
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

# run regression ---------------------------------------------------------------

lm(bill_depth_mm ~ flipper_length_mm, data = penguins) %>% tidy()

lm(bill_depth_mm ~ flipper_length_mm, data = penguins_nongentoo) %>% tidy()

# visualize non-gentoos --------------------------------------------------------

p_nongentoo <- ggplot(penguins_nongentoo) +
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(aes(x = flipper_length_mm, y = bill_depth_mm),
              method = "lm", color = beluga) +
  labs(
    x = "Bill depth (mm)",
    y = "Flipper length (mm)",
    color = "Species"
  ) +
  scale_color_manual(
    values = c("Chinstrap" = turkish_sea, "Adelie" = limpet_shell)
  )

ggsave(p_nongentoo,
       filename = here::here("slides/scripts/tab1-fig2/", "penguins-nongentoo.png"),
       width = 7, height = 4,
       dpi = "retina", bg = "transparent")

# plot with color --------------------------------------------------------------

p_nongentoo_hl <- ggplot(penguins_nongentoo) +
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(aes(x = flipper_length_mm, y = bill_depth_mm),
              method = "lm", color = living_coral, size = 3) +
  labs(
    x = "Bill depth (mm)",
    y = "Flipper length (mm)",
    color = "Species"
  ) +
  scale_color_manual(
    values = c("Chinstrap" = turkish_sea, "Adelie" = limpet_shell)
  )

ggsave(p_nongentoo_hl,
       filename = here::here("slides/scripts/tab1-fig2/", "penguins-nongentoo-hl.png"),
       width = 7, height = 4,
       dpi = "retina", bg = "transparent")

# plot with all three species --------------------------------------------------
p_all <- ggplot(penguins) +
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(aes(x = flipper_length_mm, y = bill_depth_mm),
              method = "lm", color = beluga) +
  labs(
    x = "Bill depth (mm)",
    y = "Flipper length (mm)",
    color = "Species"
  ) +
  scale_color_manual(
    values = c("Gentoo" = turtle_green,
               "Chinstrap" = turkish_sea,
               "Adelie" = limpet_shell)
  )

ggsave(p_all,
       filename = here::here("slides/scripts/tab1-fig2/", "penguins-all.png"),
       width = 7, height = 4,
       dpi = "retina", bg = "transparent")

# how did this happen ----------------------------------------------------------
# https://carbon.now.sh

## 1

# fit model
model <- lm(bill_depth_mm ~ flipper_length_mm, data = penguins)

# print model summary
tidy(model)

## 2

# visualize the relationship
ggplot(penguins) +
  geom_point(
    aes(x = bill_depth_mm, y = flipper_length_mm, color = species)
  ) +
  geom_smooth(
    aes(x = bill_depth_mm, y = flipper_length_mm),
    method = "lm"
  )

## 3

# filter out Gentoos
penguins_nongentoo <- penguins %>%
  filter(species != "Gentoo")

# visualize the relationship
ggplot(penguins_nongentoo) +
  geom_point(
    aes(x = bill_depth_mm, y = flipper_length_mm, color = species)
  ) +
  geom_smooth(
    aes(x = bill_depth_mm, y = flipper_length_mm),
    method = "lm"
  )
