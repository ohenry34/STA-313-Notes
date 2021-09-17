library(tidyverse)
library(knitr)
library(openintro)
library(palmerpenguins)
library(ggrepel)
library(waffle)
library(broom)

options(warn = -1)


# set default theme for ggplot2
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 16))
# set default figure parameters for knitr
knitr::opts_chunk$set(
  fig.width = 8, fig.asp = 0.618, fig.retina = 3,
  dpi = 300, out.width = "60%"
)
# dplyr print min and max
options(dplyr.print_max = 6, dplyr.print_min = 6)

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_smooth(method = "lm", fullrange = TRUE, se = FALSE)

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() + geom_smooth() +
  labs(title = "Plot 1")
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() + geom_smooth() +
  scale_x_continuous(limits = c(190, 220)) + scale_y_continuous(limits = c(4000, 5000)) +
  labs(title = "Plot 2")
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() + geom_smooth() +
  xlim(190, 220) + ylim(4000, 5000) +
  labs(title = "Plot 3")
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() + geom_smooth() +
  coord_cartesian(xlim = c(190,220), ylim = c(4000, 5000)) +
  labs(title = "Plot 4")


library(maps)
ggplot(map_data("state"), aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "#3c3b6e") +
  labs(x = NULL, y = NULL) +
  coord_quickmap()

ggplot(map_data("state"), aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "#3c3b6e") +
  labs(x = NULL, y = NULL) +
  coord_map()

ggplot(map_data("state"), aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "#3c3b6e") +
  labs(x = NULL, y = NULL) +
  coord_map(projection = "stereographic")

ggplot(map_data("world"), aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "#3c3b6e") +
  labs(x = NULL, y = NULL) +
  coord_map(projection = "mercator")

ggplot(penguins, aes(y = species, x = body_mass_g, fill = species)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(island ~ ., scales = "free_y", space = "free") +
  labs(title = "Free y scale and spacing")

penguins_sans_species <- penguins %>% select(-species)
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(data = penguins_sans_species, color = "gray") +
  geom_point(aes(color = species)) +
  facet_wrap(~species)



options(warn = 0)
