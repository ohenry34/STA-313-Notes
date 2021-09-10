library(tidyverse)
library(openintro)

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 16))

knitr::opts_chunk$set(
  fig.width = 8,     # 8"
  fig.asp = 0.618,   # the golden ratio
  fig.retina = 3,    # dpi multiplier for displaying HTML output on retina
  dpi = 300,         # higher dpi, sharper image
  out.width = "60%"
)

duke_forest <- duke_forest %>%
  mutate(
    decade_built = (year_built %/% 10) * 10,
    decade_built_cat = case_when(
      decade_built <= 1940 ~ "1940 or before",
      decade_built >= 1990 ~ "1990 or after",
      TRUE                 ~ as.character(decade_built)
    ),
    decade_built_cat = factor(decade_built_cat, ordered = TRUE)
  )
duke_forest %>%
  select(year_built, decade_built, decade_built_cat)

mean_area_decade <- duke_forest %>%
  group_by(decade_built_cat) %>%
  summarise(mean_area = mean(area))
mean_area_decade



ggplot(duke_forest, aes(x = area, y = price, color = decade_built_cat)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)

ggplot(duke_forest, aes(x = area, y = price)) +
  geom_point(aes(color = decade_built_cat), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)

duke_forest <- duke_forest %>%
  mutate(
    parking = case_when(
      parking == "0 spaces" ~ "Street",
      str_detect(parking, "Carport") ~ "Carport",
      str_detect(parking, "Garage") ~ "Garage",
      str_detect(parking, "Covered") ~ "Covered",
      TRUE ~ parking
    )
  )
duke_forest %>% 
  count(parking)

ggplot(duke_forest, aes(x = parking, y = price)) + 
  geom_point(alpha = 0.5) +
  scale_x_continuous()

ggplot(duke_forest, aes(x = parking, y = price)) + 
  geom_point(alpha = 0.5) +
  scale_y_discrete()
