library(tidyverse)
duke_forest <- openintro::duke_forest
glimpse(duke_forest)

ggplot(duke_forest, aes(x = area, y = price)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 0.7) +
  labs(
    x = "Area (square feet)",
    y = "Sale price (USD)",
    title = "Price and area of houses in Duke Forest"
  )


duke_forest <- duke_forest %>%
  mutate(decade_built = (year_built %/% 10) * 10)

duke_forest %>%
  select(year_built, decade_built)

duke_forest %>%
  count(decade_built)


duke_forest <- duke_forest %>%
  mutate(
    decade_built_cat = case_when(
      decade_built <= 1940 ~ "1940 or before",
      decade_built >= 1990 ~ "1990 or after",
      TRUE                 ~ as.character(decade_built)
    )
  )

duke_forest %>%
  count(decade_built_cat)

ggplot(duke_forest, aes(x = area, y = price, color = decade_built_cat)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, show.legend = FALSE) +
  facet_wrap(~decade_built_cat) +
  labs(
    x = "Area (square feet)",
    y = "Sale price (USD)",
    color = "Decade built",
    title = "Price and area of houses in Duke Forest"
  )


ggplot(duke_forest, aes(x = area, y = price, color = decade_built_cat)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, show.legend = FALSE) +
  facet_wrap(~decade_built_cat) +
  labs(
    x = "Area (square feet)",
    y = "Sale price (USD)",
    color = "Decade built",
    title = "Price and area of houses in Duke Forest"
  ) +
  theme_minimal(base_size = 16) +
  scale_color_viridis_d(end = 0.9)

ggplot(duke_forest, aes(x = area, y = price, color = decade_built_cat)) +
  geom_point(alpha = 0.5, size = 2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, show.legend = FALSE) +
  facet_wrap(~decade_built_cat) +
  labs(
    x = "Area (square feet)",
    y = "Sale price (USD)",
    color = "Decade built",
    title = "Price and area of houses in Duke Forest"
  ) +
  scale_color_viridis_d(end = 0.8, option = "A")

ggplot(duke_forest, aes(x = area, y = price, color = decade_built_cat)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, show.legend = FALSE) +
  facet_wrap(~decade_built_cat) +
  labs(
    x = "Area (square feet)",
    y = "Sale price (USD)",
    color = "Decade built",
    title = "Price and area of houses in Duke Forest"
  ) +
  theme_dark(base_size = 16) +
  scale_color_manual(values = c("yellow", "blue", "orange", "red", "green", "white")) +
  theme(
    text = element_text(color = "palegreen4", face = "bold"),
    plot.background = element_rect(fill = "seashell")
  )

mean_area_decade <- duke_forest %>%
  group_by(decade_built_cat) %>%
  summarise(mean_area = mean(area))


ggplot(mean_area_decade, aes(y = decade_built_cat, x = mean_area)) +
  geom_point(size = 4) +
  geom_segment(aes(
    xend = 
      mean_area,
    x =
      0, 
    y = decade_built_cat,
    yend = decade_built_cat
  )) +
  labs(
    x = "Mean area (square feet)", y = "Decade built",
    title = "Mean area of houses in Duke Forest, by decade built"
  ) +
  theme_minimal(base_size = 16)

ggplot(mean_area_decade, aes(y = decade_built_cat, x = mean_area)) +
  geom_point(size = 4) +
  geom_segment(aes(
    xend = 0,
    yend = decade_built_cat
  )) +
  labs(
    x = "Mean area (square feet)", y = "Decade built",
    title = "Mean area of houses in Duke Forest, by decade built"
  ) +
  theme_minimal(base_size = 16)










