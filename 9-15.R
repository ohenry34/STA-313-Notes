# load packages
library(tidyverse)
library(glue)
library(lubridate)
library(scales)
library(knitr)
library(palmerpenguins)
library(openintro)
library(ggrepel)
library(waffle)

# set default theme for ggplot2
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 16))

# set default figure parameters for knitr
knitr::opts_chunk$set(
  fig.width = 8, fig.asp = 0.618, fig.retina = 3,
  dpi = 300, out.width = "60%"
)

# dplyr print min and max
options(dplyr.print_max = 6, dplyr.print_min = 6)

hotels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

hotels_summary <- hotels %>%
  group_by(hotel, is_canceled) %>%
  summarise(
    across(
      .cols = starts_with("stays"),
      list(mean = mean),
      .names = "{.fn}_{.col}"
    ),
    .groups = "drop"
  )
hotels_summary

hotels_summary %>%
  pivot_longer(., cols = c(mean_stays_in_week_nights, mean_stays_in_weekend_nights),
               names_to = "day_type",
               values_to = "mean_stays"
               ) %>%
  mutate(
    day_type = if_else(str_detect(day_type, "end"), "Weekend", "Weekday"),
    is_canceled = if_else(is_canceled == 0, "Not\nCanceled", "Canceled")
  ) %>%
  ggplot(aes(x = is_canceled, y = mean_stays, color = hotel, linetype = hotel, group = hotel)) +
  geom_line() +
  geom_point() +
  facet_wrap(~day_type) +
  scale_color_manual(values = c("cornsilk4", "deepskyblue3")) +
  labs(color = NULL, linetype = NULL,
       x = "Booking status",
       y = "Mean number of\nnights of stay") +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = 0:4, limits = c(0, 4))






