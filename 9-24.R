library(tidyverse)
library(colorspace)
library(colorblindr)
library(ggtext)
library(scales)
library(fs)
library(openintro)
library(palmerpenguins)
library(dsbox)
library(scales)
# set default theme for ggplot2
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 16))
# set default figure parameters for knitr
knitr::opts_chunk$set(
  fig.width = 8, fig.asp = 0.618, fig.retina = 3,
  dpi = 300, out.width = "60%"
)
# dplyr print min and max
options(dplyr.print_max = 6, dplyr.print_min = 6)

p <- ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point(size = 3) +
  theme(legend.position = "top")

p +
  scale_color_discrete_qualitative() # better bc not inherent ordering in data
p +
  scale_color_discrete_sequential()


p +
  scale_color_OkabeIto()

lyme_data <- tribble(
  ~state, ~n,
  "Pennsylvania",         10208,
  "New Jersey",            4000,
  "New York",              3638,
  "Wisconsin",             1869,
  "Connecticut",           1859,
  "Maine",                 1405,
  "Minnesota",             1541,
  "New Hampshire",         1428,
  "Maryland",              1382,
  "Virginia",              1139,
  "Rhode Island",          1111,
  "West Virginia",          671,
  "Vermont",                576,
  "Delaware",               520,
  "Ohio",                   293,
  "Remaining States + DC", 2026
)


# plot -------------------------------------------------------------------------

lyme_data <- lyme_data %>%
  mutate(
    state = fct_reorder(state, n),
    state = fct_relevel(state, "Remaining States + DC", after = 0),
    highlight = if_else(state == "Connecticut", "1", "0"),
    perc = paste0(round(n/sum(n), 3) * 100, "%")
  )
ggplot(lyme_data, aes(x = n, y = state, fill= highlight)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = perc, color = highlight, size = highlight), nudge_x = 500, show.legend = F) +
  scale_x_continuous(label = comma) +
  scale_fill_manual(values = c("gray", "#0c2d83")) +
  scale_color_manual(values = c("black", "#0c2d83")) +
  scale_size_manual(values=c(3,4))

rdu_flights_2020 <- read_csv(here::here("Data/rdu-flights-2020.csv"))
rdu_planes_2020 <- read_csv(here::here("Data/rdu-planes-2020.csv"))

# join and wrangle data --------------------------------------------------------

rdu_flights_planes <- rdu_flights_2020 %>%
  inner_join(rdu_planes_2020, by = "tailnum") %>%
  mutate(
    size = case_when(
      seats <= 30 ~ "Small",
      seats >= 31 & seats <= 110 ~ "Medium",
      seats >= 111 & seats <= 210 ~ "Large",
      seats > 211 ~ "Jumbo"
    ),
    size = fct_relevel(size, "Small", "Medium", "Large", "Jumbo")
  )

# plot -------------------------------------------------------------------------

rdu_flights_planes %>%
  count(hour, size) %>%
  ggplot(aes(x = hour, y = n, color = size)) +
  geom_line() +
  labs(
    x = "Hour of day",
    y = "Number of flights",
    color = "Size of plane",
    title = "Number of flights out of RDU in 2020",
    subtitle = "By size of plane",
    caption = "Source: FAA Aircraft Registry and\nBureau of Transportation Statistics"
  )




