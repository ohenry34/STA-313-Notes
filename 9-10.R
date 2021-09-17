library(tidyverse)
library(glue)
library(lubridate)
library(scales)

hotels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

hotels_1 <- hotels %>%
  mutate(month = match(arrival_date_month,month.name)) %>%
  mutate(date = make_date(arrival_date_year, month, arrival_date_day_of_month))

daily_avg <- hotels_1 %>%
  group_by(date, hotel) %>%
  summarise(mean_daily_rate = mean(adr))

ggplot(daily_avg, aes(date, mean_daily_rate, color = hotel)) +
  geom_line() +
  scale_color_manual(values = c("cornsilk4", "deepskyblue")) +
  labs(title = "Cost of daily hotel stay",
       subtitle = "July 2015 to August 2017",
       x = "Arrival date",
       y = "Mean average daily rate (USD)") +
  scale_y_continuous(labels = label_dollar()) +
  theme(legend.position = c(0.15, 0.9),
        plot.subtitle=element_text(color = "cornsilk4"),
        legend.box.background = element_rect(fill = "white",
                                             color = "white"),
        legend.title = element_blank())

ggplot(daily_avg, aes(date, mean_daily_rate)) +
  geom_line(aes(linetype = hotel)) +
  labs(title = "Cost of daily hotel stay",
       subtitle = "July 2015 to August 2017",
       x = "Arrival date",
       y = "Mean average daily rate (USD)") +
  scale_y_continuous(labels = label_dollar()) +
  theme(legend.position = c(0.15, 0.9),
        plot.subtitle=element_text(color = "cornsilk4"),
        legend.box.background = element_rect(fill = "white",
                                             color = "white"),
        legend.title = element_blank())

hotels %>%
  select(adults, children, babies) %>%
  mutate(total = (adults + children + babies))
