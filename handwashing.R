yearly <- read.csv(file.choose())

attach(yearly)

head(yearly)

library(tidyverse)

yearly <- yearly %>%
  
  mutate(proportion_deaths = deaths/births)

ggplot(data = yearly, aes(year, proportion_deaths, colour= clinic)) + geom_line()

monthly <- read.csv(file.choose())

monthly <- monthly %>%
  mutate(proportion_deaths= deaths/births)

head(monthly)

ggplot(monthly, aes(date, proportion_deaths, group = 1)) +
  geom_line() +
  labs(x= "Year", y = "Proportion Deaths")

handwashing_start = as.Date('1847-06-01')

monthly <- monthly %>%
  mutate(handwashing_started = date >= handwashing_start)

ggplot(monthly, aes(date, proportion_deaths, color = handwashing_started, group = 1)) +
  geom_line() +
  labs(x = "Year", y= "Proportion Deaths")

monthly_summary <- monthly %>%
  group_by(handwashing_started) %>%
  summarise(mean_proportion_deaths = mean(proportion_deaths))

monthly_summary

monthly

test_result <- t.test(proportion_deaths ~ handwashing_started, data = monthly)
  
  
  
