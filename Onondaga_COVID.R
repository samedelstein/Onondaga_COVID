library(tidyverse)
library(ggplot2)
library(gridExtra)


df <- read.csv("https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)

head(df)
ggplot(df, aes(Test.Date, Cumulative.Number.of.Positives, group = County)) +
  geom_line(color = "grey") +
  geom_line(data = filter(df, County == "Onondaga"), aes(Test.Date, Cumulative.Number.of.Tests.Performed, group = County), color = "red")+
  #scale_y_log10()+
  ggthemes::theme_fivethirtyeight()

x <- df %>%
  filter(County == "Onondaga") %>%
  mutate(Test.Date = as.Date(Test.Date, "%m/%d/%Y"),
         Percent.Change = round(((Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 1) )/lag(Cumulative.Number.of.Positives, 1))*100),
         Three.Day.Average = (Cumulative.Number.of.Positives + lag(Cumulative.Number.of.Positives, 1) + lag(Cumulative.Number.of.Positives, 2))/3,
         Three.Day.Change = Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 4),
         Three.Day.Percentage = round(((Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 4))/lag(Cumulative.Number.of.Positives, 4))*100),
         Last.28.Days = Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives,29),
         Last.14.Days = Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives,15),
         Percent.Positive = round((New.Positives/Total.Number.of.Tests.Performed)*100),
         Total.Percent.Positive = round((Cumulative.Number.of.Positives/Cumulative.Number.of.Tests.Performed)*100),
         Three.Day.Percent.Positive = round(((New.Positives + lag(New.Positives, 1) + lag(New.Positives, 2)) / (Total.Number.of.Tests.Performed + lag(Total.Number.of.Tests.Performed,1) + lag(Total.Number.of.Tests.Performed,2))*100)),
         Percent.Change.Tests = round(((Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 1) )/lag(Cumulative.Number.of.Positives, 1))*100),
         Three.Day.Average.Tests = (Cumulative.Number.of.Tests.Performed + lag(Cumulative.Number.of.Tests.Performed, 1) + lag(Cumulative.Number.of.Tests.Performed, 2))/3,
         Three.Day.Change.Tests = Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed, 4),
         Three.Day.Percentage.Tests = round(((Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed, 4))/lag(Cumulative.Number.of.Tests.Performed, 4))*100),
         Last.28.Days.Tests = Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed,29),
         Last.14.Days.Tests = Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed,15))

colors_Total <- c(
  'Cumulative.Number.of.Positives' = 'steelblue',
  'Last.14.Days' = 'black',
  'New.Positives' = 'orange')

ggPositive <- ggplot(x) +
  geom_line(aes(Test.Date, Cumulative.Number.of.Positives, group = 1, color = "Cumulative.Number.of.Positives")) +
  geom_line(aes(Test.Date, Last.14.Days, group = 1, color = "Last.14.Days"))  +
  geom_line(aes(Test.Date, New.Positives, group = 1, color="New.Positives")) +
  labs(title = "Positive Tests in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_color_manual(values = colors_Total) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))

colors_Tests <- c(
  'Cumulative.Number.of.Tests.Performed' = 'steelblue',
  'Last.14.Days.Tests' = 'black',
  'Total.Number.of.Tests.Performed' = 'orange')

ggTests <- ggplot(x) +
  geom_line(aes(Test.Date, Cumulative.Number.of.Tests.Performed, group = 1, color = "Cumulative.Number.of.Tests.Performed")) +
  geom_line(aes(Test.Date, Last.14.Days.Tests, group = 1, color = "Last.14.Days.Tests"))  +
  geom_line(aes(Test.Date, Total.Number.of.Tests.Performed, group = 1, color="Total.Number.of.Tests.Performed")) +
  labs(title = "Tests Performed in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_color_manual(values = colors_Tests) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))

colors_Percent <- c(
  'Total.Percent.Positive' = 'steelblue',
  'Three.Day.Percent.Positive' = 'black',
  'Percent.Positive' = 'orange')

ggPercent <- ggplot(x) +
  geom_line(aes(Test.Date, Three.Day.Percent.Positive, group = 1, color = "Three.Day.Percent.Positive")) +
  geom_line(aes(Test.Date, Percent.Positive, group = 1, color = "Percent.Positive")) +
  geom_line(aes(Test.Date, Total.Percent.Positive, group = 1, color = "Total.Percent.Positive"), size = 2) +
  labs(title = "Percent Positive Cases in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Percent Positive Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_color_manual(values = colors_Percent) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))

grid.arrange(ggPositive, ggTests, ggPercent, nrow = 2)

