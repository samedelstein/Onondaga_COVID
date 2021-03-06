setwd("~/Onondaga_COVID")
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(zoo)
library(ggthemes)
library(lubridate)

State_Covid <- read.csv("https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)

Cumulative_Positives_State <- ggplot(df, aes(Test.Date, Cumulative.Number.of.Positives, group = County)) +
  geom_line(color = "grey") +
  geom_line(data = filter(df, County == "Onondaga"), aes(Test.Date, Cumulative.Number.of.Positives, group = County), color = "red")+
  #scale_y_log10()+
  ggthemes::theme_fivethirtyeight()

Cumulative_Tests_State <- ggplot(df, aes(Test.Date, Cumulative.Number.of.Tests.Performed, group = County)) +
  geom_line(color = "grey") +
  geom_line(data = filter(df, County == "Onondaga"), aes(Test.Date, Cumulative.Number.of.Tests.Performed, group = County), color = "red")+
  #scale_y_log10()+
  ggthemes::theme_fivethirtyeight()
x <- State_Covid %>%
  filter(County == "Onondaga") %>%
  mutate(Test.Date = as.Date(Test.Date, "%m/%d/%Y"),
         Percent.Change = round(((Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 1) )/lag(Cumulative.Number.of.Positives, 1))*100),
         Three.Day.Average = (Cumulative.Number.of.Positives + lag(Cumulative.Number.of.Positives, 1) + lag(Cumulative.Number.of.Positives, 2))/3,
         Three.Day.Change = Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 4),
         Three.Day.Percentage = round(((Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 3))/lag(Cumulative.Number.of.Positives, 3))*100),
         Last.28.Days = Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives,28),
         Last.14.Days = Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives,14),
         Last.7.Days = Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives,7),
         Last.7.Days.Mean = (Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives,7))/7,
         Positive.Per.100000 = (Last.7.Days.Mean/460528)*100000,
         Percent.Positive = (New.Positives/Total.Number.of.Tests.Performed),
         Total.Percent.Positive = (Cumulative.Number.of.Positives/Cumulative.Number.of.Tests.Performed),
         Three.Day.Percent.Positive = ((New.Positives + lag(New.Positives, 1) + lag(New.Positives, 2)) / (Total.Number.of.Tests.Performed + lag(Total.Number.of.Tests.Performed,1) + lag(Total.Number.of.Tests.Performed,2))),
         Percent.Change.Tests = round(((Cumulative.Number.of.Positives - lag(Cumulative.Number.of.Positives, 1) )/lag(Cumulative.Number.of.Positives, 1))*100),
         Three.Day.Average.Tests = (Cumulative.Number.of.Tests.Performed + lag(Cumulative.Number.of.Tests.Performed, 1) + lag(Cumulative.Number.of.Tests.Performed, 2))/3,
         Three.Day.Change.Tests = Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed, 3),
         Three.Day.Percentage.Tests = round(((Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed, 3))/lag(Cumulative.Number.of.Tests.Performed, 3))*100),
         Last.28.Days.Tests = Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed,28),
         Last.14.Days.Tests = Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed,14),
         Last.7.Days.Tests = Cumulative.Number.of.Tests.Performed - lag(Cumulative.Number.of.Tests.Performed,7),
         Roll.Last.7.Days.Cases = zoo::rollmean(New.Positives, k = 7, fill = NA, align = "right"),
         Roll.Last.7.Days.Tests = zoo::rollmean(Total.Number.of.Tests.Performed, k = 7, fill = NA, align = "right"),
         Roll.Percent.Positive.7.days = (Roll.Last.7.Days.Cases/Roll.Last.7.Days.Tests)*100)
write.csv(x, "data/State_Onondaga_COVID.csv", row.names = FALSE)

x %>%
  mutate(week = week(Test.Date),
         year = year(Test.Date)) %>%
  group_by(week,year) %>%
  summarise(sumtests = sum(Total.Number.of.Tests.Performed)) %>%
ggplot(aes(week, sumtests)) +
  geom_col() +
  facet_wrap(~year)


Percent_positive_by_week_viz <- x %>%
  mutate(week = week(Test.Date),
         year = year(Test.Date)) %>%
  group_by(week, year) %>%
  summarise(case_weekly_total = sum(New.Positives),
            test_weekly_total = sum(Total.Number.of.Tests.Performed),
            weekly_percent_positive = (case_weekly_total/test_weekly_total)*100) %>%
  ggplot(aes(week, weekly_percent_positive)) +
  geom_col(fill = 'steelblue') +
  geom_text(
    aes(label = paste0(round(weekly_percent_positive,1)), x = week, y = weekly_percent_positive + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  ) +
  labs(title = "COVID-19 Hospitalizations in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Percent Positive",
       color = '')  +
  facet_wrap(~year) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Percent_positive_by_week_viz.jpg", plot = Percent_positive_by_week_viz, width = 10, height = 7)


zoo::rollmean(x$New.Positives, k = 7, fill = NA, align = "right")
lag(x$Cumulative.Number.of.Positives, 7)
average_cases_per_day <- mean(x$New.Positives, na.rm = TRUE)
x$Last.7.Days.Tests
tail(x)
ggplot(x) +
  geom_col(aes(Test.Date, New.Positives, fill = ifelse(New.Positives < average_cases_per_day,'red','green')))

colors_Total <- c(
  'Cumulative.Number.of.Positives' = '#d7191c',
  'New.Positives' = '#fdae61',
  'Rolling.Average.7.Day.Positives' = '#7b3294',
  'Total.Cases.Last.7.Days' = '#2c7bb6')

ggPositive <- ggplot(x) +
  geom_hline(aes(yintercept = max(New.Positives, na.rm = TRUE)), alpha = .5, color = 'black', linetype = 'dashed') +
  geom_hline(aes(yintercept = max(Last.7.Days, na.rm = TRUE)),alpha = .5, color = 'black', linetype = 'dashed') +
  geom_line(aes(Test.Date, Cumulative.Number.of.Positives, group = 1, color = "Cumulative.Number.of.Positives")) +
  geom_line(aes(Test.Date, Last.7.Days, group = 1, color = "Total.Cases.Last.7.Days"))  +
  geom_col(aes(Test.Date, New.Positives, group = 1, fill='New.Positives')) +
  #geom_line(aes(Test.Date, Last.7.Days.Mean, group = 1), color = 'black') +
  labs(title = "Positive Tests in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_color_manual(values = colors_Total) +
  scale_fill_manual(values = colors_Total) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Positive Tests in Onondaga County.jpg", plot = ggPositive, width = 10, height = 7)

ggPositiveLast30 <- ggplot(filter(x, Test.Date > Sys.Date() -30)) +
  geom_hline(aes(yintercept = max(New.Positives, na.rm = TRUE)), alpha = .5, color = 'purple', linetype = 'dashed') +
  geom_hline(aes(yintercept = max(Last.7.Days, na.rm = TRUE)),alpha = .5, color = 'black', linetype = 'dashed') +
  #geom_line(aes(Test.Date, Cumulative.Number.of.Positives, group = 1, color = "Cumulative.Number.of.Positives")) +
  geom_line(aes(Test.Date, Last.7.Days, group = 1, color = "Total.Cases.Last.7.Days"))  +
  geom_col(aes(Test.Date, New.Positives, group = 1, fill='New.Positives')) +
  geom_line(aes(Test.Date, Last.7.Days.Mean, group = 1, color = 'Rolling.Average.7.Day.Positives')) +
  labs(title = "Positive Tests in Onondaga County Last 30 Days",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_color_manual(values = colors_Total) +
  scale_fill_manual(values = colors_Total) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.title = element_blank())
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Positive Tests in Onondaga County Last 30 Days.jpg", plot = ggPositiveLast30, width = 10, height = 7)


colors_Tests <- c(
  'Cumulative.Number.of.Tests.Performed' = '#fdae61',
  'Last.7.Days.Tests' = '#7b3294',
  'Total.Number.of.Tests.Performed' = '#2c7bb6')


ggTests <- ggplot(x) +
  geom_line(aes(Test.Date, Cumulative.Number.of.Tests.Performed, group = 1, color = "Cumulative.Number.of.Tests.Performed")) +
  geom_line(aes(Test.Date, Last.7.Days.Tests, group = 1, color = "Last.7.Days.Tests"))  +
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
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Tests Performed in Onondaga County.jpg", plot = ggTests, width = 10, height = 7)


colors_Percent <- c(
  'Total.Percent.Positive' = '#fdae61',
  'Three.Day.Percent.Positive' = '#7b3294',
  'Percent.Positive' = '#2c7bb6')

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
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Percent Positive Cases in Onondaga County.jpg", plot = ggPercent, width = 10, height = 7)


Percent_positive_viz <- ggplot(x) +
  geom_point(aes(Test.Date, Percent.Positive*100), alpha = .5) +
  geom_line(aes(Test.Date, Roll.Percent.Positive.7.days)) +
  labs(title = "Percent Positive COVID Cases with Rolling Average in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Percent Positive Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_color_manual(values = colors_Percent) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Percent_positive_viz.jpg", plot = Percent_positive_viz, width = 10, height = 7)



colors_Positives <- c(
  'Rolling.7.Day.Positives' = '#fdae61',
  'New.Daily.Positives' = '#7b3294')



Rolling.7.Day <- ggplot(x) +
  geom_col(aes(Test.Date, New.Positives, group = 1, fill='New.Daily.Positives'), alpha = .5) +
  geom_line(aes(Test.Date, Last.7.Days.Mean, group = 1, color = 'Rolling.7.Day.Positives'), size = 1) +
  geom_hline(aes(yintercept = Last.7.Days.Mean[Test.Date == max(Test.Date)]), alpha = 1, color = 'red', linetype = 'dashed') +
  labs(title = "Positive Tests in Onondaga County with Rolling Average",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_color_manual(values = colors_Positives) +
  scale_fill_manual(values = colors_Positives) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.title = element_blank())
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Positive Tests in Onondaga County with Rolling Average.jpg", plot = Rolling.7.Day, width = 10, height = 7)

Per.100000 <- ggplot(x) +
  geom_hline(aes(yintercept = Positive.Per.100000[Test.Date == max(Test.Date)]), alpha = .7, color = 'red', linetype = 'dashed') +
  geom_line(aes(Test.Date, Positive.Per.100000, group = 1)) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Positive Cases per 100,000 People in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_color_manual(values = colors_Positives) +
  scale_fill_manual(values = colors_Positives) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Positive Cases per 100,000 People in Onondaga County.jpg", plot = Per.100000, width = 10, height = 7)

write.csv(x, "data/Onondaga_COVID_data.csv", row.names = FALSE,fileEncoding = "UTF-8")
str(x)
cuts <- data.frame(Ref = c("SU Students Return", "County In-Person &\n SCSD Remote Schooling \nStart", "SCSD Hybrid \nLearning Starts", "Halloween", 'Thanksgiving'),
                   vals = c(as.Date('2020-08-17'),as.Date('2020-09-14'), as.Date('2020-10-05'), as.Date('2020-10-31'), as.Date('2020-11-26')),
                   yvals = c(100,50,100,200,220),
                   xmin = as.Date('2020-09-09'),
                   xmax = as.Date('2020-09-14'),
                   ymin = 0,
                   ymax = 250,
                   stringsAsFactors = FALSE)


new_cases_key_dates <- ggplot(x, aes(Test.Date, New.Positives )) +
  geom_vline(mapping = aes(xintercept = vals,
                                        colour = Ref),
                          data = cuts,
                          show.legend = FALSE) +
  geom_text(mapping = aes(x = vals,
                          y = yvals,
                          label = Ref,
                          hjust = 1,
                          vjust = 0,
                          color = Ref),
            data = cuts) +
  geom_point() +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "New Cases in Onondaga County",
       subtitle = "Key Dates Highlighted",
       caption = "Source: data.ny.gov",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/new_cases_key_dates.jpg", plot = new_cases_key_dates, width = 10, height = 7)



timeto1000 <- x %>%
  mutate(by1000 = floor(Cumulative.Number.of.Positives/1000)*1000) %>%
  group_by(by1000) %>%
  slice(which.min(Test.Date)) %>%
  select(Test.Date, by1000)

timeto1000$days <- difftime( timeto1000$Test.Date,lag(timeto1000$Test.Date,1))
timeto1000 <- timeto1000 %>% filter(by1000 != 0)

DaysToReach1000Cases <- ggplot(timeto1000, aes(factor(by1000), days)) +
  geom_col() +
  geom_text(
    aes(label = paste0(days, " days"), y = days + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  )+ 
  labs(title = "Days to Reach the Next 1,000 Cases in Onondaga County",
        caption = "Source: data.ny.gov",
        x = "Thousand Cases",
        y = "Number of Days",
        color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/DaysToReach1000Cases.jpg", plot = DaysToReach1000Cases, width = 10, height = 7)



head(x)

x %>%
  mutate(month = month(Test.Date),
         year = year(Test.Date)) %>%
  group_by(month, year) %>%
  summarise(sum_cases = sum(New.Positives),
            sum_tests = sum(Total.Number.of.Tests.Performed))
