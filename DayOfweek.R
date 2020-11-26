##### State: County
#Weekday Test and Cases
x %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01') %>%
  group_by(weekday) %>%
  summarise(case_difference = mean(percent_diff_cases, na.rm = TRUE),
            test_difference = mean(percent_diff_tests, na.rm = TRUE)) %>%
  pivot_longer(!weekday, names_to = 'type', values_to = 'percent') %>%
  ggplot(aes(weekday, percent, fill = type)) +
  geom_col() + 
  geom_text(
    aes(label = paste0(round(percent,2), "%"), y = percent + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  ) +
  labs(title = "NYS Data: Onondaga County",
       subtitle = "Tests and Cases Difference from 7-Day Average By Weekday",
       caption = "Source: data.ny.gov",
       x = "",
       y = "%",
       color = '') +
  ylim(-30,60) +
  scale_fill_manual(values = c('#e34a33','#08519c'),name = "", labels = c('Cases', 'Tests')) +
  ggthemes::theme_economist() +
  theme(legend.position="bottom",
        strip.background = element_blank(), strip.text = element_blank()) +
  facet_wrap(~type, nrow = 2)
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSOnonCountyCasesvs7day.jpg", width = 10, height = 7)

#Daily vs Rolling 7 day Cases
x %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01') %>%
  ggplot() +
  geom_col(aes(Test.Date, New.Positives), fill = '#e34a33', alpha = .5) + 
  geom_line(aes(Test.Date, Roll.Last.7.Days.Cases, group = 1), color = '#e34a33') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "NYS Data: Onondaga County",
       subtitle = "Daily Cases, 7 Day Average Line",
       caption = "Source: data.ny.gov",
       x = "",
       y = "COVID-19 Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSOnonCountyrollingcases.jpg", width = 10, height = 7)


#daily vs Rolling 7 day tests
x %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01') %>%
  ggplot() +
  geom_col(aes(Test.Date, Total.Number.of.Tests.Performed), fill = '#08519c', alpha = .5) + 
  geom_line(aes(Test.Date, Roll.Last.7.Days.Tests, group = 1), color = '#08519c') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "NYS Data: Onondaga County",
       subtitle = "Daily Tests, 7 Day Average Line",
       caption = "Source: data.ny.gov",
       x = "",
       y = "COVID-19 Tests",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSOnonCountyrollingtests.jpg", width = 10, height = 7)






#State Overall

state_totals <- State_Covid %>%
  group_by(Test.Date) %>%
  summarise(New.Positives = sum(New.Positives),
            Cumulative.Number.of.Positives = sum(Cumulative.Number.of.Positives),
            Total.Number.of.Tests.Performed = sum(Total.Number.of.Tests.Performed),
            Cumulative.Number.of.Tests.Performed = sum(Cumulative.Number.of.Tests.Performed)) %>%
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
         Roll.Last.7.Days.Tests = zoo::rollmean(Total.Number.of.Tests.Performed, k = 7, fill = NA, align = "right"))

#Weekday Test and Cases
state_totals %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01') %>%
  group_by(weekday) %>%
  summarise(case_difference = mean(percent_diff_cases, na.rm = TRUE),
            test_difference = mean(percent_diff_tests, na.rm = TRUE)) %>%
  pivot_longer(!weekday, names_to = 'type', values_to = 'percent') %>%
  ggplot(aes(weekday, percent, fill = type)) +
  geom_col() + 
  geom_text(
    aes(label = paste0(round(percent,2), "%"), y = percent + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  ) + 
  labs(title = "NYS Data",
       subtitle = "Tests and Cases Difference from 7-Day Average By Weekday",
       caption = "Source: data.ny.gov",
       x = "",
       y = "%",
       color = '') +
  ylim(-20,40) +
  scale_fill_manual(values = c('#e34a33','#08519c'),name = "", labels = c('Cases', 'Tests')) +
  ggthemes::theme_economist() +
  theme(legend.position="bottom",
        strip.background = element_blank(), strip.text = element_blank()) +
  facet_wrap(~type, nrow = 2)
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSCasesvs7day.jpg", width = 10, height = 7)


#Daily vs Rolling 7 day Cases

state_totals %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01') %>%
  ggplot() +
  geom_col(aes(Test.Date, New.Positives), fill = '#e34a33', alpha = .5) +
  geom_line(aes(Test.Date, Roll.Last.7.Days.Cases, group = 1), color = '#e34a33') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
              labs(title = "NYS Data",
                   subtitle = "Daily Cases, 7 Day Average Line",
                   caption = "Source: data.ny.gov",
                   x = "",
                   y = "COVID-19 Cases",
                   color = '') +
              ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSrollingcases.jpg", width = 10, height = 7)



#Daily vs Rolling 7 day Tests

state_totals %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01') %>%
  ggplot() +
  geom_col(aes(Test.Date, Total.Number.of.Tests.Performed), fill = '#08519c', alpha = .5) +
  geom_line(aes(Test.Date, Roll.Last.7.Days.Tests, group = 1), color = '#08519c') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "NYS Data",
       subtitle = "Daily Tests, 7 Day Average Line",
       caption = "Source: data.ny.gov",
       x = "",
       y = "COVID-19 Tests",
       color = '') +
  ggthemes::theme_economist()  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSrollingtess.jpg", width = 10, height = 7)




#County Facet

byCounty <- State_Covid %>%
  group_by(County, Test.Date) %>%
  summarise(New.Positives = sum(New.Positives),
            Cumulative.Number.of.Positives = sum(Cumulative.Number.of.Positives),
            Total.Number.of.Tests.Performed = sum(Total.Number.of.Tests.Performed),
            Cumulative.Number.of.Tests.Performed = sum(Cumulative.Number.of.Tests.Performed)) %>%
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
         Roll.Last.7.Days.Tests = zoo::rollmean(Total.Number.of.Tests.Performed, k = 7, fill = NA, align = "right"))

Counties_1000 <- byCounty %>%
  group_by(County) %>%
  summarize(sum_Positives = sum(New.Positives)) %>%
  filter(sum_Positives > 5100)

#Cases Difference Day of Week

byCounty %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  group_by(County) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         #percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01' & County %in% Counties_1000$County) %>%
  group_by(weekday, County) %>%
  summarise(case_difference = mean(percent_diff_cases, na.rm = TRUE)) %>%
  #,test_difference = mean(percent_diff_tests, na.rm = TRUE)
  #pivot_longer(!weekday, names_to = 'type', values_to = 'percent') %>%
  ggplot(aes(weekday, case_difference)) +
  geom_col(fill = '#e34a33') + 
  labs(title = "NYS Data: Top 15 Counties",
       subtitle = "Cases Difference from 7-Day Average By Weekday",
       caption = "Source: data.ny.gov",
       x = "",
       y = "%",
       color = '') +
  #scale_fill_manual(values = c('#e34a33','#08519c'),name = "", labels = c('Cases', 'Tests')) +
  ggthemes::theme_economist() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90)) +
  facet_wrap(~County)
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSbyCountyCasesvs7day.jpg", width = 10, height = 7)

#Test difference
byCounty %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  group_by(County) %>%
  mutate(weekday = weekdays(Test.Date),
         #percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Test.Date > '2020-05-01' & County %in% Counties_1000$County) %>%
  group_by(weekday, County) %>%
  summarise(test_difference = mean(percent_diff_tests, na.rm = TRUE)) %>%
  #,
  #pivot_longer(!weekday, names_to = 'type', values_to = 'percent') %>%
  ggplot(aes(weekday, test_difference)) +
  geom_col(fill = '#08519c') + 
  labs(title = "NYS Data: Top 15 Counties",
       subtitle = "Test Difference from 7-Day Average By Weekday",
       caption = "Source: data.ny.gov",
       x = "",
       y = "%",
       color = '') +
  #scale_fill_manual(values = c('#e34a33','#08519c'),name = "", labels = c('Cases', 'Tests')) +
  ggthemes::theme_economist() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90)) +
  facet_wrap(~County)
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSbyCountyTestsvs7day.jpg", width = 10, height = 7)


#Daily Cases vs 7 day
byCounty %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  group_by(County) %>%
  mutate(weekday = weekdays(Test.Date),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         #percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday")),
         report_greater_avg = case_when(New.Positives > Roll.Last.7.Days.Cases ~ 'New',
                                        TRUE ~ 'Avg')) %>%
  filter(Test.Date > '2020-05-01' & County %in% Counties_1000$County) %>%
  ggplot() +
  geom_col(aes(Test.Date, New.Positives), fill = '#e34a33', alpha = .5) +
  geom_line(aes(Test.Date, Roll.Last.7.Days.Cases), color = '#e34a33') +
  labs(title = "NYS Data",
       subtitle = "Daily Cases, 7 Day Average Line",
       caption = "Source: data.ny.gov",
       x = "",
       y = "COVID-19 Cases",
       color = '') +
  ggthemes::theme_economist() +
  facet_wrap(~County)

ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSbyCountyrollingcases.jpg", width = 10, height = 7)


#Daily Tests vs 7 day
byCounty %>%
  select(Test.Date, New.Positives,Roll.Last.7.Days.Cases,Roll.Last.7.Days.Tests,Total.Number.of.Tests.Performed) %>%
  group_by(County) %>%
  mutate(weekday = weekdays(Test.Date),
         #percent_diff_cases = ((Roll.Last.7.Days.Cases - New.Positives)/((Roll.Last.7.Days.Cases + New.Positives)/2))*100,
         percent_diff_tests = ((Roll.Last.7.Days.Tests - Total.Number.of.Tests.Performed)/((Roll.Last.7.Days.Tests + Total.Number.of.Tests.Performed)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday")),
         report_greater_avg = case_when(New.Positives > Roll.Last.7.Days.Cases ~ 'New',
                                        TRUE ~ 'Avg')) %>%
  filter(Test.Date > '2020-05-01' & County %in% Counties_1000$County) %>%
  ggplot() +
  geom_col(aes(Test.Date, Total.Number.of.Tests.Performed), fill = '#08519c', alpha = .5) +
  geom_line(aes(Test.Date, Roll.Last.7.Days.Tests), color = '#08519c') +
  labs(title = "NYS Data: Top 15 Counties",
       subtitle = "Daily Tests, 7 Day Average Line",
       caption = "Source: data.ny.gov",
       x = "",
       y = "%",
       color = '') +
  ggthemes::theme_economist() +
  facet_wrap(~County)
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/NYSbyCountyrollingtests.jpg", width = 10, height = 7)



### County Numbers
#Weekday
county_case_mapping_df_new %>%
  select(DATE, new_cases) %>%
  mutate(weekday = weekdays(DATE),
         Roll.Last.7.Days.Cases = zoo::rollmean(new_cases, k = 7, fill = NA, align = "right"),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - new_cases)/((Roll.Last.7.Days.Cases + new_cases)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(DATE > '2020-05-01') %>%
  group_by(weekday) %>%
  summarise(case_difference = mean(percent_diff_cases, na.rm = TRUE)) %>%
  ggplot(aes(weekday, case_difference)) +
  geom_col(fill = 'red') +
  geom_text(
    aes(label = paste0(round(case_difference,2), "%"), y = case_difference + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  ) +
  ylim(-20,45) +
  labs(title = "Onondaga County Data",
       subtitle = 'Cases Difference From 7-Day Average By Weekday',
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "%",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.ticks = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/OnonCountyTestsvs7day.jpg", width = 10, height = 7)


#Daily vs 7 day
county_case_mapping_df_new %>%
  select(DATE, new_cases) %>%
  mutate(weekday = weekdays(DATE),
         Roll.Last.7.Days.Cases = zoo::rollmean(new_cases, k = 7, fill = NA, align = "right"),
         percent_diff_cases = ((Roll.Last.7.Days.Cases - new_cases)/((Roll.Last.7.Days.Cases + new_cases)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(DATE > '2020-05-01') %>%
  ggplot() +
  geom_col(aes(DATE, new_cases), fill = '#e34a33', alpha = .5) +
  geom_line(aes(DATE, Roll.Last.7.Days.Cases, group = 1), color = '#e34a33') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "Onondaga County Daily Cases, 7-Day Average Line",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/OnonCountyrollingcases.jpg", width = 10, height = 7)




##### County Hospitalizations

#Daily vs 7 day hospitalizations
df %>%
  mutate(Roll.Last.7.Days.Total.Hospitalized = zoo::rollmean(`Total Hospitalized`, k = 7, fill = NA, align = "right"),
         Roll.Last.7.Days.New.Admissions = zoo::rollmean(`New admissions`, k = 7, fill = NA, align = "right"),
         weekday = weekdays(Date),
         percent_diff_total_hospitalized = ((Roll.Last.7.Days.Total.Hospitalized - `Total Hospitalized`)/((Roll.Last.7.Days.Total.Hospitalized + `Total Hospitalized`)/2))*100,
         percent_diff_new_admissions = ((Roll.Last.7.Days.New.Admissions - `New admissions`)/((Roll.Last.7.Days.New.Admissions + `New admissions`)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Date > '2020-05-01') %>%
  ggplot() +
  geom_col(aes(Date,`Total Hospitalized`), fill = '#006d2c', alpha = .5) +
  geom_line(aes(Date,Roll.Last.7.Days.Total.Hospitalized, group = 1), color = '#006d2c') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "Onondaga County Hospitalizations, 7-Day Average Line",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Hospitalizations",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/HospitalizationsRolling.jpg", width = 10, height = 7)






df %>%
  mutate(Roll.Last.7.Days.Total.Hospitalized = zoo::rollmean(`Total Hospitalized`, k = 7, fill = NA, align = "right"),
         Roll.Last.7.Days.New.Admissions = zoo::rollmean(`New admissions`, k = 7, fill = NA, align = "right"),
         weekday = weekdays(Date),
         percent_diff_total_hospitalized = ((Roll.Last.7.Days.Total.Hospitalized - `Total Hospitalized`)/((Roll.Last.7.Days.Total.Hospitalized + `Total Hospitalized`)/2))*100,
         percent_diff_new_admissions = ((Roll.Last.7.Days.New.Admissions - `New admissions`)/((Roll.Last.7.Days.New.Admissions + `New admissions`)/2))*100,
         Sunday = case_when(weekday == 'Sunday' ~ 'Yes',
                            TRUE ~ 'No'),
         weekday = ordered(weekday, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday"))) %>%
  filter(Date > '2020-05-01') %>%
  group_by(weekday) %>%
  summarise(total_difference = mean(percent_diff_total_hospitalized, na.rm = TRUE),
            new_difference = mean(percent_diff_new_admissions, na.rm = TRUE)) %>%
  pivot_longer(!weekday, names_to = 'type', values_to = 'percent') %>%
  ggplot(aes(weekday, percent, fill = type)) +
  geom_col()  +   
  geom_text(
    aes(label = paste0(round(percent,2), "%"), y = percent + 0.1),
    position = position_dodge(0.9),
    vjust = 1
  ) +
  labs(title = "Onondaga County",
       subtitle = "New and Total Hospitalizations Difference from 7-Day Average By Weekday",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "%",
       color = '') +
  ylim(-10,50) +
  scale_fill_manual(values = c('#810f7c','#006d2c'),name = "", labels = c('New Hospitalizations', 'Total Hospitalizations')) +
  ggthemes::theme_economist() +
  theme(legend.position="bottom",
        strip.background = element_blank(), strip.text = element_blank()) +
  facet_wrap(~type, nrow = 2)

ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Hospializations7dayvsrolling.jpg", width = 10, height = 7)





