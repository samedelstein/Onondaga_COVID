SU_Cases <- fromJSON("https://minio.dev.digitalservices.syr.edu/data/public/covid/syr-stats.json") %>%
  mutate(DateRecorded = as.Date(DateRecorded))

SU_Cases <- SU_Cases %>%
  arrange(DateRecorded) %>%
  mutate(cumulative_positives = cumsum(NewStudentPositives), 
         Last.7.Days.Mean = (cumulative_positives - lag(cumulative_positives,7))/7)

write.csv(SU_Cases, "data/SU_Cases.csv", row.names = FALSE)


ggplot() +
  geom_point(data = SU_Cases, aes(DateRecorded, NewStudentPositives), color = "red", alpha = .2) +
  geom_line(data = SU_Cases, aes(DateRecorded, Last.7.Days.Mean), color = "red") +
  #geom_vline(mapping = aes(xintercept = as.Date('2020-10-31')), linetype = 'dashed') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "New COVID-19 Cases at Syracuse University",
       caption = "Source: https://www.syracuse.edu/covid-dashboard/",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))


ggplot(SU_Cases) +
  geom_hline(aes(yintercept = max(ActiveCases, na.rm = TRUE)), alpha = .5, color = 'black', linetype = 'dashed') +
  geom_line(aes(DateRecorded, ActiveCases, group = 1)) +
  labs(title = "Active Cases at Syracuse University",
       subtitle = paste("Data as of", max(SU_Cases$DateRecorded), sep = " "),
       caption = "Source: syracuse.edu",
       x = "",
       y = "Active Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())

ggplot(SU_Cases) +
  geom_hline(aes(yintercept = max(NewStudentPositives, na.rm = TRUE)), alpha = .5, color = 'black', linetype = 'dashed') +
  geom_line(aes(DateRecorded, NewStudentPositives, group = 1)) +
  labs(title = "New Cases at Syracuse University",
       subtitle = paste("Data as of", max(SU_Cases$DateRecorded), sep = " "),
       caption = "Source: syracuse.edu",
       x = "",
       y = "New Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())

ggplot(SU_Cases) +
  geom_hline(aes(yintercept = max(StudentsInQuarantine, na.rm = TRUE)), alpha = .5, color = 'black', linetype = 'dashed') +
  geom_line(aes(DateRecorded, StudentsInQuarantine, group = 1)) +
  labs(title = "Students In Quarantine at Syracuse University",
       subtitle = paste("Data as of", max(SU_Cases$DateRecorded), sep = " "),
       caption = "Source: syracuse.edu",
       x = "",
       y = "Students In Quarantine",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
