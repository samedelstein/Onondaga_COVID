SU_Cases <- fromJSON("https://minio.dev.digitalservices.syr.edu/data/public/covid/syr-stats.json") %>%
  mutate(DateRecorded = as.Date(DateRecorded))

write.csv(SU_Cases, "data/SU_Cases.csv", row.names = FALSE)

tail(SU_Cases)
ggplot(SU_Cases, aes(DateRecorded, ActiveCases)) +
  geom_point()


ggplot(SU_Cases) +
  geom_hline(aes(yintercept = max(ActiveCases, na.rm = TRUE)), alpha = .5, color = 'black', linetype = 'dashed') +
  geom_line(aes(DateRecorded, ActiveCases, group = 1)) +
  labs(title = "Positive Tests at Syracuse University",
       subtitle = paste("Data as of", max(SU_Cases$DateRecorded), sep = " "),
       caption = "Source: syracuse.edu",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
