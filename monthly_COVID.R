state_county_COVID <- read.csv("data/State_Onondaga_COVID.csv", stringsAsFactors = FALSE)

state_county_COVID %>%
  mutate(month = month(Test.Date)) %>%
  group_by(month) %>%
  summarise(sum_cases = sum(New.Positives),
            sum_tests = sum(Total.Number.of.Tests.Performed))

hospitalizations <- read.csv("data/Onondaga_County_Hospitalizations.csv", stringsAsFactors = FALSE)

Avg_monthly_hospitalizations <- hospitalizations %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarise(mean(Total.Hospitalized))