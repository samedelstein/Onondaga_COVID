library(gridExtra)
library(grid)


state_county_COVID <- read.csv("data/State_Onondaga_COVID.csv", stringsAsFactors = FALSE)

monthly_state_county_COVID <- state_county_COVID %>%
  mutate(month = month(Test.Date, label = TRUE),
         year = year(Test.Date)) %>%
  group_by(month, year) %>%
  summarise(sum_cases = sum(New.Positives),
            sum_tests = sum(Total.Number.of.Tests.Performed))

p1 <-   ggplot(monthly_state_county_COVID,aes(month, sum_tests)) +
  geom_col(fill = 'darkblue') +
  ylim(0,max(monthly_state_county_COVID$sum_tests) + 40000) +
  geom_text(data=subset(monthly_state_county_COVID, month == 'Dec'),aes(x = month, y=sum_tests, label=sum_tests),                   
            position= position_dodge(width=0.9), hjust=-.25, angle = 90, color="darkblue") +
  labs(title = 'Monthly Tests',
       x = '',
       y = '') +
  theme_minimal() +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face="bold"),
        panel.grid.major.x = element_blank())

hospitalizations <- read.csv("data/Onondaga_County_Hospitalizations.csv", stringsAsFactors = FALSE)

hospitalizations_monthly <- hospitalizations %>%
  mutate(month = month(Date, label = TRUE,),
         year = year(Date)) %>%
  group_by(month, year) %>%
  summarise(avg_hospitalized = mean(Total.Hospitalized)) 

p3 <- ggplot(hospitalizations_monthly, aes(month, avg_hospitalized)) +
  geom_col(fill = '#56B4E9')  + 
  ylim(0,max(hospitalizations_monthly$avg_hospitalized) + 50) +
  geom_text(data=subset(hospitalizations_monthly, month == 'Dec'),aes(x = month, y=avg_hospitalized, label=round(avg_hospitalized)),                   
                         position= position_dodge(width=0.9), hjust=-.25, angle = 90, color="#56B4E9") +
  labs(title = 'Avg Monthly Hospitalized',
       x = '',
       y = '') +
  theme_minimal() +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face="bold"),
        panel.grid.major.x = element_blank())

county_COVID <- read.csv("data/county_case_mapping.csv",stringsAsFactors = FALSE)

county_case_data <- county_COVID %>%
  mutate(month = month(DATE, label = TRUE),
         new_deaths = DEATHS - lag(DEATHS,1)) %>%
  group_by(month) %>%
  summarise(sum_deaths = sum(new_deaths, na.rm = TRUE),
            sum_cases = sum(new_cases, na.rm = TRUE))

p4 <- ggplot(county_case_data) +
  geom_col(aes(month, sum_deaths), fill = 'black') +
  ylim(0,max(county_case_data$sum_deaths) + 50) +
  geom_text(data=subset(county_case_data, month == 'Dec'),aes(x = month, y=sum_deaths, label=sum_deaths),                   
            position= position_dodge(width=0.9), hjust=-.25, angle = 90, color="black") +
  labs(title = 'Monthly Deaths',
       x = '',
       y = '') +
  theme_minimal() +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face="bold"),
        panel.grid.major.x = element_blank())

p2 <- ggplot(county_case_data) +
  geom_col(aes(month, sum_cases), fill = 'red') +
  ylim(0,max(county_case_data$sum_cases) + 4000) +
  geom_text(data=subset(county_case_data, month == 'Dec'),aes(x = month, y=sum_cases, label=sum_cases),                   
            position= position_dodge(width=0.9), hjust=-.25, angle = 90, color="red") +
  labs(title = 'Monthly Cases',
       x = '',
       y = '') +
  theme_minimal() +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face="bold"),
        panel.grid.major.x = element_blank())






grid.arrange(p1, p2,p3,p4, nrow=1,top = grid::textGrob("ONONDAGA COUNTY COVID-19 METRICS BY MONTH", x = 0, hjust = 0, gp=gpar(fontsize=15,font=8)))

             