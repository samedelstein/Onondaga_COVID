library(ggrepel)

hospitals <- read.csv("data/reported_hospital_capacity_admissions_facility-level_weekly_average_timeseries_20201207.csv", stringsAsFactors = FALSE)

hospitals %>%
   filter(city == 'SYRACUSE', state == 'NY') %>%
  select(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_coverage)


syracuse_hospitals <- hospitals %>%
  select(c(city, state, hospital_name, collection_week, total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
             all_adult_hospital_inpatient_beds_7_day_avg,
           staffed_adult_icu_bed_occupancy_7_day_avg,
             total_staffed_adult_icu_beds_7_day_avg ,
             staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg,
             total_staffed_adult_icu_beds_7_day_avg ,
             total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
             all_adult_hospital_inpatient_bed_occupied_7_day_avg)) %>%
  mutate(na_if(., -999999),
         collection_week = as.Date(collection_week),
         hospitalized_confirmed_suspected_covid = total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg/ all_adult_hospital_inpatient_beds_7_day_avg,
         total_staffed_ICU_fullness = staffed_adult_icu_bed_occupancy_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg,
         ICU_COVID = staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg/ total_staffed_adult_icu_beds_7_day_avg,
         hospitalized_covid = total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg /all_adult_hospital_inpatient_bed_occupied_7_day_avg,
         remaining_beds = total_staffed_adult_icu_beds_7_day_avg - staffed_adult_icu_bed_occupancy_7_day_avg) %>%
  filter(city == 'SYRACUSE', state == 'NY')

group_syracuse_hospitals <- syracuse_hospitals %>%
  group_by(collection_week) %>%
  summarise(tot_patients_covid = sum(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg, na.rm = TRUE),
            inpatient_beds = sum(all_adult_hospital_inpatient_beds_7_day_avg, na.rm = TRUE),
            icu_beds_occupied = sum(staffed_adult_icu_bed_occupancy_7_day_avg, na.rm = TRUE),
            icu_beds = sum(total_staffed_adult_icu_beds_7_day_avg, na.rm = TRUE),
            icu_beds_occupied_covid = sum(staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg, na.rm = TRUE),
            inpatient_bed_occupied = sum(all_adult_hospital_inpatient_bed_occupied_7_day_avg, na.rm = TRUE))
#Show averages per hospital 

colors_Total <- c(
  "UNIVERSITY HOSPITAL S U N Y HEALTH SCIENCE CENTER" = '#d7191c',
  "CROUSE HOSPITAL" = '#2c7bb6',
  "ST JOSEPH'S HOSPITAL HEALTH CENTER"  = '#7b3294')

hospitalized_confirmed_suspected_covid_viz = ggplot(syracuse_hospitals, aes(collection_week, hospitalized_confirmed_suspected_covid, color = hospital_name) ) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  scale_color_manual(values = colors_Total) +
  labs(title = "Percentage of Available Hospital Inpatient Beds\n with Suspected or Confirmed COVID Patients",
       subtitle = 'Each Hospital in Syracuse',
       source = 'healthdata.gov',
       x = '',
       y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5)) +
  geom_text_repel(data=subset(syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(hospitalized_confirmed_suspected_covid * 100), '%')),
                  size=5,
                  nudge_x=5,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/hospitalized_confirmed_suspected_covid_viz.jpg", plot = hospitalized_confirmed_suspected_covid_viz, width = 10, height = 7)


total_staffed_ICU_fullness_viz = ggplot(syracuse_hospitals, aes(collection_week, total_staffed_ICU_fullness, color = hospital_name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colors_Total) +
  labs(title = "Percentage of ICU Beds Occupied for Any Reason",
       subtitle = 'Each Hospital in Syracuse',
       source = 'healthdata.gov',
       x = '',
       y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  geom_text_repel(data=subset(syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(total_staffed_ICU_fullness * 100), '%')),
                  size=5,
                  nudge_x=-5,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/total_staffed_ICU_fullness_viz.jpg", plot = total_staffed_ICU_fullness_viz, width = 10, height = 7)


ICU_COVID_viz <- ggplot(syracuse_hospitals, aes(collection_week, ICU_COVID, color = hospital_name)) +
  geom_line()+
  geom_point() +
  scale_color_manual(values = colors_Total) +
  labs(title = "Percentage of ICU Beds Occupied with COVID-19 Patients",
       subtitle = 'Each Hospital in Syracuse',
       source = 'healthdata.gov',
       x = '',
       y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5)) +
  geom_text_repel(data=subset(syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(ICU_COVID * 100), '%')),
                  size=5,
                  nudge_x=-5,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/ICU_COVID_viz.jpg", plot = ICU_COVID_viz, width = 10, height = 7)




hospitalized_covid_viz <- ggplot(syracuse_hospitals, aes(collection_week, hospitalized_covid, color = hospital_name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colors_Total) +
  labs(title = "Percentage of Hospitalized Patients that have COVID-19",
        subtitle = 'Each Hospital in Syracuse',
                    source = 'healthdata.gov',
                    x = '',
                    y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5)) +
  geom_text_repel(data=subset(syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(hospitalized_covid * 100), '%')),
                  size=5,
                  nudge_x=-5,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/hospitalized_covid_viz.jpg", plot = hospitalized_covid_viz, width = 10, height = 7)



#add sums together and average for entire city



group_syracuse_hospitals <- group_syracuse_hospitals %>%
  mutate(hospitalized_confirmed_suspected_covid = tot_patients_covid/inpatient_beds,
         total_staffed_ICU_fullness = icu_beds_occupied/icu_beds,
         ICU_COVID = icu_beds_occupied_covid/icu_beds,
         hospitalized_covid = tot_patients_covid/inpatient_bed_occupied)



hospitalized_confirmed_suspected_covid_allcuse_viz <- ggplot(group_syracuse_hospitals, aes(collection_week, hospitalized_confirmed_suspected_covid) ) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  labs(title = "Percentage of Available Hospital Inpatient Beds\n with Suspected or Confirmed COVID Patients",
       subtitle = 'All Syracuse Hospitals Combined',
       source = 'healthdata.gov',
       x = '',
       y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5)) +
  geom_text_repel(data=subset(group_syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(hospitalized_confirmed_suspected_covid * 100), '%')),
                  size=5,
                  nudge_x=-5,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/hospitalized_confirmed_suspected_covid_allcuse_viz.jpg", plot = hospitalized_confirmed_suspected_covid_allcuse_viz, width = 10, height = 7)


total_staffed_ICU_fullness_allcuse_viz <- ggplot(group_syracuse_hospitals, aes(collection_week, total_staffed_ICU_fullness) ) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  labs(title = "Percentage of ICU Beds Occupied for Any Reason",
       subtitle = 'All Syracuse Hospitals Combined',
       source = 'healthdata.gov',
       x = '',
       y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  geom_text_repel(data=subset(group_syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(total_staffed_ICU_fullness * 100), '%')),
                  size=5,
                  nudge_y=-.1,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/total_staffed_ICU_fullness_allcuse_viz.jpg", plot = total_staffed_ICU_fullness_allcuse_viz, width = 10, height = 7)


ICU_COVID_allcuse_viz <- ggplot(group_syracuse_hospitals, aes(collection_week, ICU_COVID) ) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  labs(title = "Percentage of ICU Beds Occupied with COVID-19 Patients",
       subtitle = 'All Syracuse Hospitals Combined',
       source = 'healthdata.gov',
       x = '',
       y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5)) +
  geom_text_repel(data=subset(group_syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(ICU_COVID * 100), '%')),
                  size=5,
                  nudge_x=-5,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/ICU_COVID_allcuse_viz.jpg", plot = ICU_COVID_allcuse_viz, width = 10, height = 7)


hospitalized_covid_allcuse_viz <- ggplot(group_syracuse_hospitals, aes(collection_week, hospitalized_covid) ) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  labs(title = "Percentage of Hospitalized Patients that have COVID-19",
       subtitle = 'All Syracuse Hospitals Combined',
       source = 'healthdata.gov',
       x = '',
       y = '') +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5)) +
  geom_text_repel(data=subset(group_syracuse_hospitals, collection_week==max(collection_week)),
                  aes(label= paste0(round(hospitalized_covid * 100), '%')),
                  size=5,
                  nudge_x=-5,
                  show.legend = FALSE,
                  arrow = arrow(length = unit(0.015, "npc"))) +
  ggthemes::theme_economist() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size=9),
        legend.title = element_blank()) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/hospitalized_covid_allcuse_viz.jpg", plot = hospitalized_covid_allcuse_viz, width = 10, height = 7)
