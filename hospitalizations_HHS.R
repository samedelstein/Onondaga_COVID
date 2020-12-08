hospitals <- read.csv("data/reported_hospital_capacity_admissions_facility-level_weekly_average_timeseries_20201207.csv", stringsAsFactors = FALSE)
names(hospitals)
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
         hospitalized_covid = total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg /all_adult_hospital_inpatient_bed_occupied_7_day_avg) %>%
  filter(city == 'SYRACUSE', state == 'NY')

str(syracuse_hospitals)
ggplot(syracuse_hospitals, aes(collection_week, hospitalized_confirmed_suspected_covid, color = hospital_name)) +
  geom_line()

ggplot(syracuse_hospitals, aes(collection_week, total_staffed_ICU_fullness, color = hospital_name)) +
  geom_line()

ggplot(syracuse_hospitals, aes(collection_week, ICU_COVID, color = hospital_name)) +
  geom_line()

ggplot(syracuse_hospitals, aes(collection_week, hospitalized_covid, color = hospital_name)) +
  geom_line()
         