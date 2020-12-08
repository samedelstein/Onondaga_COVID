library(stringi)
library(rvest)
library(hrbrthemes) # git[la|hu]b / hrbrmstr / hrbrthemes
library(tidyverse)
library(data.table)

#County Hospitalizations
data_list <- read_html('https://datawrapper.dwcdn.net/I4IZD/131/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'New admissions,') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Date ,Total Hospitalized,New admissions,Total Critical Con", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
df <- data.table::fread(paste(step_2, collapse = "\n"))
names(df) <- c("Date", "Total Hospitalized","New admissions","Total Critical Condition","Baseline")
df$Baseline <- gsub("\\", "", df$Baseline,  fixed = TRUE)

df$Date <- as.Date(df$Date, "%m/%d/%y")

df %>%
  mutate(mean.7.days = zoo::rollmean(`New admissions`, k = 7, fill = NA, align = 'right')) %>%
  ggplot( ) +
  geom_point(aes(Date, `New admissions`), alpha = .5) +
  geom_line(aes(Date, mean.7.days))

Hospitalizations_per_week_viz <- df %>%
  mutate(Last.7.Days.Mean_County = zoo::rollmean(`Total Hospitalized`, k = 7, fill = NA, align = "right"),
         week = week(Date)) %>%
  group_by(week) %>%
  summarise(sum_total_hospitalized = sum(`New admissions`)) %>%
  ggplot(aes(week, sum_total_hospitalized)) +
  geom_col(fill = "steelblue") +  
  geom_text(
    aes(label = sum_total_hospitalized),
    position = position_dodge(0.9),
    vjust = -.5
  ) +
  #scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "New COVID Hospitalizations Per Week (Sunday - Saturday)",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Hospitalizations",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Hospitalizations_per_week_viz.jpg", plot = Hospitalizations_per_week_viz, width = 10, height = 7)


write.csv(df, "data/Onondaga_County_Hospitalizations.csv", row.names = FALSE)
hospitalizations <- read.csv("data/Onondaga_County_Hospitalizations.csv", stringsAsFactors = FALSE)

Avg_monthly_hospitalizations <- hospitalizations %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarise(mean(Total.Hospitalized))


Total_COVID_Hospitalizations_CountyData <- ggplot(df, aes(Date, `Total Hospitalized`)) +
  geom_col() +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "Current COVID-19 Hospital Admissions in Onondaga County",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Current Hospitalized",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/Total_COVID_Hospitalizations_CountyData.jpg", plot = Total_COVID_Hospitalizations_CountyData, width = 10, height = 7)






cuts <- data.frame(Ref = c("SU Students Return", "SCSD Remote \nLearning Starts", "SCSD Hybrid \nLearning Starts", "Halloween"),
                   vals = c(as.Date('2020-08-17'),as.Date('2020-09-14'), as.Date('2020-10-05'), as.Date('2020-10-31')),
                   yvals = c(5,5,8,12),
                   stringsAsFactors = FALSE)

new_COVID_Hospitalizations_CountyData <- ggplot(df, aes(Date, `New admissions` )) +
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
  labs(title = "New COVID-19 Hospital Admissions in Onondaga County",
       subtitle = "Key Dates Highlighted",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/new_COVID_Hospitalizations_CountyData.jpg", plot = new_COVID_Hospitalizations_CountyData, width = 10, height = 7)

colors_Total <- c(
  'Total.Hospitalized' = '#d7191c',
  'Total.Critical.Condition' = '#fdae61')

rolling_hospitalizations_viz <- df %>%
  mutate(Last.7.Days.Mean_County = zoo::rollmean(`Total Hospitalized`, k = 7, fill = NA, align = "right"),
         Last.7.Days.Critical_County = zoo::rollmean(`Total Critical Condition`, k = 7, fill = NA, align = "right")) %>%
  ggplot() +
  geom_col(aes(Date, `Total Hospitalized`, fill = 'Total.Hospitalized'), alpha = .5 ) +
  geom_col(aes(Date, `Total Critical Condition`, fill = 'Total.Critical.Condition'), alpha = .5) +
  geom_line(aes(Date,Last.7.Days.Mean_County, color = 'Total.Hospitalized')) +
  geom_line(aes(Date,Last.7.Days.Critical_County, color = 'Total.Critical.Condition')) +
  labs(title = "COVID-19 Hospitalizations in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Hospitalizations",
       color = '') +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/rolling_hospitalizations_viz.jpg", plot = rolling_hospitalizations_viz, width = 10, height = 7)



hospitalizations_by_week <- df %>%
  mutate(week = week(Date)) %>%
  group_by(week) %>%
  summarise(sum_total_hospitalized = sum(`Total Hospitalized`),
            sum_total_critical_condition = sum(`Total Critical Condition`))

hospitalizations_by_week_viz <-   ggplot(hospitalizations_by_week) +
  geom_col(aes(week, sum_total_hospitalized, fill = 'Total.Hospitalized'), alpha= .5) +
  geom_col(aes(week, sum_total_critical_condition, fill = 'Total.Critical.Condition'), alpha = .5) +
  geom_text(
    aes(label = paste0(sum_total_hospitalized), x = week, y = sum_total_hospitalized + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  ) +
  geom_text(
    aes(label = paste0(sum_total_critical_condition), x = week, y = sum_total_critical_condition + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  ) +
    labs(title = "COVID-19 Hospitalizations in Onondaga County",
         subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
         caption = "Source: covid19.ongov.net/data",
         x = "",
         y = "Hospitalizations",
         color = '') +
  scale_color_manual(values = colors_Total) +
  scale_fill_manual(values = colors_Total)+
    ggthemes::theme_economist() +
    theme(axis.text.x = element_text(angle = 90))+
    theme(legend.title = element_blank())
  ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/hospitalizations_by_week_viz.jpg", plot = hospitalizations_by_week_viz, width = 10, height = 7)
  

merge(df, county_case_mapping_df_new, by.x = 'Date', by.y = 'DATE') %>%
  ggplot() +
  geom_col(aes(Date, `New admissions`), alpha = .5, color = 'red', fill = 'red') +
  geom_col(aes(Date, new_cases), alpha = .5, color = 'blue', fill = 'blue')


####Hospitalizations By Race
df_old <- read.csv('data/HospitalizationsByRace.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/poevQ/17/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, ',Hospitalizations,') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Race,Hospitalizations,County Population", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")

step_3 <- append(list(c("Race, Hospitalizations, County Population")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`County Population` <- gsub("\\", "", df$`County Population`,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/HospitalizationsByRace.csv", row.names = TRUE)

####Hospitalizations By Gender
df_old <- read.csv('data/HospitalizationsByGender.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/tCh3s/6/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Gender,Onondaga County') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Gender,Onondaga County", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Gender, Onondaga County")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`Onondaga County` <- gsub("\\", "", df$`Onondaga County`,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/HospitalizationsByGender.csv", row.names = FALSE)

#Community Deaths by Race
df_old <- read.csv("data/Onondaga_County_DeathsbyRace.csv")
data_list <- read_html('https://datawrapper.dwcdn.net/SEXD8/18/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Race ,Deaths,County Population') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Race ,Deaths,County Population", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Race ,Deaths,County Population,delete")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$delete <- NULL

df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))
write.csv(df_tosave, "data/Onondaga_County_DeathsbyRace.csv", row.names = FALSE)


#Cases by Age
df_old <- read.csv("data/Onondaga_County_CasesByAge.csv")
data_list <- read_html('https://datawrapper.dwcdn.net/hIlQl/17/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Age Range,Onondaga County') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Age Range,Onondaga County", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Age Range,Onondaga County")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`Onondaga County` <- gsub("\\", "", df$`Onondaga County`,  fixed = TRUE)

df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))
write.csv(df_tosave, "data/Onondaga_County_CasesByAge.csv", row.names = FALSE)


####Deaths By Gender
df_old <- read.csv('data/DeathsByGender.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/DRqpE/8/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Gender ,Onondaga County') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Gender ,Onondaga County", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Gender, Onondaga County")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`Onondaga County` <- gsub("\\", "", df$`Onondaga County`,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/DeathsByGender.csv", row.names = FALSE)

#Cases by Age Under 20
df_old <- read.csv("data/Onondaga_County_CasesByAgeUnder20.csv")
data_list <- read_html('https://datawrapper.dwcdn.net/OjSMZ/16/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Age Group,Percent of Positive Cases,County Population') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Age Group,Percent of Positive Cases,County Population", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Age Group,Percent of Positive Cases,County Population")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`County Population` <- gsub("\\", "", df$`County Population`,  fixed = TRUE)

df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))
write.csv(df_tosave, "data/Onondaga_County_CasesByAgeUnder20.csv", row.names = FALSE)


#Cases by Race - County

df_old <- read.csv('data/CasesbyRace.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/ECFlZ/14/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Race,Onondaga County') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Race,Onondaga County", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Race,Onondaga County")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`Onondaga County` <- gsub("\\", "", df$`Onondaga County`,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/CasesbyRace.csv", row.names = FALSE)



#Cases by Ethnicity - County

df_old <- read.csv('data/CasesbyEthnicity.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/0LBtV/15/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Ethnicity,Onondaga County') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Ethnicity,Onondaga County", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Ethnicity,Onondaga County")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`Onondaga County` <- gsub("\\", "", df$`Onondaga County`,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/CasesbyEthnicity.csv", row.names = FALSE)



#Cases by Race - Syracuse

df_old <- read.csv('data/CasesbyRace_Syracuse.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/8ghm2/17/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Race,Syracuse') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Race,Syracuse", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Race,Syracuse")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$Syracuse <- gsub("\\", "", df$Syracuse,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/CasesbyRace_Syracuse.csv", row.names = FALSE)



#Cases by Age Under 20 - Syracuse
df_old <- read.csv("data/Syracuse_CasesByAgeUnder20.csv")
data_list <- read_html('https://datawrapper.dwcdn.net/MLDXA/18/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Age Group,Percent of Positive Cases,City  Population') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Age Group,Percent of Positive Cases,City  Population", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Age Group,Percent of Positive Cases,City Population")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$`City Population` <- gsub("\\", "", df$`City Population`,  fixed = TRUE)

df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))
write.csv(df_tosave, "data/Syracuse_CasesByAgeUnder20.csv", row.names = FALSE)

#Cases by Gender - County

df_old <- read.csv('data/CasesbyGender_County.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/IP9Gs/15/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Gender,Onondaga County') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Gender,Onondaga County", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Gender,Onondaga County")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$Syracuse <- gsub("\\", "", df$Syracuse,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/CasesbyGender_County.csv", row.names = FALSE)




#Cases by Ethnicity - Syracuse

df_old <- read.csv('data/CasesbyEthnicity_Syracuse.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/YWEZJ/14/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Ethnicity,Syracuse') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Ethnicity,Syracuse", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Ethnicity,Syracuse")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$Syracuse <- gsub("\\", "", df$Syracuse,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/CasesbyEthnicity_Syracuse.csv", row.names = FALSE)


#Cases by Age - Syracuse
df_old <- read.csv("data/CasesByAge_Syracuse.csv")
data_list <- read_html('https://datawrapper.dwcdn.net/mDbeO/17/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Age Range,Syracuse') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Age Range,Syracuse", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Age Range,Syracuse")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$Syracuse <- gsub("\\", "", df$Syracuse,  fixed = TRUE)

df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))
write.csv(df_tosave, "data/CasesByAge_Syracuse.csv", row.names = FALSE)


#Cases by Gender - Syracuse

df_old <- read.csv('data/CasesbyGender_Syracuse.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/CeIA5/14/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Gender,Syracuse') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Gender,Syracuse", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Gender,Syracuse")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$Syracuse <- gsub("\\", "", df$Syracuse,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))

write.csv(df_tosave, "data/CasesbyGender_Syracuse.csv", row.names = FALSE)


#COVID-19 Cases by Onset Date

data_list <- read_html('https://datawrapper.dwcdn.net/hO47c/21/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'MMWR Week,Cases') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("MMWR Week,Cases", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("MMWR Week,Cases")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$Cases <- gsub("\\", "", df$Cases,  fixed = TRUE)


write.csv(df, "data/CasesbyOnsetDate.csv", row.names = FALSE)


#COVID-19 Cases by Clinical Presentation


data_list <- read_html('https://datawrapper.dwcdn.net/Z3GOT/20/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'MMWR Week,Symptomatic ,Asymptomatic ,Unknown') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("MMWR Week,Symptomatic ,Asymptomatic ,Unknown", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("MMWR Week,Symptomatic ,Asymptomatic ,Unknown")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))
df$Unknown <- gsub("\\", "", df$Unknown,  fixed = TRUE)
df <- data.frame(df) %>%
  gather(Type, Count, -MMWR.Week)

write.csv(df, "data/CasesbyClinicalPresentation.csv", row.names = FALSE)


#Hospitalizations By Age
df_old <- read.csv("data/HospitalizationsByAge_County.csv")
data_list <- read_html('https://datawrapper.dwcdn.net/XF4oy/11/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'Age,Hospitalizations') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("Age,Hospitalizations", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("Age,Hospitalizations")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))

df$Hospitalizations <- gsub("\\", "", df$Hospitalizations,  fixed = TRUE)

df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))
write.csv(df_tosave, "data/HospitalizationsByAge_County.csv", row.names = FALSE)


#Likely Exposure Source


data_list <- read_html('http://datawrapper.dwcdn.net/YOOXi/18/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'visJSON')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'MMWR Week,Community,Senior Facility,Travel,Unknown') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("MMWR Week,Community,Senior Facility,Travel,Unknown", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("MMWR Week,Community,Senior Facility,Travel,Unknown")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))
df$Unknown <- gsub("\\", "", df$Unknown,  fixed = TRUE)
df <- data.frame(df) %>%
  gather(Type, Count, -MMWR.Week)

write.csv(df, "data/LikelyExposureSource.csv", row.names = FALSE)



#COVID-19 Testing: Tests Reported


data_list <- read_html('https://datawrapper.dwcdn.net/C77SR/39/') %>%  #Need to check to see how often url changes
  html_node(xpath=".//script[contains(., 'tables')]") %>% # find the javascript section with the data
  html_text() %>% # get that section
  stri_split_lines() %>% # split into lines so we can target the actual data element
  unlist() %>% 
  keep(stri_detect_fixed, 'MMWR Week,Community: Unknown Source,Community: Known Source,Household Member,Travel,Senior Facility,Other Congregate Setting,Unknown') %>% # just get the data line
  stri_trim_both() %>% # prep it for extraction
  stri_replace_all_fixed("\\n", "\n") %>% # make lines lines
  stri_split_lines() %>% 
  unlist() 


step_1 <- data_list[grep("MMWR Week,Community: Unknown Source,Community: Known Source,Household Member,Travel,Senior Facility,Other Congregate Setting,Unknown", data_list)+1:length(data_list)]
step_2 <- step_1[1:grep("isPreview", step_1)-1] %>% stri_split_fixed("\\t")
step_3 <- append(list(c("MMWR Week,Community: Unknown Source,Community: Known Source,Household Member,Travel,Senior Facility,Other Congregate Setting,Unknown")), step_2)

df <- data.table::fread(paste(step_3, collapse = "\n"))
df$Unknown <- gsub("\\", "", df$Unknown,  fixed = TRUE)
df <- data.frame(df) %>%
  gather(Type, Count, -MMWR.Week)

write.csv(df, "data/LikelyExposureSource.csv", row.names = FALSE)

