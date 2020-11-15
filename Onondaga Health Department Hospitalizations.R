library(stringi)
library(rvest)
library(hrbrthemes) # git[la|hu]b / hrbrmstr / hrbrthemes
library(tidyverse)
library(data.table)

data_list <- read_html('https://datawrapper.dwcdn.net/I4IZD/107/') %>%  #Need to check to see how often url changes
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

write.csv(df, "data/Onondaga_County_Hospitalizations.csv", row.names = FALSE)

ggplot(df, aes(Date, `Total Hospitalized`)) +
  geom_point()


merge(df, county_case_mapping_df_new, by.x = 'Date', by.y = 'DATE') %>%
  ggplot() +
  geom_col(aes(Date, `Total Hospitalized`), alpha = .5, color = 'red', fill = 'red') +
  geom_col(aes(Date, new_cases), alpha = .5, color = 'blue', fill = 'blue')






####Hospitalizations By Race
df_old <- read.csv('HospitalizationsByRace.csv')
data_list <- read_html('https://datawrapper.dwcdn.net/poevQ/16/') %>%  #Need to check to see how often url changes
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
df <- data.table::fread(paste(step_2, collapse = "\n"))
names(df) <- c("Race","Hospitalizations","County Population")
df$`County Population` <- gsub("\\", "", df$`County Population`,  fixed = TRUE)
df <- df %>%
  mutate(Date = Sys.Date())
duprows <- rownames(df_old) %in% rownames(df)
df_tosave <- data.frame(rbind(df, df_old[!duprows,]))


write.csv(df_tosave, "data/HospitalizationsByRace.csv", row.names = TRUE)
