library(jsonlite)
options(scipen=999)
####School Counts
SCSD_Counts_old <- read.table("data/SCSD_School_Summaries.txt")

SCSD_Counts <- read_json("https://schoolcovidreportcard.health.ny.gov/data/public/district.421800.json", simplifyDataFrame = TRUE)
SCSD_SchoolSummaries <- data.frame(SCSD_Counts$schoolSummaries)

write.table(data.frame(subset(SCSD_SchoolSummaries,select=-c(positiveCounts)),unclass(SCSD_SchoolSummaries$positiveCounts)),"data/SCSD_School_Summaries_new.txt")
SCSD_Counts_new <- read.table("data/SCSD_School_Summaries_new.txt")
duprows <- SCSD_Counts_old$updateDate %in% SCSD_Counts_new$updateDate
SCSD_Counts_tosave <- data.frame(rbind(SCSD_Counts_new, SCSD_Counts_old[!duprows,]))
write.table(SCSD_Counts_tosave, "data/SCSD_School_Summaries.txt")

x <- SCSD_Counts_tosave %>%
  mutate(Date = as.Date(updateDate, '%b %d, %Y %H:%M:%S')) %>%
  group_by(Date) %>%
  summarise(sumonSitePositiveStudents = sum(onSitePositiveStudents),
            onSitePositiveTeachers = sum(onSitePositiveTeachers),
            onSitePositiveStaff = sum(onSitePositiveStaff),
            onSitePositiveTotal = sum(onSitePositiveTotal),
            offSitePositiveStudents = sum(offSitePositiveStudents),
            offSitePositiveTeachers = sum(offSitePositiveTeachers),
            offSitePositiveStaff = sum(offSitePositiveStaff),
            offSitePositiveTotal = sum(offSitePositiveTotal)) %>%
  mutate(new_cases = sumonSitePositiveStudents - lag(sumonSitePositiveStudents,1))
  ggplot(aes(Date,onSitePositiveStudents)) +
  geom_col()

#Current Counts
SCSD_Current_Counts_old <- read.csv("data/SCSD_CurrentCounts.csv")
SCSD_CurrentCounts <- data.frame(unlist(SCSD_Counts$currentCounts)) %>%
  mutate(Type = rownames(data.frame(unlist(SCSD_Counts$currentCounts))),
         Date = SCSD_Counts$updateDate) %>%
  rename(counts = unlist.SCSD_Counts.currentCounts.)
duprows <- rbind(SCSD_CurrentCounts,SCSD_Current_Counts_old)

SCSD_CurrentCounts_tosave = duprows[!duplicated(paste0(duprows$Date, duprows$Type)),]

write.csv(SCSD_Current_Counts_tosave,"data/SCSD_CurrentCounts.csv", row.names = FALSE)





SCSD_Counts$allTimeCounts
#Today Counts
SCSD_Today_Counts_old <- read.csv("data/SCSD_TodayCounts.csv")
SCSD_TodayCounts <- data.frame(unlist(SCSD_Counts$todayCounts)) %>%
  mutate(Type = rownames(data.frame(unlist(SCSD_Counts$todayCounts))),
         Date = SCSD_Counts$updateDate) %>%
  rename(counts = unlist.SCSD_Counts.todayCounts.)
duprows <- rbind(SCSD_TodayCounts,SCSD_Today_Counts_old)

SCSD_TodayCounts_tosave = duprows[!duplicated(paste0(duprows$Date, duprows$Type)),]

write.csv(SCSD_Today_Counts_tosave,"data/SCSD_TodayCounts.csv", row.names = FALSE)


#All Time Counts
SCSD_alltime_Counts_old <- read.csv("data/SCSD_alltimeCounts.csv")
SCSD_alltimeCounts <- data.frame(unlist(SCSD_Counts$allTimeCounts)) %>%
  mutate(Type = rownames(data.frame(unlist(SCSD_Counts$allTimeCounts))),
         Date = SCSD_Counts$updateDate) %>%
  rename(counts = unlist.SCSD_Counts.allTimeCounts.)
duprows <- rbind(SCSD_alltime_Counts_old,SCSD_alltimeCounts)

SCSD_alltimeCounts_tosave = duprows[!duplicated(paste0(duprows$Date, duprows$Type)),]

write.csv(SCSD_alltime_Counts_tosave,"data/SCSD_alltimeCounts.csv", row.names = FALSE)


#Past Week Counts
SCSD_pastWeekCounts_old <- read.csv("data/SCSD_pastWeekCounts.csv")
SCSD_pastWeekCounts <- data.frame(unlist(SCSD_Counts$pastWeekCounts)) %>%
  mutate(Type = rownames(data.frame(unlist(SCSD_Counts$pastWeekCounts))),
         Date = SCSD_Counts$updateDate) %>%
  rename(counts = unlist.SCSD_Counts.pastWeekCounts.)
duprows <- rbind(SCSD_pastWeekCounts,SCSD_pastWeekCounts_old)

SCSD_pastWeekCounts_tosave = duprows[!duplicated(paste0(duprows$Date, duprows$Type)),]

write.csv(SCSD_pastWeekCounts_tosave,"data/SCSD_pastWeekCounts.csv", row.names = FALSE)



#Past Two Weeks Counts
SCSD_pastTwoWeeksCounts_old <- read.csv("data/SCSD_pastTwoWeeksCounts.csv")
SCSD_pastTwoWeeksCounts <- data.frame(unlist(SCSD_Counts$pastTwoWeeksCounts)) %>%
  mutate(Type = rownames(data.frame(unlist(SCSD_Counts$pastTwoWeeksCounts))),
         Date = SCSD_Counts$updateDate) %>%
  rename(counts = unlist.SCSD_Counts.pastTwoWeeksCounts.)
duprows <- rbind(SCSD_pastTwoWeeksCounts_old,SCSD_pastTwoWeeksCounts)

SCSD_pastTwoWeeksCounts_tosave = duprows[!duplicated(paste0(duprows$Date, duprows$Type)),]

write.csv(SCSD_pastTwoWeeksCounts_tosave,"data/SCSD_pastTwoWeeksCounts.csv", row.names = FALSE)


#Last 7 days counts
SCSD_lastSevenDaysCounts_old <- read.csv("data/SCSD_lastSevenDaysCounts.csv")
SCSD_lastSevenDaysCounts <- data.frame(unlist(SCSD_Counts$lastSevenDaysCounts)) %>%
  mutate(Type = rownames(data.frame(unlist(SCSD_Counts$lastSevenDaysCounts))),
         Date = SCSD_Counts$updateDate) %>%
  rename(counts = unlist.SCSD_Counts.lastSevenDaysCounts.)
duprows <- rbind(SCSD_lastSevenDaysCounts_old,SCSD_lastSevenDaysCounts)

SCSD_lastSevenDaysCounts_tosave = duprows[!duplicated(paste0(duprows$Date, duprows$Type)),]

write.csv(SCSD_lastSevenDaysCounts_tosave,"data/SCSD_lastSevenDaysCounts.csv", row.names = FALSE)

#Last14Days counts
SCSD_lastFourteenDaysCounts_old <- read.csv("data/SCSD_lastFourteenDaysCounts.csv")
SCSD_lastFourteenDaysCounts <- data.frame(unlist(SCSD_Counts$lastFourteenDaysCounts)) %>%
  mutate(Type = rownames(data.frame(unlist(SCSD_Counts$lastFourteenDaysCounts))),
         Date = SCSD_Counts$updateDate) %>%
  rename(counts = unlist.SCSD_Counts.lastFourteenDaysCounts.)
duprows <- rbind(SCSD_lastFourteenDaysCounts_old,SCSD_lastFourteenDaysCounts)

SCSD_lastFourteenDaysCounts_tosave = duprows[!duplicated(paste0(duprows$Date, duprows$Type)),]

write.csv(SCSD_lastFourteenDaysCounts_tosave,"data/SCSD_lastFourteenDaysCounts.csv", row.names = FALSE)


#School Administered Test History
SCSD_schoolAdministeredTestHistory_old <- read.csv("data/SCSD_schoolAdministeredTestHistory.csv")

schoolAdministeredTestHistory <- SCSD_Counts$schoolAdministeredTestHistory
duprows <- rbind(SCSD_schoolAdministeredTestHistory_old,schoolAdministeredTestHistory)
schoolAdministeredTestHistory_tosave = duprows[!duplicated(duprows$date),]

schoolAdministeredTestHistory_tosave <- apply(schoolAdministeredTestHistory_tosave,2,as.character)
write.csv(schoolAdministeredTestHistory_tosave,"data/SCSD_schoolAdministeredTestHistory.csv", row.names = FALSE)


#History
positiveHistory <- read.csv('data/positiveHistory.csv')
positiveHistoryNew <- SCSD_Counts$positiveHistory
positiveHistoryOldNew <- rbind(positiveHistory, positiveHistoryNew)
positiveHistory_tosave = positiveHistoryOldNew[!duplicated(positiveHistoryOldNew$date),]
write.csv(positiveHistory_tosave, 'data/positiveHistory.csv',row.names = FALSE)

positiveHistory_tosave %>%
  mutate(Date = as.Date(date, '%b %d, %Y %H:%M:%S')) %>%
  arrange(Date)
