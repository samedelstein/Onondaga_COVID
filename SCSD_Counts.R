library(jsonlite)
SCSD_Counts_old <- read.table("data/SCSD_School_Summaries.txt")

SCSD_Counts <- read_json("https://schoolcovidreportcard.health.ny.gov/data/public/district.421800.json", simplifyDataFrame = TRUE)
SCSD_SchoolSummaries <- data.frame(SCSD_Counts$schoolSummaries)

write.table(data.frame(subset(SCSD_SchoolSummaries,select=-c(positiveCounts)),unclass(SCSD_SchoolSummaries$positiveCounts)),"data/SCSD_School_Summaries_new.txt")
SCSD_Counts_new <- read.table("data/SCSD_School_Summaries_new.txt")
duprows <- rownames(SCSD_Counts_old) %in% rownames(SCSD_Counts_new)
SCSD_Counts_tosave <- data.frame(rbind(SCSD_Counts_new, SCSD_Counts_old[!duprows,]))
write.table(SCSD_Counts_tosave, "data/SCSD_School_Summaries.txt")

