# IJC437/IJC445 Introduction to R (Part 4)

# 1. SETUP AND DATA DOWNLOAD 

library(tidyverse)

exped_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv"
exped     <- read_csv(exped_url)

peak_url  <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv"
peaks     <- read.csv(peak_url)

# 2. INITIAL DATA INSPECTION 

head(exped)

glimpse(exped)  

cat(nrow(exped), "records of expeditions\n")
cat(nrow(peaks), "records of peaks\n")

# 3. BASIC YEAR-LEVEL ANALYSIS 

range(exped$YEAR, na.rm = TRUE)

exped %>%
  count(YEAR)

expeditions_per_year <- table(exped$YEAR)


plot(
  names(expeditions_per_year),
  expeditions_per_year,
  type = "l",
  main = "Number of Expeditions by Year",
  xlab = "Year",
  ylab = "Number of Expeditions"
)

# 4. SEASONAL DISTRIBUTION OF EXPEDITIONS

seasons_dist <- table(exped$SEASON_FACTOR)

barplot(
  seasons_dist,
  xlab = "Season",
  ylab = "Number of Expeditions",
  main = "Number of Expeditions by Season"
)

# 5. SEASON × YEAR (GROUPED BARPLOT)

year_season_table <- table(exped$SEASON_FACTOR, exped$YEAR)

barplot(
  year_season_table,
  beside = TRUE,
  col = c("blue", "green", "yellow", "orange"),
  main = "Expeditions per Year and Season",
  xlab = "Year",
  ylab = "Number of Expeditions",
  legend.text = TRUE,
  args.legend = list(title = "Season", x = "topleft")
)

par(mar = c(5, 4, 4, 8))

barplot(
  year_season_table,
  beside = TRUE,
  col = c("blue", "green", "yellow", "orange"),
  main = "Expeditions per Year and Season",
  xlab = "Year",
  ylab = "Number of Expeditions",
  las = 2
)

legend(
  x     = par("usr")[2] + 0.5,
  y     = max(year_season_table),
  legend = row.names(year_season_table),
  fill   = c("blue", "green", "yellow", "orange"),
  title  = "Season",
  xpd    = TRUE,
  bty    = "n"
)

# 6. MOST CLIMBED PEAKS

exped %>%
  count(PEAKID)

peak_counts    <- table(exped$PEAKID)
peak_counts_df <- as.data.frame(peak_counts)
names(peak_counts_df) <- c("PEAKID", "expedition_count")

peak_counts_df <- peak_counts_df[order(-peak_counts_df$expedition_count), ]

peak_joined <- merge(
  peak_counts_df,
  peaks,
  by    = "PEAKID",
  all.x = TRUE
)

peak_joined <- peak_joined[order(-peak_joined$expedition_count), ]

top20peaks <- head(peak_joined, 20)

barplot(
  top20peaks$expedition_count,
  names.arg = top20peaks$PKNAME,
  las  = 2,
  col  = "lightblue",
  main = "Top 20 Most Climbed Peaks",
  ylab = "Number of Expeditions"
)

# 7. MARK SUCCESSFUL EXPEDITIONS

exped$SUCCESS <- grepl(
  "Success",
  exped$TERMREASON_FACTOR,
  ignore.case = TRUE
)

# 8. SUCCESS RATE OVER YEARS

success_years <- sort(unique(exped$YEAR))
success_rate  <- numeric(length(success_years))

for (i in 1:length(success_years)) {
  data_year        <- exped[exped$YEAR == success_years[i], ]
  success_rate[i]  <- mean(data_year$SUCCESS, na.rm = TRUE)
}

plot(
  success_years,
  success_rate,
  type = "l",
  col  = "darkgreen",
  main = "Expedition Success Rate Over Years",
  xlab = "Year",
  ylab = "Success Rate"
)

# 9. SUCCESS BY PEAK 

# Total expeditions per peak
total_by_peak <- table(exped$PEAKID)

success_by_peak <- table(exped$PEAKID[exped$SUCCESS == TRUE])

total_peak_df <- as.data.frame(total_by_peak)
names(total_peak_df) <- c("PEAKID", "total_expeditions")

success_peak_df <- as.data.frame(success_by_peak)
names(success_peak_df) <- c("PEAKID", "successful_expeditions")

# Merge totals
peak_summary <- merge(
  total_peak_df,
  success_peak_df,
  by    = "PEAKID",
  all.x = TRUE
)

# Replace NA (no successful climbs) with 0
peak_summary$successful_expeditions[
  is.na(peak_summary$successful_expeditions)
] <- 0

# Compute success rate per peak
peak_summary$success_rate <-
  peak_summary$successful_expeditions / peak_summary$total_expeditions

# Join with peak names
peak_summary <- merge(
  peak_summary,
  peaks[, c("PEAKID", "PKNAME")],
  by    = "PEAKID",
  all.x = TRUE
)

peak_summary <- peak_summary[order(-peak_summary$success_rate), ]

par(mar = c(10, 5, 4, 2))  

barplot(
  peak_summary$success_rate,
  names.arg  = peak_summary$PKNAME,
  las        = 2,
  cex.names  = 0.4,
  col        = "lightgreen",
  main       = "Top 10 Peaks by Expedition Success Rate",
  ylab       = "Success Rate",
  ylim       = c(0, 1)
)
