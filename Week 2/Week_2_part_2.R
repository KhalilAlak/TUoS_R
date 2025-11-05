#Analysis of Himalayan Expeditions
#We will look at downloading a public dataset available from Github (rfordatascience)
#We will then conduct some analysis of the expeditions to the Himalayas
#Then we will make some summaries and draw some simple plots


#We will install tidyverse, which is a helpful set of tools for data science 
install.packages('tidyverse')
library(tidyverse)

#We will now read the dataset from the Github link
exped_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv"
exped <- read_csv(exped_url)

peak_url <-"https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv"
peaks <- read.csv(peak_url)


#Let's look at the first few rows
head(exped)

#and see all the column names and types
glimpse(exped) #each column is a variable and each row is a single expedition

#You could also 'view' the data by using 
View(exped)


#total number of expeditions and peaks
cat(nrow(exped), " records of expeditions")
cat(nrow(peaks), " records of peaks")

# See which years are included
range(exped$YEAR, na.rm = TRUE) #na.rm removes missing values during calculations

#We will now use pipes in R, which helps to chain multiple operations together.
#We use the symbol '%>%' for this, which means that the output of the left part of the expression is passed to the right part

# Count how many expeditions happened each year
exped %>%
  count(YEAR)

#Let's also plot this data

expeditions_per_year <- table(exped$YEAR)
View(expeditions_per_year)
plot(names(expeditions_per_year),
     expeditions_per_year,
     type = "l",                    # l = line plot
     main = "Number of Expeditions by Year",
     xlab = "Year",
     ylab = "Number of Expeditions")

#Let's try to plot this based on seasons
#However, it doesn't make sense as a line plot so we will plot this as a barchart

seasons_dist <- table(exped$SEASON_FACTOR)

barplot(seasons_dist,
        xlab="Season", 
        ylab="Number of Expeditions", 
        main="Number of Expeditions by Season", )

#So we can see that the majority of expeditions were conducted during Spring 
#but we don't know if this was a pattern for every year. Let's try to bring the seasons and year together

#adjust our margin for the plot to position the legend correctly
par(mar = c(5, 4, 4, 15))
# Create a table counting expeditions by year and season
year_season_table <- table(exped$SEASON_FACTOR,exped$YEAR)
# Make a grouped barplot with a legend
barplot(year_season_table,
        beside = TRUE,            # beside=TRUE makes grouped bars
        col = c("blue", "green", "yellow", "orange"),
        main = "Expeditions per Year and Season",
        xlab = "Year",
        ylab = "Number of Expeditions",
        legend.text = TRUE,
        args.legend = list(title = "Season", x = "topright"))


#move legend to outside the graph

par(mar = c(5, 4, 4, 8)) #increase the right margin
# Grouped barplot
barplot(year_season_table,
        beside = TRUE,            
        col = c("blue", "green", "yellow", "orange"),
        main = "Expeditions per Year and Season",
        xlab = "Year",
        ylab = "Number of Expeditions",
        las = 2)  # rotate x-axis labels

# Add legend outside the plot (right side)
legend(x = par("usr")[2] + 0.5,  # just past right edge of plot
       y = max(year_season_table),  
       legend = row.names(year_season_table),  # explicitly use season names
       fill = c("blue", "green", "yellow", "orange"),
       title = "Season",
       xpd = TRUE,
       bty = "n")


#Let's try to analyse which peaks have been attempted - for that, we will need to work with the PEAKID in exped 

# Count how many expeditions happened each year
exped %>%
  count(PEAKID)

peak_counts <- table(exped$PEAKID)

# Convert table to data frame to make it easier to join with the peaks dataset
peak_counts_df <- as.data.frame(peak_counts)
names(peak_counts_df) <- c("PEAKID", "expedition_count")

#sort the peak counts in descending order
peak_counts_df <- peak_counts_df[order(-peak_counts_df$expedition_count), ]
#let's try to join this dataframe with the peaks data so that we can know the names of the peaks
# Merge will match peakID from both datasets
peak_joined <- merge(peak_counts_df, peaks, by="PEAKID",all.x = TRUE)

#Now we can create a barplot of top 20 peaks attempted -but first we will sort the dataframe based on expedition count
peak_joined <- peak_joined[order(-peak_joined$expedition_count),]

top20peaks <- head(peak_joined, 20)

barplot(top20peaks$expedition_count,
        names.arg = top20peaks$PKNAME,
        las = 2,
        col = "lightblue",
        main = "Top 20 Most Climbed Peaks",
        ylab = "Number of Expeditions")


#Which expeditions have been successful? for this, we will need to understand where we can find successful expeditions
#There could be two ways of doing that, the first could be to look at matches of the word 'success' in the termination reason factor column
#The second could be to look at the columns success1-4 and if any one of them is TRUE, the expedition indicates success

#we will create a new column in the exped dataframe
exped$SUCCESS <- grepl("Success", exped$TERMREASON_FACTOR, ignore.case = TRUE)

# Calculate success rate per year
success_years <- sort(unique(exped$YEAR))
success_rate <- numeric(length(years))

for(i in 1:length(success_years)) {
  data_year <- exped[exped$YEAR == success_years[i], ]
  success_rate[i] <- mean(data_year$SUCCESS, na.rm = TRUE)
}

#Let's now plot the success rates
# Line plot of success rate
plot(success_years,
     success_rate,
     type = "l",
     col = "darkgreen",
     main = "Expedition Success Rate Over Years",
     xlab = "Year",
     ylab = "Success Rate")

#Let's also study the success per peak name - now that we have the SUCCESS field, we will just use that 

# Total expeditions per peak
total_by_peak <- table(exped$PEAKID)

# Successful expeditions per peak
success_by_peak <- table(exped$PEAKID[exped$SUCCESS == TRUE])

total_peak_df <- as.data.frame(total_by_peak)
names(total_peak_df) <- c("PEAKID", "total_expeditions")

success_peak_df <- as.data.frame(success_by_peak)
names(success_peak_df) <- c("PEAKID", "successful_expeditions")

#merge the totals and successes based on PEAKID

peak_summary <- merge(total_peak_df, success_peak_df, by = "PEAKID", all.x = TRUE)

# Replace NAs (no successful climbs) with 0
peak_summary$successful_expeditions[is.na(peak_summary$successful_expeditions)] <- 0

# Compute success rate
peak_summary$success_rate <- peak_summary$successful_expeditions / peak_summary$total_expeditions
# Join this with the peak names

peak_summary <- merge(peak_summary, peaks[, c("PEAKID", "PKNAME")], by = "PEAKID", all.x = TRUE)
peak_summary <- peak_summary[order(-peak_summary$success_rate), ] #sort based on success rate

#Let's plot this the success rates for all peaks
# Make sure long names fit in the plot area
par(mar = c(10, 5, 4, 2))   # increase bottom margin

barplot(peak_summary$success_rate,
        names.arg = peak_summary$PKNAME,
        las = 2,
        cex.names = 0.4, 
        col = "lightgreen",
        main = "Top 10 Peaks by Expedition Success Rate",
        ylab = "Success Rate",
        ylim = c(0, 1))


#New visualisations
# Count how many expeditions happened each year
exped %>%
  count(NATION)

attempt_counts <- table(exped$NATION)

# Convert table to data frame to make it easier to join with the peaks dataset
attempt_counts_df <- as.data.frame(attempt_counts)
names(attempt_counts_df) <- c("Attempt", "attempt_count")



attempt_counts_df <- attempt_counts_df[order(-attempt_counts_df$attempt_count), ]
top15attempt <- head(attempt_counts_df, 15)

barplot(top15attempt$attempt_count,
        names.arg = top15attempt$PKNAME,
        las = 2,
        col = "yellow",
        main = "Top 15 attended countries",
        ylab = "Number of Attempts",
        xlab = "NATION")
