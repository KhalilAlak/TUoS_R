library(MASS)
data("women")
women
summary(women)
hist(women$height)
hist(x=women$height, breaks=4, main="Histogram showing women's heights",
     xlab="Height")
par(mfrow=c(1,2))
hist(x=women$height, breaks=4, main="Heights", xlab="Height")
hist(x=women$weight, breaks=2, main="Weights", xlab="Weight")
par(mfrow=c(1,1))
plot(women)
installed.packages()
View(install.packages())
library(tidyverse)
data()
library(tidyverse)
testFile<-read_tsv("test.tsv")
testFile
library(tidyverse)
testFile<-read_tsv("test.tsv", col_names=FALSE)
testFile
testCSVFile<-read_csv("freeschoolmeals.csv")
head(testCSVFile)
testCSVFile<-read_csv("freeschoolmeals.csv", col_types="cciici")
head(testCSVFile)
library(readxl)
excelFile<-file.path("indicator hiv estimated prevalence% 15-49.xlsx")
testExcelFile<-read_excel(excelFile, sheet="Data")
head(testExcelFile)
#Visualization
install.packages("rvest")
library(rvest)
url <- "https://en.wikipedia.org/wiki/Sheffield"
wikiPage <- read_html(url)
wikiPage
h2Sections <- wikiPage %>%
  html_nodes("h2")
h2Sections
h2Sections[1]
h2Sections[2]
h2Sections[1:2]
h2Sections %>%
html_text()
pageText <- wikiPage %>%
  html_nodes("p") %>%
  html_text()
pageText[1]
pageText[2]
install.packages("jsonlite")
library(jsonlite)
json <-
  '[
{"Name" : "Mario", "Age" : 32, "Occupation" : "Plumber"}, 
{"Name" : "Peach", "Age" : 21, "Occupation" : "Princess"},
{},
{"Name" : "Bowser", "Occupation" : "Koopa"}
]'
mydf <- fromJSON(json)
mydf
myjson <- toJSON(mydf)
myjson
citibike <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")
str(citibike$data)
View(citibike$data$stations)
stations <- citibike$data$stations$name
stations
library(jsonlite)
library(dplyr)
library(ggplot2)

station_info_url <- "https://gbfs.citibikenyc.com/gbfs/en/station_information.json"
station_info_json <- fromJSON(station_info_url)
station_info <- station_info_json$data$stations

station_status_url <- "https://gbfs.citibikenyc.com/gbfs/en/station_status.json"
station_status_json <- fromJSON(station_status_url)
station_status <- station_status_json$data$stations

bike_data <- left_join(station_info, station_status, by = "station_id")

ggplot(bike_data, aes(x = num_bikes_available)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Available Bikes per Station",
       x = "Number of Available Bikes",
       y = "Number of Stations") +
  theme_minimal()


library(tidyr)

dock_data <- bike_data %>%
  select(station_id, name, num_docks_available, capacity) %>%
  pivot_longer(cols = c(num_docks_available, capacity), 
               names_to = "dock_type", 
               values_to = "count")

ggplot(dock_data, aes(x = count, fill = dock_type)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  scale_fill_manual(values = c("num_docks_available" = "darkgreen", "capacity" = "orange"),
                    labels = c("Available Docks", "Total Docks")) +
  labs(title = "Comparison of Available Docks vs Total Docks per Station",
       x = "Number of Docks",
       y = "Number of Stations",
       fill = "") +
  theme_minimal()

install.packages("rvest")
library(rvest)
url <- "https://en.wikipedia.org/wiki/Sheffield"
wikiPage <- read_html(url)
wikiPage
h2Sections <- wikiPage %>%
  html_nodes("h2")
h2Sections  
h2Sections[1]
h2Sections[2]
h2Sections[1:2]
h2Sections %>%
  html_text()
pageText <- wikiPage %>%
  html_nodes("p") %>%
  html_text()
pageText[1]
pageText[2]
install.packages("jsonlite")
library(jsonlite)
json <-
  '[
{"Name" : "Mario", "Age" : 32, "Occupation" : "Plumber"}, 
{"Name" : "Peach", "Age" : 21, "Occupation" : "Princess"},
{},
{"Name" : "Bowser", "Occupation" : "Koopa"}
]'

mydf <- fromJSON(json)
mydf
myjson <- toJSON(mydf)
myjson
citibike <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")
str(citibike$data)
View(citibike$data$stations)
stations <- citibike$data$stations$name
stations

library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

stations <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")$data$stations
status <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_status.json")$data$stations

bike_data <- left_join(stations, status, by = "station_id")

dock_data <- bike_data %>%
  select(name, num_docks_available, capacity) %>%
  pivot_longer(cols = c(num_docks_available, capacity),
               names_to = "dock_type",
               values_to = "count")

ggplot(dock_data, aes(x = count, fill = dock_type)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  scale_fill_manual(values = c("num_docks_available" = "darkgreen", "capacity" = "orange"),
                    labels = c("Available Docks", "Total Docks")) +
  labs(title = "Available Docks vs Total Docks per Station",
       x = "Number of Docks",
       y = "Number of Stations",
       fill = "") +
  theme_minimal()
