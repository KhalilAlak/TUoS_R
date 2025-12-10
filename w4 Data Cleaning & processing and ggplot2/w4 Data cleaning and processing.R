library(tidyverse)  # load library
str(mpg)
filter(mpg, manufacturer=="audi")
filter(mpg, displ > 2)
filter(mpg, displ > 2 & cyl > 6)  # how to turn it into a pipe-dplyr way?

# Multiple conditions
filter(mpg, manufacturer=="audi", year==1999)
filter(mpg, manufacturer=="audi" & year==1999)
filter(mpg, (manufacturer=="audi" | manufacturer=="chevrolet"), year==1999)

#task 1
library(dplyr)

mpg_filtered_1 <- mpg %>%
  filter(manufacturer == "audi" | year == 1999)

# View the first few rows
head(mpg_filtered_1)

mpg_filtered_2 <- mpg %>%
  filter(year == 1999 & manufacturer != "audi")

# View the first few rows
head(mpg_filtered_2)

#task 1 ends here-------------------------------

filter(mpg, (manufacturer %in% c("audi","chevrolet")), year==1999)

# pipe it to count()
filter(mpg, (manufacturer %in% c("audi","chevrolet")), year==1999) %>%
  count(manufacturer)

# filtering for sampling
sample_frac(mpg, 0.05, replace=TRUE) # sample 5% of the data
sample_n(mpg, 10, replace=TRUE) # sample of 10 rows

rio2016Medals <- read_csv("Rio2016.csv")
arrange(rio2016Medals, Country)
arrange(rio2016Medals, desc(Country))
arrange(rio2016Medals, desc(Gold), desc(Silver), desc(Bronze))

#task 2
rio2016Medals %>%
  arrange(desc(Gold), desc(Silver), desc(Bronze)) %>%
  View()
#task 2--------------------------------

select(mpg, manufacturer, hwy)
select(mpg, starts_with("d")) # Selecting with pattern

# Combine select + filter + arrange with pipes
mpg %>% 
  select(manufacturer, hwy) %>% 
  filter(manufacturer != "chevrolet" & hwy >=20) %>%
  arrange(desc(manufacturer))

mutate(rio2016Medals, Total = Gold + Silver + Bronze)

summarise(mpg, avg = mean(hwy))

# grouped-based summary
mpg |> 
  group_by(year, manufacturer) |> 
  summarise(count = n())

#task 3
library(dplyr)

mpg %>%
  group_by(manufacturer) %>%
  summarise(unique_models = n_distinct(model)) %>%
  arrange(desc(unique_models))

mpg %>%
  mutate(HwyCtyRatio = hwy / cty) %>%
  select(manufacturer, model, cty, hwy, HwyCtyRatio) %>%
  head(10)
#task 3 ends here------------------------------------------

install.packages("nycflights13")
library(nycflights13)
nycflights13::airlines
nycflights13::airports
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

airlines

flights2 %>% 
  left_join(airlines, by = "carrier")
flights2

meals <- read_csv("freeschoolmeals.csv")
head(meals)

# check summary
summary(meals$FSMTaken)
# significant difference between median & mean --> mean affected by outliers --> potential problem.
# 17 NAs

mean(meals$FSMTaken)  # NA
mean(meals$FSMTaken, na.rm=TRUE) # NA removed, but mean is still suspiciously high.

meals |>
  filter(FSMTaken != 999) |>
  mean(na.rm = TRUE)

filter(meals, (FSMTaken < 9999 | is.na(FSMTaken))) %>% count()
filter(meals, (FSMTaken < 9999 | is.na(FSMTaken))) %>% 
  mutate(newFSMTaken = ifelse(is.na(FSMTaken), floor(mean(FSMTaken, na.rm = TRUE)), FSMTaken))

y <- c(4,5,6,NA)
is.na(y)
y[is.na(y)] <- mean(y, na.rm = TRUE)
y

actualFSMTaken<-meals$FSMTaken[meals$FSMTaken!=9999]

length(actualFSMTaken)

actualFSMTaken[is.na(actualFSMTaken)]<-floor(mean(actualFSMTaken, na.rm=TRUE))

mean(actualFSMTaken, na.rm=TRUE)

filter(meals, FSMTaken<9999)

filter(meals, FSMTaken<9999) %>% count()

filter(meals, (FSMTaken<9999 | is.na(FSMTaken))) %>% count()

y <- c(4,5,6,NA)

is.na(y)
y[is.na(y)] <- mean(y,na.rm=TRUE)
y

actualFSMTaken[is.na(actualFSMTaken)]<-floor(mean(actualFSMTaken, na.rm=TRUE))

filter(meals, (FSMTaken<9999 | is.na(FSMTaken))) %>%
  mutate(newFSMTaken=ifelse(is.na(FSMTaken), floor(mean(FSMTaken, na.rm=TRUE)),
                            FSMTaken))

#task 4
meals <- meals %>%
  mutate(FSMTaken_as_4 = ifelse(FSMTaken == 9999, 4, FSMTaken))


mean(meals$FSMTaken_as_4, na.rm = TRUE)

meals %>%
  select(FSMTaken, FSMTaken_as_4, FSMTaken_as_0) %>%
  View()
