install.packages("tidyverse")
library(tidyverse)
t<-seq(from=0, to=10, by=0.1)
y<-sin(t)
df <- data.frame(t,y)
ggplot(df, aes(x = t, y = y)) + geom_point()
ggplot(df) + geom_point(aes(x = t, y = y))
library(WDI)
new_wdi_cache <- WDIcache()
gdp_capita <- WDI(country="all",
                  indicator = "NY.GDP.PCAP.KD",
                  start = 2000,
                  end = 2020,
                  cache = new_wdi_cache)
View(gdp_capita)
eu_countries <- subset(gdp_capita,
                       country=="United Kingdom" |
                         country=="France" |
                         country=="Spain" |
                         country =="Italy" |
                         country =="Netherlands")
View(eu_countries)
eu_countries <- WDI(country=c("GB","FR","ES","IT","NL"),
                    indicator = "NY.GDP.PCAP.KD",
                    start = 2000, end = 2020,
                    cache = new_wdi_cache)
View(eu_countries)
country_data <- WDI(country=c("GB","FR","ES","IT","NL", "CN","AE","IN","JO","US"),
                    indicator = c("NY.GDP.PCAP.KD","NY.GDP.PCAP.KD.ZG","SP.POP.TOTL","SP.DYN.LE00.IN"),
                    start = 2000, end = 2020,
                    extra=TRUE,
                    cache = new_wdi_cache)
View(country_data)
ggplot(country_data) + geom_point(aes(year, NY.GDP.PCAP.KD))

ggplot(country_data, aes(year, NY.GDP.PCAP.KD, colour = country)) +
  geom_point()

ggplot(country_data,aes(year,NY.GDP.PCAP.KD,colour=country,size=NY.GDP.PCAP.KD.ZG)) +
  geom_point()

ggplot(country_data, aes(year,
                         NY.GDP.PCAP.KD,colour = country,
                         size=NY.GDP.PCAP.KD.ZG,
                         shape = region)) +
  geom_point()

ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(colour = "blue", size = 5, shape = 4)

ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG, colour = country)) +
  geom_point() +
  labs(x="Year", y="GDP Per Capita Growth (annual %)", colour="Country" )

ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG, colour = country)) +
  geom_point() +
  labs(x="Year", y="GDP Per Capita Growth (annual %)", colour="Country" )+
  xlim(2000,2016)+
  ylim(-10,10)

ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG, colour = country)) +
  geom_line() +
  labs(x="Year", y="GDP Per Capita Growth (annual %)", colour="Country" )

#Final Task

unique(country_data$income)

library(WDI)

new_wdi_cache <- WDIcache()

life_data <- WDI(
  country = c("US", "NL", "CN", "JO", "IN", "EG", "ET", "UG"),
  indicator = c("SP.DYN.LE00.IN"),
  start = 2000,
  end = 2020,
  extra = TRUE,
  cache = new_wdi_cache
)
view(life_data)

life_data <- na.omit(life_data)

library(ggplot2)

ggplot(life_data, aes(x = year, y = SP.DYN.LE00.IN, colour = country, shape = income)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Life Expectancy at Birth (years)",
    colour = "Country",
    shape = "Income Level",
    title = "Life Expectancy (2000–2020) by Country and Income Group"
  ) +
  theme_minimal()
